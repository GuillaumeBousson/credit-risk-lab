# ============================================================================
# 04_challenger_ml.R — Modèle challenger : forêt aléatoire
# Projet : credit-risk-lab
# Démarche champion / challenger :
#   - Champion  = scorecard logit (script 03) : interprétable, auditable,
#     conforme aux attentes réglementaires
#   - Challenger = forêt aléatoire sur les 11 variables brutes (sans
#     discrétisation) : borne haute de performance atteignable
#   La comparaison quantifie le "coût d'interprétabilité" du champion.
# ============================================================================

library(data.table)
library(ranger)
library(pROC)
library(ggplot2)

loans <- readRDS("data/loans.rds")
split <- readRDS("data/split.rds")
scored_test <- readRDS("data/scored_test.rds")   # PD du champion sur le test

accent <- "#2A5A8C"; ink <- "#333333"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", color = ink),
                  plot.subtitle = element_text(color = "#666666")))

# ----------------------------------------------------------------------------
# 1. PRÉPARATION (mêmes variables candidates, même partition qu'au script 03)
# ----------------------------------------------------------------------------
vars_x <- c("fico", "ltv", "dti", "orig_rate", "orig_upb",
            "purpose", "occupancy", "channel", "first_time_buyer",
            "prop_type", "n_borrowers")
dt <- loans[, c("loan_id", "default_90", vars_x), with = FALSE]

# La forêt n'accepte pas les NA : imputation simple et explicite
# (médiane pour le numérique, modalité "MISSING" pour le qualitatif)
for (v in vars_x) {
  if (is.numeric(dt[[v]])) {
    med <- median(dt[[v]], na.rm = TRUE)
    dt[is.na(get(v)), (v) := med]
  } else {
    dt[is.na(get(v)) | get(v) == "", (v) := "MISSING"]
    dt[, (v) := factor(get(v))]
  }
}
dt[, default_90 := factor(default_90, levels = c(0, 1))]

train <- dt[loan_id %in% split$train_ids]
test  <- dt[loan_id %in% split$test_ids]
cat("Train :", nrow(train), "| Test :", nrow(test),
    "(partition identique au script 03)\n")

# ----------------------------------------------------------------------------
# 2. FORÊT ALÉATOIRE
# ----------------------------------------------------------------------------
set.seed(42)
rf <- ranger(
  dependent.variable.name = "default_90",
  data = train[, c("default_90", vars_x), with = FALSE],
  num.trees = 500,
  probability = TRUE,          # sorties = probabilités de défaut
  importance = "impurity",
  seed = 42
)
cat("\nForêt entraînée — erreur OOB (Brier) :", round(rf$prediction.error, 4), "\n")

pd_rf_train <- predict(rf, train)$predictions[, "1"]
pd_rf_test  <- predict(rf, test)$predictions[, "1"]

# ----------------------------------------------------------------------------
# 3. COMPARAISON CHAMPION vs CHALLENGER (échantillon test)
# ----------------------------------------------------------------------------
# Alignement des PD du champion par loan_id (même partition, même ordre garanti
# par la jointure)
comp <- merge(test[, .(loan_id, default_90)],
              scored_test[, .(loan_id, pd_logit = pd)], by = "loan_id")
comp[, pd_rf := pd_rf_test[match(loan_id, test$loan_id)]]
y_test <- as.integer(as.character(comp$default_90))

roc_logit <- roc(y_test, comp$pd_logit, quiet = TRUE)
roc_rf    <- roc(y_test, comp$pd_rf,    quiet = TRUE)
auc_rf_train <- as.numeric(auc(roc(as.integer(as.character(train$default_90)),
                                   pd_rf_train, quiet = TRUE)))

cat("\n--- AUC sur l'échantillon test ---\n")
cat(sprintf("Champion  (logit WoE)      : %.3f (Gini %.3f)\n",
            auc(roc_logit), 2 * auc(roc_logit) - 1))
cat(sprintf("Challenger (forêt, test)   : %.3f (Gini %.3f)\n",
            auc(roc_rf), 2 * auc(roc_rf) - 1))
cat(sprintf("Challenger (forêt, train)  : %.3f — écart train/test à surveiller\n",
            auc_rf_train))

# Test de DeLong : la différence d'AUC est-elle significative ?
dl <- roc.test(roc_logit, roc_rf, method = "delong", paired = TRUE)
cat(sprintf("\nTest de DeLong (H0 : AUC égales) : p-value = %.4f\n", dl$p.value))

# ----------------------------------------------------------------------------
# 4. IMPORTANCE DES VARIABLES (lecture croisée avec l'IV du script 03)
# ----------------------------------------------------------------------------
imp <- data.table(variable = names(rf$variable.importance),
                  importance = rf$variable.importance)
setorder(imp, -importance)
print(imp)

p_imp <- ggplot(imp, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = accent, width = 0.65) +
  coord_flip() +
  labs(title = "Importance des variables — forêt aléatoire (impureté)",
       subtitle = "Lecture croisée avec l'Information Value du champion (fig. 06)",
       x = NULL, y = "Importance (diminution d'impureté de Gini)")
ggsave("outputs/fig_08_importance_rf.png", p_imp,
       width = 8, height = 5, dpi = 150)

# Courbes ROC superposées
roc_dt <- rbindlist(list(
  data.table(modele = "Champion — logit WoE",
             fpr = 1 - roc_logit$specificities, tpr = roc_logit$sensitivities),
  data.table(modele = "Challenger — forêt aléatoire",
             fpr = 1 - roc_rf$specificities, tpr = roc_rf$sensitivities)
))
p_roc <- ggplot(roc_dt, aes(fpr, tpr, color = modele)) +
  geom_line(linewidth = 0.9) +
  geom_abline(linetype = "dotted", color = "#999999") +
  scale_color_manual(values = c("Champion — logit WoE" = accent,
                                "Challenger — forêt aléatoire" = "#C0392B"),
                     name = NULL) +
  labs(title = "Courbes ROC — champion vs challenger (test)",
       x = "Taux de faux positifs", y = "Taux de vrais positifs") +
  theme(legend.position = "top")
ggsave("outputs/fig_09_roc_champion_challenger.png", p_roc,
       width = 7, height = 5.5, dpi = 150)

saveRDS(comp, "data/comp_champion_challenger.rds")

cat("\n========== SYNTHÈSE CHALLENGER ==========\n")
cat(sprintf("Gain d'AUC du challenger : %+.3f point(s)\n",
            as.numeric(auc(roc_rf)) - as.numeric(auc(roc_logit))))
cat("Arbitrage : si le gain est faible, le champion interprétable est\n",
    "préférable en production (auditabilité, stabilité, conformité).\n")
cat("Figures : fig_08 (importance), fig_09 (ROC comparées)\n")
