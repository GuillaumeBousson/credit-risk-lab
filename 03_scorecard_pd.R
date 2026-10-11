# ============================================================================
# 03_scorecard_pd.R — Scorecard de probabilité de défaut (PD)
# Projet : credit-risk-lab
# Démarche (standard bancaire) :
#   1. Sélection des variables candidates à l'octroi (pas de variables
#      post-octroi : le score doit être utilisable au moment de la décision)
#   2. Partition train/test 70/30 (seed fixé, réutilisée par les scripts 04-05)
#   3. Discrétisation WoE (Weight of Evidence) + Information Value (IV)
#   4. Régression logistique sur variables WoE = modèle champion
#   5. Conversion en grille de score (ancrage 600 pts, PDO = 50)
#   6. Sauvegardes : bins, modèle, grille, jeux scorés
# ============================================================================

library(data.table)
library(scorecard)
library(pROC)
library(ggplot2)
library(scales)

loans <- readRDS("data/loans.rds")

accent <- "#2A5A8C"; gris <- "#9AA5B1"; ink <- "#333333"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", color = ink),
                  plot.subtitle = element_text(color = "#666666")))

# ----------------------------------------------------------------------------
# 1. VARIABLES CANDIDATES (connues à l'octroi uniquement)
# ----------------------------------------------------------------------------
vars_x <- c("fico", "ltv", "dti", "orig_rate", "orig_upb",
            "purpose", "occupancy", "channel", "first_time_buyer",
            "prop_type", "n_borrowers")

dt <- loans[, c("loan_id", "default_90", vars_x), with = FALSE]
cat("Base de modélisation :", nrow(dt), "prêts,", length(vars_x),
    "variables candidates\n")

# ----------------------------------------------------------------------------
# 2. PARTITION TRAIN / TEST (70/30, seed fixé pour reproductibilité)
# ----------------------------------------------------------------------------
# NB : avec un seul millésime (2018), pas de split "out-of-time" par date
# d'octroi possible ; la stabilité temporelle sera contrôlée au script 05
# (PSI par trimestre d'origination).
set.seed(42)
splits  <- split_df(dt, y = "default_90", ratios = c(0.7, 0.3), seed = 42)
train   <- splits$train
test    <- splits$test
cat(sprintf("Train : %d prêts (défaut %.2f%%) | Test : %d prêts (défaut %.2f%%)\n",
            nrow(train), 100 * mean(train$default_90),
            nrow(test),  100 * mean(test$default_90)))

# Sauvegarde de la partition (les scripts 04-05 réutilisent la même)
saveRDS(list(train_ids = train$loan_id, test_ids = test$loan_id),
        "data/split.rds")

# ----------------------------------------------------------------------------
# 3. DISCRÉTISATION WoE ET INFORMATION VALUE
# ----------------------------------------------------------------------------
# woebin : arbre de décision univarié, classes d'au moins ~5 % des effectifs,
# les valeurs manquantes forment leur propre classe.
bins <- woebin(train, y = "default_90", x = vars_x,
               count_distr_limit = 0.05, method = "tree")

# Information Value : pouvoir discriminant univarié de chaque variable
iv_tab <- data.table(variable = names(bins),
                     iv = sapply(bins, function(b) b$total_iv[1]))
setorder(iv_tab, -iv)
iv_tab[, force := fcase(iv >= 0.3, "fort",
                        iv >= 0.1, "moyen",
                        iv >= 0.02, "faible",
                        default = "négligeable")]
cat("\n--- Information Value (train) ---\n"); print(iv_tab)

p_iv <- ggplot(iv_tab, aes(x = reorder(variable, iv), y = iv)) +
  geom_col(fill = accent, width = 0.65) +
  geom_text(aes(label = round(iv, 3)), hjust = -0.15, size = 3.4, color = ink) +
  coord_flip() +
  geom_hline(yintercept = c(0.02, 0.1, 0.3), linetype = "dotted",
             color = "#999999") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Pouvoir discriminant des variables (Information Value)",
       subtitle = "Pointillés : seuils usuels 0,02 / 0,1 / 0,3 — échantillon d'apprentissage",
       x = NULL, y = "Information Value")
ggsave("outputs/fig_06_information_value.png", p_iv,
       width = 8, height = 5, dpi = 150)

# On retient les variables d'IV >= 0,02 (seuil d'usage)
vars_keep <- iv_tab[iv >= 0.02, variable]
cat("\nVariables retenues (IV >= 0.02) :", paste(vars_keep, collapse = ", "), "\n")

# Détail des classes WoE (exporté pour le rapport)
bins_tab <- rbindlist(bins[vars_keep], fill = TRUE)
cols_exp <- intersect(c("variable", "bin", "count", "count_distr",
                        "good", "bad", "neg", "pos", "badprob", "posprob",
                        "woe", "bin_iv"), names(bins_tab))
fwrite(bins_tab[, ..cols_exp], "outputs/bins_woe.csv")

# ----------------------------------------------------------------------------
# 4. RÉGRESSION LOGISTIQUE SUR VARIABLES WoE (modèle champion)
# ----------------------------------------------------------------------------
train_woe <- woebin_ply(train, bins[vars_keep])
test_woe  <- woebin_ply(test,  bins[vars_keep])

form <- as.formula(paste("default_90 ~",
                         paste(paste0(vars_keep, "_woe"), collapse = " + ")))
logit <- glm(form, family = binomial(), data = train_woe)
cat("\n--- Modèle logit (coefficients) ---\n")
print(summary(logit)$coefficients)

# PD prédites
pd_train <- predict(logit, train_woe, type = "response")
pd_test  <- predict(logit, test_woe,  type = "response")

auc_train <- as.numeric(auc(roc(train_woe$default_90, pd_train, quiet = TRUE)))
auc_test  <- as.numeric(auc(roc(test_woe$default_90,  pd_test,  quiet = TRUE)))
cat(sprintf("\nAUC train : %.3f (Gini %.3f) | AUC test : %.3f (Gini %.3f)\n",
            auc_train, 2 * auc_train - 1, auc_test, 2 * auc_test - 1))

# ----------------------------------------------------------------------------
# 5. GRILLE DE SCORE
# ----------------------------------------------------------------------------
# Convention : 600 points pour une cote de 1 défaut contre 19 sains,
# PDO = 50 (le risque double tous les 50 points perdus)
card <- scorecard(bins[vars_keep], logit,
                  points0 = 600, odds0 = 1/19, pdo = 50)
card_tab <- rbindlist(card, fill = TRUE)
fwrite(card_tab[!is.na(variable), .(variable, bin, woe, points)],
       "outputs/scorecard_points.csv")
cat("\nGrille de score exportée : outputs/scorecard_points.csv\n")

scores_train <- scorecard_ply(train, card)
scores_test  <- scorecard_ply(test,  card)

scored_test <- data.table(loan_id   = test$loan_id,
                          default_90 = test$default_90,
                          pd        = pd_test,
                          score     = scores_test$score)

p_dist <- ggplot(scored_test,
                 aes(x = score, fill = factor(default_90,
                                              labels = c("Sain", "Défaut")))) +
  geom_density(alpha = 0.55, color = NA) +
  scale_fill_manual(values = c("Sain" = gris, "Défaut" = accent), name = NULL) +
  labs(title = "Distribution des scores sur l'échantillon test",
       subtitle = "Séparation des populations saines et en défaut — plus les courbes sont disjointes, meilleur est le score",
       x = "Score (600 pts = cote 19:1 ; +50 pts = risque divisé par 2)",
       y = "Densité") +
  theme(legend.position = "top")
ggsave("outputs/fig_07_distribution_scores.png", p_dist,
       width = 8, height = 5, dpi = 150)

# ----------------------------------------------------------------------------
# 6. SAUVEGARDES
# ----------------------------------------------------------------------------
saveRDS(bins[vars_keep], "data/bins.rds")
saveRDS(logit,           "data/model_logit.rds")
saveRDS(card,            "data/card.rds")
saveRDS(scored_test,     "data/scored_test.rds")

cat("\n========== SYNTHÈSE SCORECARD ==========\n")
cat("Variables dans le modèle :", length(vars_keep), "\n")
cat(sprintf("AUC test : %.3f | Gini test : %.3f\n", auc_test, 2 * auc_test - 1))
cat("Scores test — min/médiane/max :",
    paste(round(quantile(scored_test$score, c(0, .5, 1), na.rm = TRUE)),
          collapse = " / "), "\n")
cat("Fichiers : data/{bins,model_logit,card,scored_test,split}.rds\n")
cat("Figures  : fig_06 (IV), fig_07 (distribution des scores)\n")
cat("Tables   : outputs/{bins_woe,scorecard_points}.csv\n")
