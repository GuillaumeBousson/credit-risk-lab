# ============================================================================
# 05_validation.R — Validation du modèle et gestion du risque de modèle
# Projet : credit-risk-lab
# Quatre dimensions de validation (échantillon test, hors apprentissage) :
#   1. DISCRIMINATION : AUC (+ IC DeLong), Gini, statistique KS
#   2. CALIBRATION    : prédit vs observé par décile, test de Hosmer-Lemeshow
#   3. STABILITÉ      : PSI du score par trimestre d'origination
#   4. BACKTESTING    : test binomial exact par classe de risque (master
#                       scale), logique feux vert / orange / rouge
# ============================================================================

library(data.table)
library(scorecard)
library(pROC)
library(ggplot2)
library(scales)

loans <- readRDS("data/loans.rds")
split <- readRDS("data/split.rds")
card  <- readRDS("data/card.rds")
logit <- readRDS("data/model_logit.rds")
bins  <- readRDS("data/bins.rds")

accent <- "#2A5A8C"; gris <- "#9AA5B1"; ink <- "#333333"
rouge <- "#C0392B"; orange_c <- "#D68910"; vert <- "#1E8449"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", color = ink),
                  plot.subtitle = element_text(color = "#666666")))

# --- Scores et PD pour TOUS les prêts (nécessaire pour le PSI temporel) ----
dt_all <- copy(loans)
dt_all[, score := scorecard_ply(dt_all, card)$score]
woe_all <- woebin_ply(dt_all, bins)
dt_all[, pd := predict(logit, woe_all, type = "response")]
dt_all[, echantillon := fifelse(loan_id %in% split$train_ids, "train", "test")]

test  <- dt_all[echantillon == "test"]
train <- dt_all[echantillon == "train"]

# ----------------------------------------------------------------------------
# 1. DISCRIMINATION : AUC (IC 95 %), Gini, KS
# ----------------------------------------------------------------------------
roc_test <- roc(test$default_90, test$pd, quiet = TRUE)
ic <- ci.auc(roc_test, method = "delong")
cat(sprintf("AUC test : %.3f  IC95%% [%.3f ; %.3f]  |  Gini : %.3f\n",
            ic[2], ic[1], ic[3], 2 * ic[2] - 1))

# Statistique KS : écart max entre les fonctions de répartition des scores
# des défauts et des sains (plus le score est bas, plus le risque est élevé)
setorder(test, score)
test[, `:=`(cdf_defaut = cumsum(default_90) / sum(default_90),
            cdf_sain   = cumsum(1 - default_90) / sum(1 - default_90))]
test[, ecart := cdf_defaut - cdf_sain]
ks <- test[which.max(ecart)]
cat(sprintf("KS : %.3f atteint au score %d\n", ks$ecart, ks$score))

p_ks <- ggplot(test, aes(x = score)) +
  geom_line(aes(y = cdf_defaut, color = "Défauts"), linewidth = 0.9) +
  geom_line(aes(y = cdf_sain, color = "Sains"), linewidth = 0.9) +
  annotate("segment", x = ks$score, xend = ks$score,
           y = ks$cdf_sain, yend = ks$cdf_defaut,
           linetype = "dashed", color = ink) +
  annotate("text", x = ks$score + 12, y = (ks$cdf_sain + ks$cdf_defaut) / 2,
           label = sprintf("KS = %.2f", ks$ecart), hjust = 0, color = ink) +
  scale_color_manual(values = c("Défauts" = accent, "Sains" = gris),
                     name = NULL) +
  labs(title = "Courbes de répartition cumulée des scores (test)",
       subtitle = "La statistique KS mesure l'écart maximal entre défauts et sains",
       x = "Score", y = "Répartition cumulée") +
  theme(legend.position = "top")
ggsave("outputs/fig_10_ks.png", p_ks, width = 8, height = 5, dpi = 150)

# ----------------------------------------------------------------------------
# 2. CALIBRATION : prédit vs observé par décile de PD + Hosmer-Lemeshow
# ----------------------------------------------------------------------------
test[, decile := cut(pd, quantile(pd, 0:10 / 10), include.lowest = TRUE,
                     labels = 1:10)]
calib <- test[, .(n = .N,
                  pd_predite = mean(pd),
                  taux_observe = mean(default_90)), by = decile][order(decile)]
print(calib)

# Test de Hosmer-Lemeshow (chi2 à 8 ddl sur les 10 déciles)
hl_chi2 <- calib[, sum((n * taux_observe - n * pd_predite)^2 /
                       (n * pd_predite * (1 - pd_predite)))]
hl_p <- pchisq(hl_chi2, df = 8, lower.tail = FALSE)
cat(sprintf("\nHosmer-Lemeshow : chi2 = %.2f (8 ddl), p-value = %.4f\n",
            hl_chi2, hl_p))
cat(sprintf("PD moyenne prédite (test) : %.2f%% | Taux observé : %.2f%%\n",
            100 * mean(test$pd), 100 * mean(test$default_90)))

p_calib <- ggplot(calib, aes(x = pd_predite, y = taux_observe)) +
  geom_abline(linetype = "dotted", color = "#999999") +
  geom_point(color = accent, size = 2.6) +
  geom_line(color = accent, linewidth = 0.6, alpha = 0.6) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Calibration : PD prédite vs taux de défaut observé (déciles, test)",
       subtitle = "Pointillés : calibration parfaite (y = x)",
       x = "PD prédite (moyenne du décile)", y = "Taux de défaut observé")
ggsave("outputs/fig_11_calibration.png", p_calib,
       width = 7, height = 5.5, dpi = 150)

# ----------------------------------------------------------------------------
# 3. STABILITÉ : PSI du score par trimestre d'origination
# ----------------------------------------------------------------------------
# Cohortes approximées par la date de premier paiement (octroi + 1-2 mois)
dt_all[, trimestre := paste0(substr(first_pay_date, 1, 4), "-T",
                             ceiling(as.integer(substr(first_pay_date, 5, 6)) / 3))]
cat("\nEffectifs par cohorte :\n")
print(dt_all[, .N, by = trimestre][order(trimestre)])

# PSI : écart de distribution des scores entre une cohorte de référence
# (la première) et chaque cohorte suivante, sur 10 classes de score
# définies sur l'ensemble. Seuils usuels : < 0,1 stable ; 0,1-0,25 à
# surveiller ; > 0,25 alerte.
bornes <- unique(quantile(dt_all$score, 0:10 / 10, na.rm = TRUE))
dt_all[, classe_score := cut(score, bornes, include.lowest = TRUE)]
# PSI calculé uniquement sur les cohortes d'effectif suffisant (n >= 500) :
# un PSI sur quelques dizaines de prêts ne mesure que du bruit d'échantillon.
eff_coh <- dt_all[, .N, by = trimestre][order(trimestre)]
cat("Cohortes exclues du PSI (n < 500) :",
    paste(eff_coh[N < 500, trimestre], collapse = ", "), "\n")
trims <- eff_coh[N >= 500, trimestre]
ref <- dt_all[trimestre == trims[1], prop.table(table(classe_score))]
psi_tab <- rbindlist(lapply(trims[-1], function(tr) {
  cur <- dt_all[trimestre == tr, prop.table(table(classe_score))]
  p_ref <- pmax(as.numeric(ref), 1e-4)
  p_cur <- pmax(as.numeric(cur), 1e-4)
  data.table(trimestre = tr,
             psi = sum((p_cur - p_ref) * log(p_cur / p_ref)))
}))
psi_tab[, statut := fcase(psi < 0.1, "stable",
                          psi < 0.25, "à surveiller",
                          default = "alerte")]
cat("\n--- PSI par cohorte (référence :", trims[1], ") ---\n")
print(psi_tab)
fwrite(psi_tab, "outputs/psi_cohortes.csv")

# ----------------------------------------------------------------------------
# 4. BACKTESTING PAR CLASSE DE RISQUE (master scale + test binomial)
# ----------------------------------------------------------------------------
# Master scale : 5 classes définies par les quintiles de score du TRAIN
# (A = meilleurs scores ... E = plus risqués), appliquées au TEST.
b5 <- unique(quantile(train$score, 0:5 / 5, na.rm = TRUE))
etiq <- LETTERS[5:1]   # E (risqué, scores bas) -> A (sûr, scores hauts)
test[,  classe := cut(score, b5, labels = etiq, include.lowest = TRUE)]
train[, classe := cut(score, b5, labels = etiq, include.lowest = TRUE)]

# PD de référence de chaque classe = PD moyenne prédite sur le TRAIN
pd_ref <- train[, .(pd_classe = mean(pd)), by = classe]

backtest <- merge(
  test[, .(n = .N, defauts = sum(default_90),
           taux_observe = mean(default_90)), by = classe],
  pd_ref, by = "classe")
setorder(backtest, classe)

# Test binomial exact : H0 "le taux observé est compatible avec la PD prédite"
backtest[, p_value := mapply(function(d, n, p)
  binom.test(d, n, p)$p.value, defauts, n, pd_classe)]
backtest[, `:=`(
  ic_bas  = mapply(function(d, n) binom.test(d, n)$conf.int[1], defauts, n),
  ic_haut = mapply(function(d, n) binom.test(d, n)$conf.int[2], defauts, n))]
backtest[, feu := fcase(p_value >= 0.05, "vert",
                        p_value >= 0.01, "orange",
                        default = "rouge")]
cat("\n--- Backtesting par classe de risque (test binomial exact) ---\n")
print(backtest[, .(classe, n, defauts,
                   pd_predite = round(pd_classe, 4),
                   taux_observe = round(taux_observe, 4),
                   p_value = round(p_value, 3), feu)])
fwrite(backtest, "outputs/backtest_classes.csv")

p_bt <- ggplot(backtest, aes(x = classe)) +
  geom_pointrange(aes(y = taux_observe, ymin = ic_bas, ymax = ic_haut,
                      color = feu), size = 0.6, linewidth = 0.8) +
  geom_point(aes(y = pd_classe), shape = 4, size = 3, stroke = 1.2,
             color = ink) +
  scale_color_manual(values = c(vert = vert, orange = orange_c,
                                rouge = rouge), guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Backtesting par classe de risque (test)",
       subtitle = "Points : taux observé avec IC binomial 95 % (couleur = feu) — Croix : PD prédite (train)",
       x = "Classe de risque (A = plus sûr)", y = "Taux de défaut")
ggsave("outputs/fig_12_backtest_classes.png", p_bt,
       width = 7.5, height = 5, dpi = 150)

saveRDS(backtest, "data/backtest.rds")
saveRDS(dt_all[, .(loan_id, score, pd, classe_score, echantillon)],
        "data/scores_all.rds")

cat("\n========== SYNTHÈSE VALIDATION ==========\n")
cat(sprintf("Discrimination : AUC %.3f [%.3f ; %.3f], KS %.2f\n",
            ic[2], ic[1], ic[3], ks$ecart))
cat(sprintf("Calibration    : HL p = %.3f (pred %.2f%% vs obs %.2f%%)\n",
            hl_p, 100 * mean(test$pd), 100 * mean(test$default_90)))
cat(sprintf("Stabilité      : PSI max = %.3f (%s)\n",
            psi_tab[, max(psi)], psi_tab[which.max(psi), trimestre]))
cat("Backtesting    :", paste(backtest$classe, backtest$feu, collapse = " | "), "\n")
cat("Figures 10-12, tables psi_cohortes.csv & backtest_classes.csv\n")
