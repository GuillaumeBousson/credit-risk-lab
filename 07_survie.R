# ============================================================================
# 07_survie.R — Analyse de survie : time-to-default
# Projet : credit-risk-lab
# Complément de l'approche cohorte (06) par l'approche par durées :
#   1. Kaplan-Meier : fonction de survie au défaut, globale et par classe
#      FICO (non paramétrique)
#   2. Modèle de Cox à hasards proportionnels : effet des caractéristiques
#      à l'octroi sur le hasard instantané de défaut (semi-paramétrique)
#   3. Diagnostic de Schoenfeld (cox.zph) : l'hypothèse de proportionnalité
#      tient-elle face au choc COVID ?
# Convention : durée = âge du prêt (mois) au premier D90+ ; les prêts
# remboursés par anticipation ou toujours actifs sont CENSURÉS à leur
# dernière observation. Limite assumée : la censure par remboursement est
# informative (risques concurrents) — extension naturelle : Fine-Gray.
# ============================================================================

library(data.table)
library(survival)
library(ggplot2)
library(scales)

loans <- readRDS("data/loans.rds")

accent <- "#2A5A8C"; ink <- "#333333"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", color = ink),
                  plot.subtitle = element_text(color = "#666666")))

# ----------------------------------------------------------------------------
# 1. PRÉPARATION DES DURÉES
# ----------------------------------------------------------------------------
surv_dt <- loans[!is.na(time_event),
                 .(loan_id, time = time_event, event = default_90,
                   fico, ltv, dti, orig_rate, n_borrowers,
                   first_time_buyer, outcome)]
surv_dt[, time := as.numeric(time)]
surv_dt[time <= 0, time := 0.5]          # Surv() exige des durées > 0
surv_dt[, fico_band := cut(fico, c(300, 660, 700, 740, 780, 850),
                           labels = c("< 660", "660-699", "700-739",
                                      "740-779", "780+"),
                           include.lowest = TRUE, right = FALSE)]
cat(sprintf("Prêts : %d | Défauts (événements) : %d | Censurés : %d\n",
            nrow(surv_dt), sum(surv_dt$event), sum(1 - surv_dt$event)))
cat("Nature des censures :\n")
print(surv_dt[event == 0, .N, by = outcome])

# ----------------------------------------------------------------------------
# 2. KAPLAN-MEIER
# ----------------------------------------------------------------------------
km_global <- survfit(Surv(time, event) ~ 1, data = surv_dt)
cat(sprintf("\nSurvie au défaut à 24 mois : %.2f%% | à 60 mois : %.2f%%\n",
            100 * summary(km_global, times = 24)$surv,
            100 * summary(km_global, times = 60)$surv))

km_fico <- survfit(Surv(time, event) ~ fico_band, data = surv_dt)

# Test du log-rank : les courbes par classe FICO diffèrent-elles ?
lr <- survdiff(Surv(time, event) ~ fico_band, data = surv_dt)
cat(sprintf("Log-rank (égalité des courbes par FICO) : chi2 = %.1f, p %s\n",
            lr$chisq,
            ifelse(pchisq(lr$chisq, 4, lower.tail = FALSE) < 1e-16,
                   "< 1e-16",
                   sprintf("= %.2e", pchisq(lr$chisq, 4, lower.tail = FALSE)))))

# Figure KM par classe FICO (gamme ordonnée de bleus : variable ordinale)
km_df <- data.table(time = km_fico$time, surv = km_fico$surv,
                    bande = rep(sub("fico_band=", "", names(km_fico$strata)),
                                km_fico$strata))
km_df[, bande := factor(bande, levels = c("< 660", "660-699", "700-739",
                                          "740-779", "780+"))]
bleus <- c("< 660" = "#9E2B25", "660-699" = "#C97A4A",
           "700-739" = "#7F9CB8", "740-779" = "#436E95", "780+" = "#1F3B57")
p_km <- ggplot(km_df, aes(time, surv, color = bande)) +
  geom_step(linewidth = 0.9) +
  scale_color_manual(values = bleus, name = "FICO à l'octroi") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(NA, 1)) +
  labs(title = "Survie au défaut (Kaplan-Meier) par classe FICO",
       subtitle = "Censure : prêts remboursés par anticipation ou actifs en fin de fenêtre",
       x = "Âge du prêt (mois)", y = "Probabilité de non-défaut") +
  theme(legend.position = "right")
ggsave("outputs/fig_14_km_fico.png", p_km, width = 8.5, height = 5, dpi = 150)

# ----------------------------------------------------------------------------
# 3. MODÈLE DE COX
# ----------------------------------------------------------------------------
cox_dt <- surv_dt[complete.cases(surv_dt[, .(fico, ltv, dti, orig_rate,
                                             n_borrowers)])]
cat(sprintf("\nCox estimé sur %d prêts (%d exclus pour valeurs manquantes)\n",
            nrow(cox_dt), nrow(surv_dt) - nrow(cox_dt)))

cox <- coxph(Surv(time, event) ~ fico + ltv + dti + orig_rate +
               n_borrowers + first_time_buyer, data = cox_dt)
s <- summary(cox)
hr <- data.table(variable = rownames(s$coefficients),
                 hazard_ratio = s$conf.int[, "exp(coef)"],
                 ic_bas  = s$conf.int[, "lower .95"],
                 ic_haut = s$conf.int[, "upper .95"],
                 p_value = s$coefficients[, "Pr(>|z|)"])
cat("\n--- Hazard ratios (Cox) ---\n")
print(hr[, .(variable, hazard_ratio = round(hazard_ratio, 4),
             ic = sprintf("[%.3f ; %.3f]", ic_bas, ic_haut),
             p_value = signif(p_value, 3))])
fwrite(hr, "outputs/cox_hazard_ratios.csv")

cat(sprintf("\nHR pour -10 points de FICO : x%.3f | pour +10 points de DTI : x%.3f\n",
            exp(-10 * coef(cox)["fico"]), exp(10 * coef(cox)["dti"])))
cat(sprintf("Concordance (C-index) : %.3f — à rapprocher de l'AUC 0.720 du logit\n",
            s$concordance["C"]))

# ----------------------------------------------------------------------------
# 4. DIAGNOSTIC DE PROPORTIONNALITÉ (résidus de Schoenfeld)
# ----------------------------------------------------------------------------
zph <- cox.zph(cox)
cat("\n--- Test de Schoenfeld (H0 : hasards proportionnels) ---\n")
print(zph$table)
cat("\nLecture attendue : le choc COVID concentre le hasard sur 2020 ;\n",
    "la proportionnalité dans le temps est donc mise à l'épreuve — même\n",
    "message que la non-homogénéité markovienne du script 06.\n")

saveRDS(cox, "data/model_cox.rds")

cat("\n========== SYNTHÈSE SURVIE ==========\n")
cat(sprintf("Événements : %d défauts | C-index Cox : %.3f\n",
            sum(surv_dt$event), s$concordance["C"]))
cat("Figure 14 (KM par FICO), table cox_hazard_ratios.csv\n")
