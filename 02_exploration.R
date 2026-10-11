# ============================================================================
# 02_exploration.R — Analyse exploratoire du risque de défaut
# Projet : credit-risk-lab
# Objectif :
#   1. Mesurer le taux de défaut (D90+) par segment de risque à l'octroi
#      (FICO, LTV, DTI, objet du prêt, occupation...) — intuition des
#      variables qui discrimineront dans la scorecard (script 03)
#   2. Situer les défauts dans le temps : chronologie des entrées en défaut
#      (mise en évidence attendue du choc COVID 2020) et courbe de montée
#      en défaut par âge du prêt (vintage curve)
#   3. Sauvegarder figures (outputs/) et tables de synthèse
# ============================================================================

library(data.table)
library(ggplot2)
library(scales)

loans       <- readRDS("data/loans.rds")
perf_states <- readRDS("data/perf_states.rds")

# --- Charte graphique sobre : un accent, des gris -----------------------------
accent <- "#2A5A8C"   # bleu — unique couleur de données
ink    <- "#333333"
theme_set(
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold", color = ink),
          plot.subtitle = element_text(color = "#666666"),
          axis.title = element_text(color = "#666666"))
)
save_fig <- function(p, nom, w = 8, h = 5) {
  ggsave(file.path("outputs", nom), p, width = w, height = h, dpi = 150)
  cat("Figure enregistrée :", file.path("outputs", nom), "\n")
}

taux_global <- mean(loans$default_90)
cat(sprintf("Taux de défaut global (D90+) : %.2f%% sur %d prêts\n\n",
            100 * taux_global, nrow(loans)))

# ----------------------------------------------------------------------------
# 1. TAUX DE DÉFAUT PAR SEGMENT À L'OCTROI
# ----------------------------------------------------------------------------
# Découpages en classes usuelles du crédit hypothécaire US
loans[, fico_band := cut(fico, c(300, 660, 700, 740, 780, 850),
                         labels = c("< 660", "660-699", "700-739",
                                    "740-779", "780+"),
                         include.lowest = TRUE, right = FALSE)]
loans[, ltv_band  := cut(ltv, c(0, 60, 70, 80, 90, 95, 250),
                         labels = c("≤ 60", "60-70", "70-80",
                                    "80-90", "90-95", "> 95"),
                         include.lowest = TRUE)]
loans[, dti_band  := cut(dti, c(0, 25, 35, 43, 50, 65),
                         labels = c("≤ 25", "25-35", "35-43", "43-50", "> 50"),
                         include.lowest = TRUE)]

taux_par <- function(var) {
  res <- loans[!is.na(get(var)),
               .(n = .N, taux_defaut = mean(default_90)), by = var]
  setorderv(res, var)
  res
}

fig_segment <- function(res, var, titre, xlab) {
  ggplot(res, aes(x = .data[[var]], y = taux_defaut)) +
    geom_col(fill = accent, width = 0.65) +
    geom_hline(yintercept = taux_global, linetype = "dashed",
               color = "#999999") +
    geom_text(aes(label = percent(taux_defaut, accuracy = 0.1)),
              vjust = -0.5, size = 3.4, color = ink) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(title = titre,
         subtitle = sprintf("Pointillés : taux global (%.1f%%) — prêts 2018, défaut = 90+ jours d'impayés",
                            100 * taux_global),
         x = xlab, y = "Taux de défaut")
}

res_fico <- taux_par("fico_band")
res_ltv  <- taux_par("ltv_band")
res_dti  <- taux_par("dti_band")
print(res_fico); print(res_ltv); print(res_dti)

save_fig(fig_segment(res_fico, "fico_band",
                     "Le score FICO à l'octroi discrimine fortement le défaut",
                     "Score FICO à l'octroi"), "fig_01_defaut_fico.png")
save_fig(fig_segment(res_ltv, "ltv_band",
                     "Taux de défaut croissant avec le LTV",
                     "Loan-To-Value à l'octroi (%)"), "fig_02_defaut_ltv.png")
save_fig(fig_segment(res_dti, "dti_band",
                     "Taux de défaut croissant avec le taux d'endettement",
                     "Debt-To-Income à l'octroi (%)"), "fig_03_defaut_dti.png")

# --- Variables qualitatives : table de synthèse ------------------------------
cat("\n--- Taux de défaut par variable qualitative ---\n")
for (v in c("purpose", "occupancy", "channel", "first_time_buyer",
            "prop_type", "n_borrowers")) {
  res <- loans[, .(n = .N, taux_defaut = round(mean(default_90), 4)),
               by = v][order(-taux_defaut)]
  cat("\n*", v, "\n"); print(res)
}

# Export d'une table de synthèse unique (réutilisée dans le rapport)
synthese <- rbindlist(lapply(
  c("fico_band", "ltv_band", "dti_band", "purpose", "occupancy"),
  function(v) {
    res <- taux_par(v)
    data.table(variable = v, modalite = as.character(res[[v]]),
               n = res$n, taux_defaut = res$taux_defaut)
  }))
fwrite(synthese, "outputs/taux_defaut_segments.csv")
cat("\nTable enregistrée : outputs/taux_defaut_segments.csv\n")

# ----------------------------------------------------------------------------
# 2. CHRONOLOGIE DES ENTRÉES EN DÉFAUT (effet COVID attendu)
# ----------------------------------------------------------------------------
# Premier mois où chaque prêt atteint D90+
setorder(perf_states, loan_id, period)
premiers_defauts <- perf_states[state == "D90+",
                                .(period_defaut = first(period)), by = loan_id]
defauts_mois <- premiers_defauts[, .N, by = period_defaut][order(period_defaut)]
defauts_mois[, date := as.Date(paste0(period_defaut, "01"), "%Y%m%d")]

p_chrono <- ggplot(defauts_mois, aes(x = date, y = N)) +
  geom_col(fill = accent, width = 25) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Entrées en défaut (D90+) par mois — le choc COVID est net",
       subtitle = "Millésime 2018 : pic des passages à 90+ jours d'impayés mi-2020 (reports de paiement)",
       x = NULL, y = "Nombre de prêts entrant en défaut")
save_fig(p_chrono, "fig_04_chronologie_defauts.png", w = 9, h = 4.5)

cat("\nTop 6 des mois d'entrée en défaut :\n")
print(head(defauts_mois[order(-N)], 6))

# ----------------------------------------------------------------------------
# 3. COURBE DE MONTÉE EN DÉFAUT (vintage curve)
# ----------------------------------------------------------------------------
# Part cumulée des prêts entrés en défaut selon l'âge du prêt (en mois)
ages <- loans[default_90 == 1L & !is.na(age_default), .N, by = age_default]
setorder(ages, age_default)
ages[, cum_defaut := cumsum(N) / nrow(loans)]

p_vintage <- ggplot(ages, aes(x = age_default, y = cum_defaut)) +
  geom_line(color = accent, linewidth = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 0.5),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Montée en défaut du millésime 2018",
       subtitle = "Part cumulée des prêts ayant atteint 90+ jours d'impayés, par âge du prêt",
       x = "Âge du prêt (mois)", y = "Taux de défaut cumulé")
save_fig(p_vintage, "fig_05_vintage_curve.png", w = 8, h = 4.5)

cat("\n========== SYNTHÈSE EXPLORATION ==========\n")
cat("Multiplicateur de risque FICO (<660 vs 780+) : x",
    round(res_fico[fico_band == "< 660", taux_defaut] /
          res_fico[fico_band == "780+", taux_defaut], 1), "\n")
cat("5 figures dans outputs/, 1 table CSV\n")
cat("Variables candidates pour la scorecard (03) : fico, ltv, dti, purpose,\n",
    "occupancy, channel, first_time_buyer, n_borrowers, orig_upb, orig_rate\n")
