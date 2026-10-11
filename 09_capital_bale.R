# ============================================================================
# 09_capital_bale.R — Capital réglementaire : approche IRB vs standard
# Projet : credit-risk-lab
# Périmètre : encours sains (stages 1-2) — le capital couvre la perte
# INATTENDUE ; les expositions en défaut relèvent du provisionnement (08).
#
# 1. IRB (Bâle, expositions retail hypothécaires) :
#      WCDR = N( (N^-1(PD) + sqrt(rho) * N^-1(0,999)) / sqrt(1 - rho) )
#      K    = LGD * (WCDR - PD)        [pas d'ajustement de maturité en retail]
#      RWA  = 12,5 * K * EAD
#    avec rho = 0,15 (fixé par le régulateur pour l'hypothécaire résidentiel)
#    et un plancher de PD de 0,05 % (Bâle III).
# 2. Approche STANDARD (Bâle III, immobilier résidentiel) : pondération
#    forfaitaire par tranche de LTV (20 % à 70 %).
# 3. Comparaison IRB / standard -> discussion de l'output floor (72,5 %).
# ============================================================================

library(data.table)
library(ggplot2)
library(scales)

book  <- readRDS("data/ecl_book.rds")
loans <- readRDS("data/loans.rds")

accent <- "#2A5A8C"; ink <- "#333333"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", color = ink),
                  plot.subtitle = element_text(color = "#666666")))

LGD <- 0.25
RHO <- 0.15        # corrélation réglementaire, hypothécaire résidentiel
PD_FLOOR <- 0.0005 # plancher de PD Bâle III (0,05 %)

# ----------------------------------------------------------------------------
# 1. APPROCHE IRB (stages 1-2)
# ----------------------------------------------------------------------------
sains <- merge(book[stage %in% 1:2],
               loans[, .(loan_id, ltv)], by = "loan_id")
sains[, pd_irb := pmax(pd_12m, PD_FLOOR)]

wcdr <- function(pd, rho) {
  pnorm((qnorm(pd) + sqrt(rho) * qnorm(0.999)) / sqrt(1 - rho))
}
sains[, wcdr := wcdr(pd_irb, RHO)]
sains[, k_irb := LGD * (wcdr - pd_irb)]      # exigence en % de l'EAD
sains[, capital_irb := k_irb * ead]
sains[, rwa_irb := 12.5 * capital_irb]

cat(sprintf("Périmètre sain : %d prêts, EAD %.1f M$\n",
            nrow(sains), sum(sains$ead) / 1e6))
cat(sprintf("PD annuelle : médiane %.2f%%, moyenne %.2f%% (plancher %.2f%% appliqué à %d prêts)\n",
            100 * median(sains$pd_irb), 100 * mean(sains$pd_irb),
            100 * PD_FLOOR, sains[pd_12m < PD_FLOOR, .N]))

synth_irb <- sains[, .(n = .N, ead_MUSD = sum(ead) / 1e6,
                       k_moyen = weighted.mean(k_irb, ead),
                       capital_MUSD = sum(capital_irb) / 1e6,
                       rwa_MUSD = sum(rwa_irb) / 1e6), by = stage][order(stage)]
cat("\n--- IRB par stage ---\n")
print(synth_irb[, .(stage, n, ead_MUSD = round(ead_MUSD, 1),
                    k_moyen = percent(k_moyen, accuracy = 0.01),
                    capital_MUSD = round(capital_MUSD, 2),
                    rwa_MUSD = round(rwa_MUSD, 1))])

# ----------------------------------------------------------------------------
# 2. APPROCHE STANDARD (pondérations Bâle III par tranche de LTV)
# ----------------------------------------------------------------------------
# NB : LTV d'octroi utilisé comme proxy du LTV courant (simplification
# conservatrice : l'amortissement et la hausse des prix l'ont fait baisser).
rw_std <- function(ltv) {
  fcase(is.na(ltv), 0.70,
        ltv <= 50,  0.20,
        ltv <= 60,  0.25,
        ltv <= 80,  0.30,
        ltv <= 90,  0.40,
        ltv <= 100, 0.50,
        default = 0.70)
}
sains[, rw_standard := rw_std(ltv)]
sains[, rwa_std := rw_standard * ead]
sains[, capital_std := 0.08 * rwa_std]

# ----------------------------------------------------------------------------
# 3. COMPARAISON ET OUTPUT FLOOR
# ----------------------------------------------------------------------------
tot <- data.table(
  approche = c("IRB", "Standard"),
  rwa_MUSD = c(sum(sains$rwa_irb), sum(sains$rwa_std)) / 1e6,
  capital_MUSD = c(sum(sains$capital_irb), sum(sains$capital_std)) / 1e6)
tot[, densite_rwa := rwa_MUSD * 1e6 / sum(sains$ead)]
cat("\n--- Comparaison des approches (capital = 8 % des RWA) ---\n")
print(tot[, .(approche, rwa_MUSD = round(rwa_MUSD, 1),
              capital_MUSD = round(capital_MUSD, 2),
              densite_rwa = percent(densite_rwa, accuracy = 0.1))])

ratio <- tot[approche == "IRB", rwa_MUSD] / tot[approche == "Standard", rwa_MUSD]
floor_rwa <- 0.725 * tot[approche == "Standard", rwa_MUSD]
cat(sprintf("\nRWA IRB / RWA standard : %.1f%%\n", 100 * ratio))
cat(sprintf("Output floor Bâle III (72,5%% du standard) : %.1f M$ de RWA\n", floor_rwa))
if (tot[approche == "IRB", rwa_MUSD] < floor_rwa) {
  cat("-> L'IRB tombe SOUS le plancher : les RWA retenus seraient ceux du floor.\n")
} else {
  cat("-> L'IRB reste au-dessus du plancher : RWA IRB retenus.\n")
}
fwrite(tot, "outputs/capital_comparaison.csv")

# ----------------------------------------------------------------------------
# 4. FIGURE : la courbe réglementaire K(PD) et le portefeuille
# ----------------------------------------------------------------------------
grille <- data.table(pd = exp(seq(log(0.0005), log(0.20), length.out = 200)))
grille[, k := LGD * (wcdr(pd, RHO) - pd)]

p_k <- ggplot() +
  geom_line(data = grille, aes(pd, k), color = accent, linewidth = 1) +
  geom_rug(data = sains[sample(.N, min(.N, 3000))], aes(x = pd_irb),
           sides = "b", alpha = 0.15, color = ink, length = unit(0.03, "npc")) +
  scale_x_log10(labels = percent_format(accuracy = 0.01)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Exigence de capital IRB en fonction de la PD",
       subtitle = sprintf("K = LGD x (WCDR - PD), rho = 0,15, LGD = 25 %% — traits : PD du portefeuille sain (échantillon)"),
       x = "PD annuelle (échelle log)", y = "K (% de l'EAD)")
ggsave("outputs/fig_16_capital_irb.png", p_k, width = 8, height = 5, dpi = 150)

cat("\n========== SYNTHÈSE CAPITAL ==========\n")
cat(sprintf("Capital IRB : %.2f M$ (densité RWA %.1f%%) | Standard : %.2f M$ (densité %.1f%%)\n",
            tot[approche == "IRB", capital_MUSD],
            100 * tot[approche == "IRB", densite_rwa],
            tot[approche == "Standard", capital_MUSD],
            100 * tot[approche == "Standard", densite_rwa]))
cat("Figure 16, table capital_comparaison.csv\n")
