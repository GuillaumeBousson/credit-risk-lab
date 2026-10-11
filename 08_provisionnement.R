# ============================================================================
# 08_provisionnement.R — Provisionnement attendu type IFRS 9 (ECL)
# Projet : credit-risk-lab
# Chaîne : PD (scorecard recalée) x LGD (paramètre) x EAD (capital restant
# dû par formule d'annuité), avec staging IFRS 9 :
#   - Stage 1 : prêts sains                      -> ECL à 12 mois
#   - Stage 2 : hausse significative du risque   -> ECL à maturité
#     (proxy standard : a déjà connu un impayé 30 j+, "backstop" IFRS 9)
#   - Stage 3 : défaut (convention absorbante)   -> PD = 1
# Préalable : RECALAGE D'INTERCEPT du logit (recommandation du script 05) —
# la constante est ajustée pour que la PD moyenne prédite égale le taux
# observé hors apprentissage ; le classement (discrimination) est inchangé.
# ============================================================================

library(data.table)
library(scorecard)
library(ggplot2)
library(scales)

loans <- readRDS("data/loans.rds")
bins  <- readRDS("data/bins.rds")
logit <- readRDS("data/model_logit.rds")
split <- readRDS("data/split.rds")
mats  <- readRDS("data/matrices_transition.rds")

accent <- "#2A5A8C"; ink <- "#333333"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  plot.title = element_text(face = "bold", color = ink),
                  plot.subtitle = element_text(color = "#666666")))

# ----------------------------------------------------------------------------
# 1. RECALAGE D'INTERCEPT (sur l'échantillon test, hors apprentissage)
# ----------------------------------------------------------------------------
woe_all <- woebin_ply(loans, bins)
loans[, lp := predict(logit, woe_all, type = "link")]     # prédicteur linéaire

test_lp <- loans[loan_id %in% split$test_ids]
cible   <- mean(test_lp$default_90)
delta   <- uniroot(function(d) mean(plogis(test_lp$lp + d)) - cible,
                   interval = c(-2, 2))$root
loans[, pd_vie := plogis(lp + delta)]
cat(sprintf("Recalage d'intercept : delta = %+.4f\n", delta))
cat(sprintf("PD moyenne (test) avant : %.2f%% | après : %.2f%% | observé : %.2f%%\n",
            100 * mean(plogis(test_lp$lp)),
            100 * mean(plogis(test_lp$lp + delta)), 100 * cible))

# ----------------------------------------------------------------------------
# 2. PD À 12 MOIS (ancrage sur la matrice de transition annuelle)
# ----------------------------------------------------------------------------
# Simplification documentée : structure de terme plate — la PD vie entière
# de chaque prêt est proportionnellement ramenée à l'horizon 12 mois via le
# ratio PD annuelle du portefeuille (transition C -> D90+ du script 06) /
# PD vie entière moyenne.
pd12_portefeuille <- mats$M12_emp["C", "D90+"]
k_annuel <- pd12_portefeuille / mean(loans$pd_vie)
loans[, pd_12m := pmin(pd_vie * k_annuel, pd_vie)]
cat(sprintf("\nAncrage 12 mois : PD annuelle portefeuille %.2f%% -> facteur %.3f\n",
            100 * pd12_portefeuille, k_annuel))

# ----------------------------------------------------------------------------
# 3. EAD : CAPITAL RESTANT DÛ (formule d'annuité)
# ----------------------------------------------------------------------------
# B_k = UPB x ((1+r)^n - (1+r)^k) / ((1+r)^n - 1), r mensuel, n = durée,
# k = âge du prêt (dernière observation, ou âge au défaut pour le Stage 3)
solde_restant <- function(upb, taux_annuel, n, k) {
  r <- taux_annuel / 1200
  k <- pmin(pmax(k, 0L), n)
  b <- fifelse(r > 0,
               upb * ((1 + r)^n - (1 + r)^k) / ((1 + r)^n - 1),
               upb * (n - k) / n)
  pmax(b, 0)
}

# ----------------------------------------------------------------------------
# 4. STAGING ET CALCUL DE L'ECL
# ----------------------------------------------------------------------------
LGD <- 0.25   # paramètre central (hypothécaire US, collatéralisé)

book <- loans[outcome %in% c("active_censored", "default")]
book[, stage := fcase(
  outcome == "default",                      3L,
  outcome == "active_censored" & max_dlq >= 1L, 2L,
  default = 1L
)]
book[, age_ead := fifelse(stage == 3L, age_default, last_age)]
book[, ead := solde_restant(orig_upb, orig_rate, orig_term, age_ead)]

book[, ecl := fcase(
  stage == 1L, ead * LGD * pd_12m,
  stage == 2L, ead * LGD * pd_vie,
  stage == 3L, ead * LGD
)]

synth <- book[, .(n = .N,
                  ead_MUSD = sum(ead) / 1e6,
                  ecl_MUSD = sum(ecl) / 1e6,
                  taux_couverture = sum(ecl) / sum(ead)),
              by = stage][order(stage)]
synth <- rbind(synth,
               data.table(stage = NA, n = sum(synth$n),
                          ead_MUSD = sum(synth$ead_MUSD),
                          ecl_MUSD = sum(synth$ecl_MUSD),
                          taux_couverture = sum(synth$ecl_MUSD) /
                                            sum(synth$ead_MUSD)))
cat("\n--- ECL par stage (LGD = 25 %) ---\n")
print(synth[, .(stage = fifelse(is.na(stage), "TOTAL", as.character(stage)),
                n, ead_MUSD = round(ead_MUSD, 1),
                ecl_MUSD = round(ecl_MUSD, 2),
                taux_couverture = percent(taux_couverture, accuracy = 0.01))])
fwrite(synth, "outputs/ecl_par_stage.csv")

# ----------------------------------------------------------------------------
# 5. SENSIBILITÉ À LA LGD
# ----------------------------------------------------------------------------
sensi <- rbindlist(lapply(c(0.15, 0.25, 0.40), function(l) {
  data.table(LGD = l,
             ecl_MUSD = book[, sum(fcase(stage == 1L, ead * l * pd_12m,
                                         stage == 2L, ead * l * pd_vie,
                                         stage == 3L, ead * l))] / 1e6)
}))
cat("\n--- Sensibilité de l'ECL totale à la LGD ---\n")
print(sensi[, .(LGD = percent(LGD), ecl_MUSD = round(ecl_MUSD, 2))])
fwrite(sensi, "outputs/ecl_sensibilite_lgd.csv")

# ----------------------------------------------------------------------------
# 6. FIGURE
# ----------------------------------------------------------------------------
fig_dt <- synth[!is.na(stage)]
fig_dt[, lib := paste0("Stage ", stage)]
p_ecl <- ggplot(fig_dt, aes(x = lib, y = ecl_MUSD)) +
  geom_col(fill = accent, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f M$\n(%s de l'EAD)", ecl_MUSD,
                                percent(taux_couverture, accuracy = 0.1))),
            vjust = -0.3, size = 3.4, color = ink, lineheight = 0.95) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(title = "Provisions attendues (ECL) par stage IFRS 9",
       subtitle = sprintf("LGD 25 %% — portefeuille : %s prêts, EAD %.0f M$ (capital restant dû, formule d'annuité)",
                          format(sum(fig_dt$n), big.mark = " "),
                          sum(fig_dt$ead_MUSD)),
       x = NULL, y = "ECL (millions $)")
ggsave("outputs/fig_15_ecl_stages.png", p_ecl, width = 8, height = 5, dpi = 150)

saveRDS(book[, .(loan_id, stage, ead, pd_12m, pd_vie, ecl)],
        "data/ecl_book.rds")

cat("\n========== SYNTHÈSE PROVISIONNEMENT ==========\n")
cat(sprintf("ECL totale : %.2f M$ (%.2f%% de l'EAD) — LGD centrale 25%%\n",
            synth[is.na(stage), ecl_MUSD],
            100 * synth[is.na(stage), taux_couverture]))
cat("Répartition :", paste0("Stage ", fig_dt$stage, " = ",
                            round(fig_dt$ecl_MUSD, 1), " M$",
                            collapse = " | "), "\n")
cat("Figure 15, tables ecl_par_stage.csv & ecl_sensibilite_lgd.csv\n")
