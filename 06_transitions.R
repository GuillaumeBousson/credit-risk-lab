# ============================================================================
# 06_transitions.R — Matrices de transition des états d'impayés
# Projet : credit-risk-lab
# Approche cohorte (fréquences empiriques de transition) :
#   - États : C (à jour), D30, D60, puis deux états ABSORBANTS :
#     D90+ (défaut) et P (remboursé par anticipation).
#     Convention du défaut absorbant : un prêt passé en D90+ reste compté
#     en défaut même s'il "guérit" ensuite (sinon la matrice sous-estime
#     le risque de pertes).
#   - Matrice MENSUELLE : transitions entre deux mois consécutifs
#   - Matrice ANNUELLE empirique : transitions à 12 mois d'écart
#   - Contrôle d'homogénéité : la matrice mensuelle élevée à la puissance 12
#     est comparée à l'annuelle empirique ; un écart signale que la chaîne
#     n'est pas homogène dans le temps (dépendance de durée, chocs comme
#     le COVID...)
# ============================================================================

library(data.table)
library(ggplot2)

perf <- readRDS("data/perf_states.rds")

accent <- "#2A5A8C"; ink <- "#333333"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid = element_blank(),
                  plot.title = element_text(face = "bold", color = ink),
                  plot.subtitle = element_text(color = "#666666")))

etats <- c("C", "D30", "D60", "D90+", "P")

# ----------------------------------------------------------------------------
# 1. ÉTATS ABSORBANTS (défaut prioritaire sur remboursement)
# ----------------------------------------------------------------------------
perf[, idx := as.integer(substr(period, 1, 4)) * 12L +
              as.integer(substr(period, 5, 6))]
setorder(perf, loan_id, idx)

abs_dates <- perf[, .(
  idx_def  = if (any(state == "D90+")) min(idx[state == "D90+"]) else NA_integer_,
  idx_prep = if (any(state == "P"))    min(idx[state == "P"])    else NA_integer_
), by = loan_id]

perf <- merge(perf, abs_dates, by = "loan_id")
perf[, state_abs := state]
perf[idx >= idx_prep, state_abs := "P"]
perf[idx >= idx_def,  state_abs := "D90+"]   # le défaut prime
perf[, state_abs := factor(state_abs, levels = etats)]

# ----------------------------------------------------------------------------
# 2. COMPLÉTION DU PANEL APRÈS ABSORPTION
# ----------------------------------------------------------------------------
# Un prêt remboursé (P) ou liquidé après défaut sort du fichier Freddie Mac :
# il n'a plus de lignes ensuite. Sans complétion, la matrice annuelle
# empirique perd toutes les paires (t, t+12) des prêts sortis entre-temps
# (troncature à droite) et sous-estime massivement les transitions vers P
# et D90+. L'approche cohorte exige l'état de CHAQUE prêt en fin de période :
# on prolonge donc les prêts absorbés dans leur état absorbant jusqu'à la
# fin de la fenêtre d'observation.
idx_max <- max(perf$idx)
fin <- perf[, .(idx_last   = max(idx),
                state_last = state_abs[which.max(idx)]), by = loan_id]
ext <- fin[state_last %in% c("D90+", "P") & idx_last < idx_max]
synth <- ext[, .(idx = seq.int(idx_last + 1L, idx_max)),
             by = .(loan_id, state_abs = state_last)]
panel <- rbind(perf[, .(loan_id, idx, state_abs)],
               synth[, .(loan_id, idx, state_abs)])
setorder(panel, loan_id, idx)
cat("Lignes synthétiques ajoutées après absorption :", nrow(synth), "\n")

# ----------------------------------------------------------------------------
# 3. MATRICE MENSUELLE (approche cohorte : paires de mois consécutifs)
# ----------------------------------------------------------------------------
panel[, `:=`(state_next = shift(state_abs, -1L),
             idx_next   = shift(idx, -1L)), by = loan_id]
paires <- panel[!is.na(state_next) & idx_next == idx + 1L]
cat("Paires de transitions mensuelles :", nrow(paires), "\n")

mat_freq <- function(from, to) {
  tab <- table(factor(from, levels = etats), factor(to, levels = etats))
  M <- prop.table(tab, margin = 1)
  M[is.nan(M)] <- 0
  # États absorbants imposés exactement (lignes identité)
  for (a in c("D90+", "P")) { M[a, ] <- 0; M[a, a] <- 1 }
  M
}

M1 <- mat_freq(paires$state_abs, paires$state_next)
cat("\n--- Matrice de transition MENSUELLE (%, approche cohorte) ---\n")
print(round(100 * M1, 2))

# ----------------------------------------------------------------------------
# 4. MATRICE ANNUELLE EMPIRIQUE (paires à 12 mois d'écart, panel complété)
# ----------------------------------------------------------------------------
base   <- panel[, .(loan_id, idx, state_abs)]
cible  <- panel[, .(loan_id, idx12 = idx - 12L, state_12m = state_abs)]
paires12 <- merge(base, cible, by.x = c("loan_id", "idx"),
                  by.y = c("loan_id", "idx12"))
cat("\nPaires de transitions à 12 mois :", nrow(paires12), "\n")

M12_emp <- mat_freq(paires12$state_abs, paires12$state_12m)
cat("\n--- Matrice ANNUELLE empirique (%) ---\n")
print(round(100 * M12_emp, 2))

# ----------------------------------------------------------------------------
# 5. CONTRÔLE D'HOMOGÉNÉITÉ : M1^12 vs annuelle empirique
# ----------------------------------------------------------------------------
mat_pow <- function(M, k) Reduce(`%*%`, replicate(k, M, simplify = FALSE))
M12_markov <- mat_pow(M1, 12)
cat("\n--- Matrice mensuelle élevée à la puissance 12 (%) ---\n")
print(round(100 * M12_markov, 2))

ecart <- M12_markov - M12_emp
cat(sprintf("\nÉcart max |M1^12 - annuelle empirique| : %.2f point(s) de %%\n",
            100 * max(abs(ecart))))
cat(sprintf("PD annuelle depuis C : empirique %.2f%% vs Markov %.2f%%\n",
            100 * M12_emp["C", "D90+"], 100 * M12_markov["C", "D90+"]))

# ----------------------------------------------------------------------------
# 6. FIGURE ET EXPORTS
# ----------------------------------------------------------------------------
dt_M1 <- as.data.table(as.table(M1))
setnames(dt_M1, c("de", "vers", "prob"))
dt_M1[, `:=`(de = factor(de, levels = rev(etats)),
             vers = factor(vers, levels = etats))]

p_heat <- ggplot(dt_M1, aes(x = vers, y = de, fill = prob)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f", 100 * prob)),
            color = ink, size = 3.6) +
  scale_fill_gradient(low = "#FFFFFF", high = accent, guide = "none") +
  labs(title = "Matrice de transition mensuelle des états d'impayés (%)",
       subtitle = "Approche cohorte, millésime 2018 — D90+ et P absorbants (le défaut prime)",
       x = "État au mois t+1", y = "État au mois t") +
  theme(axis.text = element_text(color = ink))
ggsave("outputs/fig_13_matrice_transition.png", p_heat,
       width = 7, height = 5, dpi = 150)

fwrite(data.table(de = rownames(M1), round(100 * M1, 3)),
       "outputs/matrice_mensuelle.csv")
fwrite(data.table(de = rownames(M12_emp), round(100 * M12_emp, 3)),
       "outputs/matrice_annuelle_empirique.csv")
saveRDS(list(M1 = M1, M12_emp = M12_emp, M12_markov = M12_markov),
        "data/matrices_transition.rds")

cat("\n========== SYNTHÈSE TRANSITIONS ==========\n")
cat(sprintf("Persistance mensuelle C->C : %.1f%% | D60->D90+ : %.1f%%\n",
            100 * M1["C", "C"], 100 * M1["D60", "D90+"]))
cat("Diagonale dominante attendue (système stable) ; l'écart Markov vs\n",
    "empirique quantifie la non-homogénéité temporelle (choc COVID).\n")
cat("Figure 13, matrices CSV dans outputs/, matrices RDS dans data/\n")
