# ============================================================================
# 01_data_prep.R — Préparation des données Freddie Mac SFLLD (sample 2018)
# Projet : credit-risk-lab
# Objectif :
#   1. Lire le fichier origination (caractéristiques des prêts à l'octroi)
#   2. Lire le fichier performance (suivi mensuel), en extraire :
#        - la cible de défaut : premier passage à 90+ jours d'impayés (D90)
#        - l'issue du prêt : actif / remboursé par anticipation / défaut
#        - l'âge du prêt au moment de l'événement (pour l'analyse de survie)
#   3. Sauvegarder deux tables propres :
#        - data/loans.rds        : 1 ligne par prêt (features + cible)
#        - data/perf_states.rds  : panel mensuel allégé (pour les matrices
#                                  de transition et la survie)
# Remarque : les fichiers bruts sont délimités par "|" et SANS en-têtes.
#            Le mapping des colonnes suit le layout officiel SFLLD.
# ============================================================================

library(data.table)

t0 <- Sys.time()

# ----------------------------------------------------------------------------
# 1. FICHIER ORIGINATION
# ----------------------------------------------------------------------------
f_orig <- "data/raw/sample_orig_2018.txt"

# Noms des 23 premières colonnes (layout SFLLD, stable dans le temps).
# Les colonnes suivantes (vendeur, servicer, indicateurs divers) ne servent
# pas à la modélisation : on les nomme génériquement.
noms_orig <- c(
  "fico",            # score de crédit à l'octroi (300-850 ; 9999 = manquant)
  "first_pay_date",  # AAAAMM du premier paiement
  "first_time_buyer",# primo-accédant (Y/N/9)
  "maturity_date",   # AAAAMM d'échéance
  "msa",             # zone métropolitaine
  "mi_pct",          # % assurance hypothécaire (999 = manquant)
  "n_units",         # nombre de logements (1-4 ; 99 = manquant)
  "occupancy",       # P = résidence principale, S = secondaire, I = investissement
  "cltv",            # Combined LTV à l'octroi (999 = manquant)
  "dti",             # Debt-To-Income (%) (999 = manquant)
  "orig_upb",        # montant du prêt à l'octroi ($)
  "ltv",             # Loan-To-Value à l'octroi (999 = manquant)
  "orig_rate",       # taux d'intérêt à l'octroi (%)
  "channel",         # R = Retail, B = Broker, C = Correspondent, 9 = manquant
  "ppm_flag",        # pénalité de remboursement anticipé (Y/N)
  "amort_type",      # FRM = taux fixe, ARM = taux variable
  "state",           # État US
  "prop_type",       # SF = maison individuelle, CO = condo, etc.
  "zip3",            # code postal (3 premiers chiffres)
  "loan_id",         # identifiant unique du prêt (clé de jointure)
  "purpose",         # P = achat, C = refi cash-out, N = refi sans cash-out
  "orig_term",       # durée initiale (mois)
  "n_borrowers"      # nombre d'emprunteurs (99 = manquant)
)

orig <- fread(f_orig, sep = "|", header = FALSE, quote = "",
              colClasses = "character", na.strings = c("", "NA"))

cat("Origination : ", nrow(orig), "prêts,", ncol(orig), "colonnes lues\n")

# On nomme les 23 premières colonnes, le reste en extra_1, extra_2, ...
n_extra <- ncol(orig) - length(noms_orig)
setnames(orig, c(noms_orig, paste0("extra_", seq_len(n_extra))))

# Conversion des colonnes numériques + codes "manquant" -> NA
num_cols <- c("fico", "mi_pct", "n_units", "cltv", "dti",
              "orig_upb", "ltv", "orig_rate", "orig_term", "n_borrowers")
orig[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
orig[fico >= 9999, fico := NA]
orig[cltv >= 999,  cltv := NA]
orig[dti  >= 999,  dti  := NA]
orig[ltv  >= 999,  ltv  := NA]
orig[mi_pct >= 999, mi_pct := NA]
orig[n_units >= 99, n_units := NA]
orig[n_borrowers >= 99, n_borrowers := NA]

# --- Contrôles de cohérence (si l'un échoue, le mapping des colonnes est faux)
stopifnot(
  orig[!is.na(fico), all(fico >= 300 & fico <= 850)],
  orig[!is.na(dti),  all(dti > 0 & dti <= 65)],
  orig[!is.na(ltv),  all(ltv > 0 & ltv <= 250)],  # 20 prêts LTV 106-222 : rares mais réels, conservés
  orig[, uniqueN(loan_id) == .N]            # loan_id est bien une clé unique
)
cat("Contrôles origination OK : FICO, DTI, LTV dans les bornes attendues\n\n")

# ----------------------------------------------------------------------------
# 2. FICHIER PERFORMANCE (234 Mo : on ne lit que les colonnes utiles)
# ----------------------------------------------------------------------------
f_perf <- "data/raw/sample_perf_2018.txt"

# Nombre de colonnes du fichier (lu sur la 1re ligne)
n_col_perf <- length(strsplit(readLines(f_perf, n = 1), "\\|")[[1]])
cat("Performance :", n_col_perf, "colonnes détectées — lecture des 12 premières\n")

# Layout SFLLD : les 6 premières colonnes sont stables :
#   1 loan_id | 2 period (AAAAMM) | 3 current_upb | 4 dlq_status
#   5 loan_age | 6 rem_months
# Le code "zero balance" (motif de sortie : remboursement anticipé, défaut...)
# se trouve un peu plus loin ; sa position variant selon les versions du
# layout, on le détecte par son contenu (codes à 2 chiffres : 01, 02, 03...).
perf <- fread(f_perf, sep = "|", header = FALSE, quote = "",
              select = 1:12, colClasses = "character",
              na.strings = c("", "NA"))
setnames(perf, c("loan_id", "period", "current_upb", "dlq_status",
                 "loan_age", "rem_months", paste0("v", 7:12)))

# --- Détection de la colonne zero-balance code parmi v7:v12
zb_codes <- c("01", "02", "03", "06", "09", "15", "16", "96", "98")
cand <- paste0("v", 7:12)
is_zb <- sapply(cand, function(cl) {
  vals <- unique(na.omit(perf[[cl]]))
  length(vals) > 0 && all(vals %in% zb_codes)
})
cat("Détection zero-balance code :", paste(cand, is_zb, collapse = " | "), "\n")
if (sum(is_zb) == 1) {
  setnames(perf, cand[is_zb], "zb_code")
  cat("-> zero-balance code =", cand[is_zb], "\n\n")
} else {
  perf[, zb_code := NA_character_]
  cat("ATTENTION : zero-balance code non identifié sans ambiguïté.\n",
      "Valeurs uniques des colonnes candidates (à m'envoyer) :\n")
  for (cl in cand) {
    v <- unique(na.omit(perf[[cl]]))
    cat("  ", cl, ":", paste(head(v, 15), collapse = ", "), "\n")
  }
}

# --- Statut d'impayé -> numérique
# dlq_status : "00" = à jour, "01" = 30j, "02" = 60j, "03" = 90j, ...,
#              "RA" = REO (saisie) -> assimilé au défaut
perf[, dlq_num := suppressWarnings(as.integer(dlq_status))]
perf[dlq_status == "RA", dlq_num := 99L]
perf[, loan_age := as.integer(loan_age)]

# ----------------------------------------------------------------------------
# 3. CONSTRUCTION DE LA TABLE PRÊT (1 ligne par prêt)
# ----------------------------------------------------------------------------
# Définition du défaut (standard bancaire / bâlois) : 90 jours d'impayés et +
setorder(perf, loan_id, period)

# Pour chaque prêt : on repère les mois en défaut (D90+) et on gère
# explicitement le cas "aucun défaut" (min() sur un ensemble vide renverrait
# Inf, un double, et casserait la cohérence de types entre groupes).
loans_perf <- perf[, {
  idx_def <- which(dlq_num >= 3L)
  .(n_obs       = .N,
    last_age    = if (all(is.na(loan_age))) NA_integer_
                  else max(loan_age, na.rm = TRUE),
    default_90  = as.integer(length(idx_def) > 0L),
    age_default = if (length(idx_def)) min(loan_age[idx_def]) else NA_integer_,
    max_dlq     = if (all(is.na(dlq_num))) NA_integer_
                  else max(dlq_num, na.rm = TRUE),
    zb_last     = if (all(is.na(zb_code))) NA_character_
                  else last(na.omit(zb_code)))
}, by = loan_id]

# Issue du prêt (mutuellement exclusif, le défaut prime)
loans_perf[, outcome := fifelse(default_90 == 1L, "default",
                        fifelse(!is.na(zb_last) & zb_last == "01", "prepaid",
                                "active_censored"))]

# Durée pour l'analyse de survie : âge au défaut, sinon âge à la dernière obs.
loans_perf[, time_event := fifelse(default_90 == 1L, age_default, last_age)]

# Jointure avec l'origination
loans <- merge(orig, loans_perf, by = "loan_id", all.x = TRUE)
cat("Prêts sans historique de performance :", loans[is.na(n_obs), .N], "\n")
loans <- loans[!is.na(n_obs)]

# ----------------------------------------------------------------------------
# 4. PANEL ALLÉGÉ POUR TRANSITIONS & SURVIE
# ----------------------------------------------------------------------------
# États retenus : C (à jour), D30, D60, D90+ (absorbant), P (remboursé, absorbant)
perf_states <- perf[, .(loan_id, period, loan_age, dlq_num, zb_code)]
perf_states[, state := fcase(
  !is.na(zb_code) & zb_code == "01", "P",
  dlq_num >= 3L,                     "D90+",
  dlq_num == 2L,                     "D60",
  dlq_num == 1L,                     "D30",
  default = "C"
)]

# ----------------------------------------------------------------------------
# 5. SAUVEGARDES ET SYNTHÈSE
# ----------------------------------------------------------------------------
saveRDS(loans,       "data/loans.rds")
saveRDS(perf_states, "data/perf_states.rds")

cat("\n========== SYNTHÈSE ==========\n")
cat("Prêts analysés        :", nrow(loans), "\n")
cat("Taux de défaut (D90+) :",
    sprintf("%.2f%%", 100 * mean(loans$default_90)), "\n")
print(loans[, .N, by = outcome][order(-N)])
cat("\nFICO  — min/médiane/max :",
    paste(round(quantile(loans$fico, c(0, .5, 1), na.rm = TRUE)), collapse = " / "), "\n")
cat("DTI   — min/médiane/max :",
    paste(round(quantile(loans$dti,  c(0, .5, 1), na.rm = TRUE)), collapse = " / "), "\n")
cat("LTV   — min/médiane/max :",
    paste(round(quantile(loans$ltv,  c(0, .5, 1), na.rm = TRUE)), collapse = " / "), "\n")
cat("Période de performance :", min(perf$period), "->", max(perf$period), "\n")
cat("Durée du script :", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("Fichiers créés : data/loans.rds, data/perf_states.rds\n")
