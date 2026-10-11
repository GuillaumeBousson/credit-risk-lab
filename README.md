# credit-risk-lab

**Chaîne complète de modélisation du risque de crédit sur données réelles** — scoring, probabilité de défaut, validation de modèle, matrices de transition, analyse de survie, provisionnement et capital réglementaire.

> 🚧 Projet en cours de construction (octobre 2026). Ce README sera enrichi au fil des modules.

## Données

[Freddie Mac Single-Family Loan-Level Dataset](https://www.freddiemac.com/research/datasets/sf-loanlevel-dataset) — échantillon de ~50 000 prêts hypothécaires américains originés en 2018, avec suivi mensuel de la performance (impayés, défauts, remboursements anticipés) jusqu'en 2026.

Conformément aux conditions d'utilisation de Freddie Mac, **les données brutes ne sont pas redistribuées dans ce dépôt** (dossier `data/` exclu du versionnement). Pour reproduire les analyses : créer un compte gratuit sur le site de Freddie Mac, télécharger `sample_2018.zip` et l'extraire dans `data/raw/`.

## Pipeline

| Script | Contenu |
|---|---|
| `01_data_prep.R` | Lecture des fichiers bruts, définition du défaut (90+ jours d'impayés), construction de la table prêt et du panel mensuel |
| `02_exploration.R` | Analyse exploratoire : taux de défaut par segment (FICO, LTV, DTI...) |
| `03_scorecard_pd.R` | Scorecard PD : discrétisation WoE/IV, régression logistique, grille de score |
| `04_challenger_ml.R` | Modèle challenger (forêt aléatoire), comparaison champion/challenger |
| `05_validation.R` | Validation : AUC/Gini, KS, calibration, stabilité (PSI), backtesting |
| `06_transitions.R` | Matrices de transition des états d'impayés (approche cohorte) |
| `07_survie.R` | Analyse de survie : Kaplan-Meier, modèle de Cox (time-to-default) |
| `08_provisionnement.R` | Provisionnement attendu type IFRS 9 : PD × LGD × EAD, staging |
| `09_capital_bale.R` | Capital réglementaire : formule IRB, actifs pondérés (Bâle) |

Le rapport de synthèse (note de validation de modèle) est produit en R Markdown dans `rapport/`.

## Environnement

R ≥ 4.4 — packages : `data.table`, `tidyverse`, `scorecard`, `pROC`, `ranger`, `survival`, `survminer`, `rmarkdown`.

## Auteur

Guillaume Bousson — M2 Statistique et économétrie (Université de Strasbourg) · M1 Finance (IAE Lille)
