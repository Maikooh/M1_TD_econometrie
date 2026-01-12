# ============================================================
# PANEL ÉCONOMÉTRIE (OPDI) — Poolé / FE / RE / Two-way FE
# + SE robustes clusterisées par route
# + Tests LM (BP) & Hausman
# + Tableau de résultats (texreg)
# ============================================================

# ---- Packages ----
pkgs <- c("dplyr", "lubridate", "plm", "lmtest", "sandwich", "texreg")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(lubridate)
library(plm)
library(lmtest)
library(sandwich)
library(texreg)

# ---- Charger le panel (à adapter si besoin) ----
# Option 1 : si panel_route_day est déjà en mémoire, le bloc ne le modifie pas.
# Option 2 : sinon, charge-le depuis le RDS.
if (!exists("panel_route_day")) {
  panel_path <- "./data/opdi/panel_route_day_final.rds"
  if (!file.exists(panel_path)) {
    stop("panel_route_day introuvable en mémoire et fichier RDS non trouvé : ", panel_path)
  }
  panel_route_day <- readRDS(panel_path)
}

# ---- Contrôles calendrier ----
panel_route_day <- panel_route_day %>%
  mutate(
    date = as.Date(date),
    dow = factor(wday(date, label = TRUE, week_start = 1)),
    month = factor(month(date, label = TRUE))
  )

# ---- pdata.frame ----
pdata <- pdata.frame(panel_route_day, index = c("route", "date"))
cat("\n--- Dimensions du panel (pdim) ---\n")
print(pdim(pdata))

# ---- Modèles ----
# (1) Poolé
mod_pool <- plm(
  duration_mean ~ congestion_dep_day + nb_flights_route_day + dow + month,
  data = pdata,
  model = "pooling"
)

# (2) Effets fixes route (within)
mod_fe <- plm(
  duration_mean ~ congestion_dep_day + nb_flights_route_day + dow + month,
  data = pdata,
  model = "within",
  effect = "individual"
)

# (3) Effets aléatoires
mod_re <- plm(
  duration_mean ~ congestion_dep_day + nb_flights_route_day + dow + month,
  data = pdata,
  model = "random",
  effect = "individual"
)

# (4) Two-way FE (route + date)
# Ici on n'inclut pas dow/month car les FE temps absorbent les effets communs par date.
mod_fe_tw <- plm(
  duration_mean ~ congestion_dep_day + nb_flights_route_day,
  data = pdata,
  model = "within",
  effect = "twoways"
)

# ---- SE robustes clusterisées par route ----
vcov_cluster_route <- function(model) vcovHC(model, type = "HC1", cluster = "group")

cat("\n--- Coefficients (SE robustes cluster route) ---\n")
ct_pool  <- coeftest(mod_pool,  vcov = vcov_cluster_route(mod_pool))
ct_fe    <- coeftest(mod_fe,    vcov = vcov_cluster_route(mod_fe))
ct_re    <- coeftest(mod_re,    vcov = vcov_cluster_route(mod_re))
ct_fe_tw <- coeftest(mod_fe_tw, vcov = vcov_cluster_route(mod_fe_tw))

cat("\n[POOLÉ]\n");  print(ct_pool)
cat("\n[FE route]\n");  print(ct_fe)
cat("\n[RE]\n");  print(ct_re)
cat("\n[FE two-way route+date]\n");  print(ct_fe_tw)

# ---- Tests de spécification ----
cat("\n--- Test LM Breusch-Pagan : poolé vs RE ---\n")
print(plmtest(mod_pool, type = "bp"))

cat("\n--- Test de Hausman : FE vs RE ---\n")
print(phtest(mod_fe, mod_re))

# # ---- Tableau de résultats (console) ----
# cat("\n--- Tableau comparatif (texreg::screenreg) ---\n")
# screenreg(
#   list(mod_pool, mod_fe, mod_re, mod_fe_tw),
#   vcov = list(
#     vcov_cluster_route(mod_pool),
#     vcov_cluster_route(mod_fe),
#     vcov_cluster_route(mod_re),
#     vcov_cluster_route(mod_fe_tw)
#   ),
#   custom.model.names = c("Poolé", "FE (route)", "RE", "FE 2-voies (route+date)")
# )

# ---- Export LaTeX (optionnel, utile pour Rmd PDF) ----
# Décommente si tu veux générer un fichier .tex à inclure dans ton rapport.
# tex_out <- "./data/opdi/table_panel_results.tex"
# texreg(
#   list(mod_pool, mod_fe, mod_re, mod_fe_tw),
#   vcov = list(
#     vcov_cluster_route(mod_pool),
#     vcov_cluster_route(mod_fe),
#     vcov_cluster_route(mod_re),
#     vcov_cluster_route(mod_fe_tw)
#   ),
#   custom.model.names = c("Poolé", "FE (route)", "RE", "FE 2-voies (route+date)"),
#   booktabs = TRUE,
#   caption = "Effet de la congestion au départ sur la durée moyenne du vol",
#   label = "tab:panel_results",
#   file = tex_out
# )
# cat("\n✓ Tableau LaTeX écrit dans : ", tex_out, "\n", sep = "")

# ---- Extra : interprétation “par 10 vols” (optionnel) ----
# Extraction du coefficient congestion_dep_day (si présent)
get_beta <- function(ct, var = "congestion_dep_day") {
  if (!var %in% rownames(ct)) return(NA_real_)
  unname(ct[var, 1])
}

beta_fe    <- get_beta(ct_fe)
beta_fe_tw <- get_beta(ct_fe_tw)

cat("\n--- Interprétation (approx.) ---\n")
cat("β_FE (congestion)    =", beta_fe, "\n")
cat("β_FE_tw (congestion) =", beta_fe_tw, "\n")
if (!is.na(beta_fe)) {
  cat("Effet FE : +10 départs =>", 10 * beta_fe, "minutes (ceteris paribus)\n")
}
if (!is.na(beta_fe_tw)) {
  cat("Effet FE two-way : +10 départs =>", 10 * beta_fe_tw, "minutes (ceteris paribus)\n")
}
