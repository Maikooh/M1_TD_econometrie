# ==========================================
# Lecture + concaténation Flight List OPDI
# (Mai–Juillet 2023)
# ==========================================

library(arrow)
library(dplyr)
library(purrr)
library(stringr)

# Dossier où tu as téléchargé les fichiers
data_dir <- "./data/opdi/flight_list"

# Lister les fichiers parquet (triés)
files <- list.files(
  path = data_dir,
  pattern = "^flight_list_2023(05|06|07)\\.parquet$",
  full.names = TRUE
) |> sort()

stopifnot(length(files) > 0)

# Lecture + bind (robuste)
flights_raw <- files |>
  map(~ {
    message("Reading: ", basename(.x))
    read_parquet(.x)
  }) |>
  bind_rows()

# Harmonisation des noms de variables (au cas où)
names(flights_raw) <- str_replace_all(names(flights_raw), "\\s+", "_")

# Aperçu
cat("\n✓ Fichiers lus:", length(files), "\n")
cat("✓ Lignes totales :", nrow(flights_raw), "\n")
cat("✓ Colonnes       :", ncol(flights_raw), "\n\n")

# Afficher quelques colonnes clés si elles existent
key_cols <- intersect(
  c("flight_id", "ADEP", "ADES", "EOBT", "ATOT", "AOBT", "ALDT", "AIBT"),
  names(flights_raw)
)
print(key_cols)

# Sauvegarde en cache (recommandé)
saveRDS(flights_raw, file = "./data/opdi/flights_raw_202305_202307.rds")
cat("\n✓ Données concaténées sauvegardées : ./data/opdi/flights_raw_202305_202307.rds\n")
