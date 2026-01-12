library(dplyr)
library(stringr)
library(lubridate)

flights_clean <- flights_raw %>%
  
  # 1) Périmètre géographique (exogène)
  filter(
    str_detect(adep, "^[ELU]"),
    str_detect(ades, "^[ELU]")
  ) %>%
  
  # 2) Données valides
  filter(
    !is.na(first_seen),
    !is.na(last_seen),
    last_seen > first_seen
  ) %>%
  
  # 3) Variables clés
  mutate(
    date = as.Date(dof),
    duration_min = as.numeric(difftime(last_seen, first_seen, units = "mins")),
    route = paste(adep, ades, sep = "-")
  ) #%>%
  
  # # 4) Nettoyage léger (technique)
  # filter(
  #   duration_min >= 5,
  #   duration_min <= 700
  # )

cat("✓ Vols après nettoyage :", nrow(flights_clean), "\n")
