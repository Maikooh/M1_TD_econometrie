summary(flights_clean$duration_min)
quantile(flights_clean$duration_min, probs = c(.01,.05,.95,.99))

#Après filtrage géographique et nettoyage minimal, l’échantillon comprend 1 376 872 vols européens

#########################

# agregation congestion aeroport/jour

congestion_day <- flights_clean %>%
  group_by(adep, date) %>%
  summarise(
    congestion_dep_day = n(),
    .groups = "drop"
  )

summary(congestion_day$congestion_dep_day)
#Interprétation : nombre total de départs par aéroport et par jour.


##########################

#construction du panel route/jour
panel_route_day <- flights_clean %>%
  group_by(route, adep, ades, date) %>%
  summarise(
    duration_mean = mean(duration_min),
    nb_flights_route_day = n(),
    .groups = "drop"
  ) %>%
  left_join(congestion_day, by = c("adep", "date"))

cat("✓ Observations panel :", nrow(panel_route_day), "\n")

summary(panel_route_day$duration_mean)
summary(panel_route_day$congestion_dep_day)


#####
# filtrer les routes peu observés

panel_route_day <- panel_route_day %>%
  group_by(route) %>%
  filter(n() >= 20) %>%   # seuil standard M1
  ungroup()

cat("✓ Panel après filtre routes :", nrow(panel_route_day), "\n")

#Les routes observées moins de 20 jours sont exclues afin de garantir une variation temporelle suffisante pour l’identification des effets fixes.”


# Sauvegarde du panel final
saveRDS(panel_route_day, "./data/opdi/panel_route_day_final.rds")

# Pour mémoire
write.csv(
  panel_route_day,
  "./data/opdi/panel_route_day_final.csv",
  row.names = FALSE
)

cat("✓ Panel final sauvegardé\n")
