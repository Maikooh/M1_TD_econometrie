library(dplyr)

panel_route_day %>%
  summarise(
    nb_routes = n_distinct(route),
    nb_days   = n_distinct(date),
    obs       = n()
  )

# Distribution du nombre de jours par route
panel_route_day %>%
  count(route) %>%
  summarise(
    min_days = min(n),
    median_days = median(n),
    mean_days = mean(n),
    max_days = max(n)
  )
# panel déséquilibré, forte hétérogénéité du nombre de jours par route

panel_route_day %>%
  summarise(
    duration_mean_mean = mean(duration_mean),
    duration_mean_sd   = sd(duration_mean),
    duration_mean_min  = min(duration_mean),
    duration_mean_max  = max(duration_mean),
    
    congestion_mean = mean(congestion_dep_day),
    congestion_sd   = sd(congestion_dep_day)
  )


# durée moyenne

summary(panel_route_day$duration_mean)
quantile(panel_route_day$duration_mean, probs = c(.01,.05,.95,.99))


#decomposition between within

library(plm)

pdata <- pdata.frame(panel_route_day, index = c("route", "date"))

# Décomposition automatique
summary(pdata$duration_mean)
summary(pdata$congestion_dep_day)


#corrélation 

panel_route_day %>%
  select(duration_mean, congestion_dep_day, nb_flights_route_day) %>%
  cor(use = "complete.obs")

#graphique 
library(ggplot2)

panel_route_day %>%
  mutate(q_congestion = ntile(congestion_dep_day, 5)) %>%
  group_by(q_congestion) %>%
  summarise(duration_mean = mean(duration_mean)) %>%
  ggplot(aes(q_congestion, duration_mean)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Quintiles de congestion au départ",
    y = "Durée moyenne du vol (minutes)"
  )


panel_route_day %>%
  group_by(date) %>%
  summarise(
    duration_mean = mean(duration_mean),
    congestion = mean(congestion_dep_day)
  ) %>%
  ggplot(aes(date, duration_mean)) +
  geom_line() +
  labs(y = "Durée moyenne (minutes)", x = "Date")



# panel_route_day %>%
#   ggplot(aes(congestion_dep_day, duration_mean)) +
#   geom_point(alpha = 0.05) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(
#     x = "Congestion au départ (vols/jour)",
#     y = "Durée moyenne du vol (minutes)"
#   )
