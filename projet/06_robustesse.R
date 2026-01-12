pdata$log_cong <- log1p(pdata$congestion_dep_day)

mod_fe_log <- plm(
  duration_mean ~ log_cong + nb_flights_route_day + dow + month,
  data = pdata, model = "within", effect = "individual"
)
coeftest(mod_fe_log, vcov = vcovHC(mod_fe_log, type="HC1", cluster="group"))


######


library(dplyr)

pdata$qc <- factor(dplyr::ntile(as.numeric(pdata$congestion_dep_day), 5))

mod_fe_q <- plm(
  duration_mean ~ qc + nb_flights_route_day + dow + month,
  data = pdata, model = "within", effect = "individual"
)
coeftest(mod_fe_q, vcov = vcovHC(mod_fe_q, type="HC1", cluster="group"))
