
library(ggplot2)
library(ssdtools)
library(dplyr)
library(purrr)

gof_dat <- flodata |>
  tidyr::unnest(gof) |>
  select(station_id, data, nyear,fit, weight, dist)

hc_multi_dat <-  flodata |>
  tidyr::unnest(hc_multi) |>
  select(station_id, proportion, multi_est = est)

hc_arithmetic_dat <-  flodata |>
  tidyr::unnest(hc_arithmetic) |>
  select(station_id, proportion, aritmetic_est = est)


hc_plot <-
  full_join(hc_multi_dat, hc_arithmetic_dat) |>
  mutate(log_ratio = aritmetic_est/multi_est)
  ggplot(aes(x=station_id, y=log_ratio)) +
    geom_bar(stat="identity") +
    facet_wrap(~proportion) +
    scale_y_log10() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(hc_plot, file = "log_ratio_by_station_ID.png", height = 6, width = 7)


hist_plot <- flodata |>
 tidyr::unnest(data) |>
  ggplot(aes(x=flow)) +
  geom_histogram() +
  facet_wrap(~station_id, scales = "free_y") +
  scale_x_log10()
ggsave(hist_plot, file = "histograms_by_station_ID.png", height = 6, width = 7)


conc_dat <- flodata |>
  dplyr::select(station_id, fit, hc_arithmetic) |>
  tidyr::unnest(hc_arithmetic) |>
  dplyr::select(station_id,
                theoretical_proportion = proportion,
                conc = est) |>
  split(~station_id)

flodata |>
  unnest()

back_calc_dat <- pmap(list(flodata$fit, conc_dat, flodata$station_id), function(x, y, z){
   ssd_hp(x, conc=y$conc, multi_est = FALSE) |>
    dplyr::select(conc, est) |>
    mutate(station_id = z,
           returned_proportion = est/100)
}) |>
  bind_rows() |>
  left_join(bind_rows(conc_dat)) |>
  mutate(event_scale = ifelse(theoretical_proportion == 0.99,100,
                              ifelse(theoretical_proportion == 0.995,200,500)),
         extremeness = (1-returned_proportion)*event_scale)










### below doesn;t wsork keep for learning

fit_dat <- flodata |>
  select(data) |>
  split(~station_id) |>
  map(.f = function(x){x["data"]}) |>
  map(.f = function(x){ssd_fit_dists(x, left="flow")})

fit_dat <- flodata |>
  select(fit) |>
  split(~station_id) |>
  map(.f = function(x){x["fit"]})


   (ssdtools::ssd_fit_dists(data, left="flow")

# try to extract fits, I give up

fit_dat <- flodata |>
  group_by(station_id) |>
  select(fit) |>
  #unlist(recursive = FALSE)
  split(~station_id)

map(fit_dat, autoplot)
river_fits <- lapply(fit_dat, FUN = function(x) {
  fit <- x |>
    select(fit) |>
    unlist(recursive = FALSE)
  #autoplot(fit)
  })

lapply(river_fits, autoplot)


fit_dat |>
  dplyr::left_join(conc_dat) |>
  mutate(hp = ssd_hp(.data$fit, conc=1))



