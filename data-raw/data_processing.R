library(readr)
read_river_file <- function(f){
  dat <- readr::read_csv(f,skip = 3,
                          col_types = cols(
                            `Date and time` = col_datetime(format = "%H:%M:%S %d/%m/%Y"),
                            Mean = col_double(),
                            Quality...3 = col_double(),
                            Min = col_double(),
                            Quality...5 = col_double(),
                            Max = col_double(),
                            Quality...7 = col_double(),
                            Comments = col_character()))
  tt <- dat |>
    dplyr::filter(Quality...3!=Quality...5 | Quality...3!=Quality...7)
  if (nrow(tt)){warning("Dropped", nrow(dat), "rows with differing quality scores")}

  dat |>
    dplyr::filter(Quality...3==Quality...5 & Quality...3==Quality...7) |>
    dplyr::mutate(date=lubridate::date(`Date and time`)) |>
    dplyr::select(date, quality=Quality...7, max=Max, comments=Comments) |>
    dplyr::filter(!is.na(max))

}



list_files <- list.files("data-raw/river_data", full.names = TRUE)

# readr::spec(readr::read_csv(list_files[[1]], skip=3))

flodata <- list_files |>
  setNames(tools::file_path_sans_ext(basename(list_files))) |>
  purrr::map(.f = read_river_file) |>
  dplyr::bind_rows(.id = "station_id") |>
  dplyr::mutate(year = lubridate::year(date)) |>
  dplyr::group_by(station_id, year) |>
  dplyr::summarise(flow=max(max),
                   n=dplyr::n(), .groups = "drop") |>
  dplyr::filter(n %in% c(1, 365, 366)) |>
  # save as dataset
  dplyr::nest_by(station_id) |>
  dplyr::mutate(nyear=nrow(data),
                fit=list(ssdtools::ssd_fit_dists(data, left="flow")),
                hc_multi=list(ssdtools::ssd_hc(fit, proportion=c(1-1/100, 1-1/200, 1-1/500))),
                hc_arithmetic=list(ssdtools::ssd_hc(fit, proportion=c(1-1/100, 1-1/200, 1-1/500), multi_est = FALSE)),
                gof=list(ssdtools::ssd_gof(fit)))


usethis::use_data(flodata, overwrite=TRUE)


