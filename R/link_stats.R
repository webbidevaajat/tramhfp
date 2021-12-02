
# filter based on IQR of time ----
check_filter <- function(temp){
  temp <- temp %>%
    dplyr::mutate(median_s = median(dif_s),
           q1_s = quantile(dif_s, 0.25),
           q3_s = quantile(dif_s, 0.75),
           iqr_s = IQR(dif_s)
    ) %>%
    dplyr::filter(dif_s > q1_s - 1.5 * iqr_s) %>%
    dplyr::filter(dif_s < q3_s + 1.5 * iqr_s)

  temp <- temp %>%
    dplyr::select(-median_s, -q1_s, -q3_s, -iqr_s)

  return(temp)
}


link_stats <- function(data_path = "", tidy_path = "", result_path = "", start_time = 6, end_time = 19,
                       tp_name = "vrk", links_shp = "links.shp", links_times_shp = "links_times.shp") {

  if (data_path == "") {
    data_path <- choose.dir(caption = "Choose directory of original data files")
  }
  if (tidy_path == "") {
    tidy_path <- choose.dir(caption = "Choose directory of tidy files")
  }
  if (result_path == "") {
    result_path <- choose.dir(caption = "Choose directory of result files")
  }

  # load  ---
  hfp_files <- dir(tidy_path, pattern = ".rds")

  links_path <- file.path(data_path, links_shp)
  links <- sf::st_read(links_path)

  # read in hfp files
  hfp <- hfp_files %>%
    purrr::map( ~ readr::read_rds(file.path(tidy_path, .))) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    tibble::as_tibble()

  hfp<- hfp %>%
    dplyr::select(link_id, oday, start_time_date, route, dir, current_time, lat, long)

  # filter time ----
  hfp <- hfp %>%
    dplyr::filter(lubridate::hour(current_time) %in% start_time:end_time)


  # sums on links and statistics ---
  hfp_sums <- hfp %>%
    dplyr::group_by(link_id, oday, start_time_date) %>%
    dplyr::summarise(
      maxtst = max(current_time),
      mintst = min(current_time),
      no_points = length(route)
    ) %>%
    dplyr::filter(no_points > 1) %>%
    dplyr::mutate(
      dif_secs = difftime(maxtst, mintst, units = "secs"),
      dif_s = as.numeric( difftime(maxtst, mintst, units = "secs") ),
      dif_h = as.numeric( difftime(maxtst, mintst, units = "secs") ) / 3600
    ) %>%
    dplyr::ungroup()

  # attach link length and calc speeds ----
  links <- links %>%
    dplyr::mutate(length_km = as.numeric(sf::st_length(.))/1000)

  hfp_sums <- hfp_sums %>%
    dplyr::left_join(links %>% sf::st_drop_geometry())

  hfp_sums <- hfp_sums %>%
    dplyr::mutate(spd_kmh = length_km / dif_h)

  # filter out abnormal speeds
  hfp_sums <- hfp_sums %>%
    dplyr::filter(spd_kmh > 1 & spd_kmh < 80)

  # filter based on IQR of time

  hfp_sums <- hfp_sums %>%
    dplyr::group_by(link_id) %>%
    dplyr::group_modify(~ check_filter(.x)) %>%
    dplyr::ungroup()

  #' JOIN TO SPATIAL FILE
  #'

  mn0 <- paste0("mn_", tp_name)
  md0 <- paste0("md_", tp_name)
  sd0 <- paste0("sd_", tp_name)
  vc0 <- paste0("vc_", tp_name)
  min0 <- paste0("min_", tp_name)
  max0 <- paste0("max_", tp_name)
  n0 <- paste0("n_", tp_name)
  l0 <- paste0("l_", tp_name)
  del0 <- paste0("del_", tp_name)

  # sums to links day
  id_stats <- hfp_sums %>%
    dplyr::group_by(link_id) %>%
    dplyr::summarise(
      !!mn0 := mean(spd_kmh, na.rm = TRUE),
      !!md0 := median(spd_kmh, na.rm = TRUE),
      !!sd0 := sd(spd_kmh, na.rm = TRUE),
      !!vc0 := !!rlang::sym(sd0) / !!rlang::sym(mn0),
      !!min0 := quantile(spd_kmh, 0.025, na.rm = TRUE),
      !!max0 := quantile(spd_kmh, 0.975, na.rm = TRUE),
      !!n0 := length(spd_kmh),
      !!l0 := mean(length_km, na.rm = TRUE),
      !!del0 :=  -3600*(!!rlang::sym(l0)/!!rlang::sym(max0)-!!rlang::sym(l0)/!!rlang::sym(md0))
    )

  id_stats_meta<- hfp_sums %>%
    dplyr::group_by(link_id) %>%
    dplyr::summarise(
      begin = min(oday),
      end = max(oday)
    )

  links <- links %>%
    dplyr::left_join(id_stats, by = c("link_id" = "link_id")) %>%
    dplyr::left_join(id_stats_meta, by = c("link_id" = "link_id"))


  # write to file ----
  path_to_write <- file.path(result_path, links_times_shp)
  if (file.exists(path_to_write)) {
    links %>%
      sf::st_write(
        file.path(result_path, links_times_shp),
        delete_dsn = TRUE)
  } else {
    links %>%
      sf::st_write(
        file.path(result_path, links_times_shp))
  }

}
