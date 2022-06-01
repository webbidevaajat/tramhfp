# function definitions ---

#' Clean HFP data files
#' @description Removes outliers and selects variables needed in analysis
#'
#' @param hfp_temp DATAFRAME that contains HFP 2.0 data
#' @param stop_times BOOLEAN true if stop times are kept in data
#' @return cleaned dataframe
#'
clean_hfp_sf <- function(hfp_temp, stop_times = TRUE){
  # start
  message("Dimensions HFP before cleaning: ", dim(hfp_temp)[1])

  # select variables ----
  hfp_temp <- hfp_temp %>%
    dplyr::select(
      route,
      # The ID of the route the vehicle is running on. This matches route_id in GTFS.
      dir,
      # The line direction of the trip, either 1 or 2. Value 1 here is same as 0 in GTFS and the Digitransit API.
      start,
      # The scheduled start time of the trip, i.e. the scheduled departure time from the first stop of the trip. The format follows %H:%M in 24-hour local time.
      tst,
      # UTC timestamp from the vehicle in ISO 8601 format as output by date --utc "+%Y-%m-%dT%H:%M:%SZ"
      lat,
      # WGS 84 latitude in degrees. null if there is no GPS fix.
      long,
      # WGS 84 longitude in degrees. null if there is no GPS fix.
      drst,
      # Door status. 0 if all the doors are closed, 1 if any of the doors are open.
      oday,
      # Operating day of the trip.
      stop,
      # ID of the stop related to the event. NA if not at any stop.
      spd
      # Speed of the vehicle, in meters per second (m/s).
    )

  # filter out points if location is unavailable ----
  hfp_temp <- hfp_temp %>%
    tidyr::drop_na(lat, long)

  # filter only weekdays ----
  hfp_temp <- hfp_temp %>%
    dplyr::filter(lubridate::wday(oday, week_start = 1) < 6)

  if (nrow(hfp_temp) == 0){
    message("Dimensions HFP after cleaning: ", dim(hfp_temp)[1])
    return(hfp_temp)
  }

  # filter points only after the start_time ----
  hfp_temp <- hfp_temp %>%
    dplyr::mutate(current_time = tst) %>%
    dplyr::select(-tst)

  hfp_temp <- hfp_temp %>%
    dplyr::mutate(start_time_date = lubridate::ymd_hms(paste(oday, start), tz = "Europe/Helsinki"))

  hfp_temp <- hfp_temp %>%
    dplyr::filter(current_time > start_time_date)

  # clean stop_times if
  if (stop_times == FALSE){
    hfp_temp <- hfp_temp %>%
      dplyr::filter(drst == 0)
  }

  message("Dimensions HFP after cleaning: ", dim(hfp_temp)[1])
  return(hfp_temp)
}


#' Tidy HFP data files from raw data
#' @description User interface for using functions clean_hfp_sf() and hfp_to_links_run()
#'
#' @param data_path pathname of original data files, "" by default (graphical interface)
#' @param tidy_path pathname where to write tidy HFP files, "" by default (graphical interface)
#' @param routes_to_keep character vector of selected routes, "" by default
#' @param links_shp filename of links data in data_path, "links.shp" by default
#' @param links_routes filename of route data in data_path, "links_routes.csv" by default
#' @examples
#' tidy_hfp()
#' tidy_hfp(routes_to_keep = c("1001", "1002"), links_shp = "example.shp")
#'
tidy_hfp <- function(data_path = "", tidy_path = "", routes_to_keep = "", links_shp = "links.shp", links_routes = "links_routes.csv") {
  # choose folders
  if (data_path == "") {
    data_path <- choose.dir(caption = "Choose directory of data files")
  }
  if (is.na(data_path)) {
    stop("Error: missing parameter (data_path)!")
  }
  if (tidy_path == "") {
    tidy_path <- choose.dir(caption = "Choose directory of tidy files")
  }
  if (is.na(tidy_path)) {
    stop("Error: missing parameter (tidy_path!)")
  }
  #read link files
  message("Reading links.shp")
  links_path <- file.path(data_path, links_shp)
  if (!file.exists(links_path)) {
    stop(paste0(links_path, " doesn't exist!"))
  }
  links <- sf::st_read(links_path)
  message("Reading links_routes.csv")
  routes_path <- file.path(data_path, links_routes)
  if (!file.exists(routes_path)) {
    stop(paste0(routes_path, " doesn't exist!"))
  }
  links_routes <- readr::read_csv(routes_path)
  message("Starting the tidying process")

  # loop over hfp data files
  data_files <- dir(data_path, pattern = ".rds")
  first_read <- TRUE
  message("Files to read: ", paste(data_files, collapse = ", "))
  for (j in data_files){
    df <- readr::read_rds(file.path(data_path, j))
    hfp <- tibble::as_tibble(df)
    tryCatch(
      {
        hfp %>% dplyr::select(route, dir, start, tst, lat, long, drst, oday, stop, spd)
      },
      error=function(cond) {
        message("Error: missing columns in HFP data! (", j,")")
        message(cond)
        return(NA)
      }
    )
    hfp <- clean_hfp_sf(hfp)

    # store all data to one tibble
    # create new tibble if first loop, else bind to existing
    if (first_read){
      data <- hfp
      first_read <- FALSE
    } else {
      data <- dplyr::bind_rows(data, hfp)
    }
    message(basename(j), " is ready")
  }
  if (routes_to_keep != "") {
    if (!any(routes_to_keep %in% data$route)) {
      stop(paste0("Routes ", routes_to_keep, " not in data!"))
    }
    data <- data %>% dplyr::filter(route %in% routes_to_keep)
  }
  #link id to hfp
  message("Link id to HFP starting")
  data <- hfp_to_links_run(data, links, links_routes)
  message("Link id to HFP ready")

  #write file for each route
  for (i in unique(data$route)) {
    data %>% dplyr::filter(route == i) %>% readr::write_rds(file.path(tidy_path, paste0(i, '_tidy.rds')))
  }

  message("Finished with all files!")
}
