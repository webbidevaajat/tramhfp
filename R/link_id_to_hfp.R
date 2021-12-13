# points to lines operations---

#' Attach link id to HFP rows
#'
#' @param hfp_temp HFP dataframe
#' @param links_temp
#' @param oday_temp
#' @param dir_temp
#'
#' @return HFP dataframe
#'
#' @examples
hfp_sf_operations<- function(hfp_temp, links_temp, links_routes_temp, oday_temp, dir_temp){

  # filter hfp of selected operating day and direction
  hfp_temp <- hfp_temp %>%
    dplyr::filter(dir == dir_temp) %>%
    dplyr::filter(oday == oday_temp)

  # keep only links that are in hfp routes and directions
  links_routes_temp <- links_routes_temp %>%
    dplyr::filter(dir_id == dir_temp) %>%
    dplyr::filter(route_id %in% unique(hfp_temp$route))

  links_temp <- links_temp %>%
    dplyr::filter(link_id %in% links_routes_temp$link_id)

  # spatial hfp ----

  # convert links to same crs
  links_temp <- links_temp %>%
    st_transform(3132)

  # convert to spatial data
  hfp_temp <- hfp_temp %>%
    drop_na("lat", "long") %>%
    st_as_sf(coords = c("long", "lat")) %>%
    st_set_crs(4326) %>%
    st_transform(3132)

  # clean hfp points to far away from lines
  hfp_temp <- hfp_temp[sf::st_buffer(links_temp, 30), ]

  # nearest points d1 and d2
  index <- sf::st_nearest_feature(hfp_temp, links_temp)
  ids <- links_temp %>%
    dplyr::slice(index) %>%
    dplyr::select("link_id")
  sf::st_geometry(ids) <- NULL
  #print(ids)
  hfp_temp$link_id <- ids$link_id

  sf::st_geometry(hfp_temp) <- NULL

  return(hfp_temp)
}

# run multicore points to lines ----

#' Run points to links operations using multiple cores
#' Send separate day to each core
#'
#' @param hfp_temp DATAFRAME which contains cleaned HFP data file
#' @param links_temp SPATIAL DATAFRAME which contains links and link_id field
#' @param links_routes_temp DATAFRAME which has fields route_id, link_id, dir_id and stop_seq
#'
#' @return HFP dataframe that has link_id attached as new column
#'
hfp_to_links_run_mc <- function(hfp_temp, links_temp, links_routes_temp){

  # set direction_id same as in hfp
  if (min(links_routes_temp$dir_id) == 0){
    links_routes_temp <- links_routes_temp %>%
      dplyr::mutate(dir_id = dir_id + 1)
  }

  # check that directions is same
  links_routes_temp %>%
    assertr::verify(dir_id %in% hfp_temp$dir)

  # lapply hfp_sf_operations() to each operating day
  odays <- unique(hfp_temp$oday)

  message("Start hfp_sf_operations direction 1")
  d1 <- lapply(odays,
               hfp_sf_operations,
               hfp_temp = hfp_temp,
               links_temp = links_temp,
               links_routes_temp = links_routes_temp,
               dir_temp = 1
  ) %>%
    purrr::reduce(rbind)

  message("Start hfp_sf_operations direction 2")
  d2 <- lapply(odays,
               hfp_sf_operations,
               hfp_temp = hfp_temp,
               links_temp = links_temp,
               links_routes_temp = links_routes_temp,
               dir_temp = 2
  ) %>%
    purrr::reduce(rbind)

  # unite directions and save to tibble
  hfp_res <- rbind(d1, d2)

  return(hfp_res)
}



