#' Shapefile results from raw HFP data
#' @description User interface for using functions tidy_hfp() and link_stats()
#'
#' @param data_path pathname of original data files, "" by default (graphical interface)
#' @param tidy_path pathname where to write tidy HFP files, "" by default (graphical interface)
#' @param result_path pathname where to write result shapefile, "" by default (graphical interface)
#' @param routes_to_keep character vector of selected routes, "" by default
#' @param start_time starting hour, 6 by default
#' @param end_time ending hour (for example 16 => 16:59 last accepted), 19 by default
#' @param tp_name character suffix to result file variables, "vrk" by default
#' @param links_shp filename of links data in data_path, "links.shp" by default
#' @param result_filename filename of result shapefile in result_path, "links_times.shp" by default
#' @param links_routes filename of route data in data_path, "links_routes.csv" by default
#'
#' @examples
#' run_all_hfp()
#' run_all_hfp(routes_to_keep = c("1001", "1002"), start_time = 7, end_time = 9, links_shp = "example.shp")
#'
run_all_hfp <- function(data_path = "", tidy_path = "", result_path = "", routes_to_keep = "", start_time = 6, end_time = 19,
                        tp_name = "vrk", links_shp = "links.shp", result_filename = "links_times.shp", links_routes = "links_routes.csv") {

  # choose folders
  if (data_path == "") {
    data_path <- choose.dir(caption = "Choose directory of data files")
  }
  if (tidy_path == "") {
    tidy_path <- choose.dir(caption = "Choose directory of tidy files")
  }
  if (result_path == "") {
    result_path <- choose.dir(caption = "Choose directory of result files")
  }
  # Tidy data
  tidy_hfp(data_path = data_path, tidy_path = tidy_path, routes_to_keep = routes_to_keep,
           links_shp = links_shp, links_routes = links_routes)

  # Statistics
  link_stats(data_path = data_path, tidy_path = tidy_path, result_path = result_path, start_time = start_time, end_time = end_time,
             tp_name = tp_name, links_shp = links_shp, result_filename = result_filename)

}
