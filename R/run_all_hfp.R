
run_all_hfp <- function(data_path = "", tidy_path = "", result_path = "", routes_to_keep = "", start_time = 6, end_time = 19,
                        tp_name = "vrk", links_shp = "links.shp", links_times_shp = "links_times.shp", links_routes = "links_routes.csv") {

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
  tidy_hfp(data_path = data_path, tidy_path = tidy_path, routes_to_keep = routes_to_keep, links_routes = links_routes)

  # Statistics
  link_stats(data_path = data_path, tidy_path = tidy_path, result_path = result_path, start_time = start_time, end_time = end_time,
             tp_name = tp_name, links_shp = links_shp, links_times_shp = links_times_shp)

}
