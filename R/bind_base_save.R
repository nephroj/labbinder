#' Bind, extract, and save laboratory data
#'
#' Bind all labs, extract baseline data, and save to CSV files.
#' @param hospital Select the hospital: seoul, bucheon, or cheonan.
#' @param ptlist_path Patient list file which included "id" and "orderdate" column.
#' @param lab_dir_path Directory path included excel files.
#' @param savefile_name The name of output file name.
#' @param basic_info_path Path of information file.
#' @param clean Make a cleaned data or not.
#' @param base_window_period Window period for baseline laboratory data (months).
#' @keywords bind_base_save
#' @export
#' @examples
#' bind_base_save(
#'   hospital = "seoul",
#'   ptlist_path = "lab/seoul_remain.xlsx",
#'   lab_dir_path = "lab/seoul_remain",
#'   savefile_name = lab_dir_path,
#'   basic_info_path = NULL,
#'   clean = TRUE,
#'   base_window_period = 4
#' )
bind_base_save = function(
  hospital,
  ptlist_path,
  lab_dir_path,
  basic_info_path = NULL,
  clean = TRUE,
  base_window_period = 4,
  savefile_name
) {

  comdt = lab_binder(
    hospital = hospital,
    ptlist_path = ptlist_path,
    lab_dir_path = lab_dir_path,
    basic_info_path = basic_info_path,
    clean = clean
  )

  basedt = extract_base(
    data = comdt,
    base_window_period = base_window_period
  )

  # save to CSV files
  save_file_all = paste0(savefile_name, "_all.csv")
  save_file_base = paste0(savefile_name, "_base.csv")
  write.csv(comdt, save_file_all, na="", row.names=F, fileEncoding="CP949")
  write.csv(basedt, save_file_base, na="", row.names=F, fileEncoding="CP949")
  cat('# All laboratory data are saved to "', save_file_all,'"\n', sep="")
  cat('# Baseline laboratory data are saved to "', save_file_base,'"\n', sep="")
}
