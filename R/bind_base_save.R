#' Bind, extract, and save laboratory data
#'
#' Bind all labs, extract baseline data, and save to CSV files.
#' @param hospital Select the hospital: seoul, bucheon, or cheonan.
#' @param ptlist_file Patient list file which included "id" and "orderdate" column.
#' @param lab_dir_path Directory path included excel files.
#' @param savefile_name The name of output file name.
#' @param lab_info_file Path of information file.
#' @param clean Make a cleaned data or not.
#' @param base_window_period Window period for baseline laboratory data (months).
#' @keywords bind_base_save
#' @importFrom utils write.csv
#' @export
bind_base_save = function(
  hospital,
  ptlist_file,
  lab_dir_path,
  lab_info_file = NULL,
  clean = TRUE,
  base_window_period = 4,
  savefile_name
) {

  comdt = lab_binder(
    hospital = hospital,
    ptlist_file = ptlist_file,
    lab_dir_path = lab_dir_path,
    lab_info_file = lab_info_file,
    clean = F
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


  if (clean){
    comdt_clean = lab_cleaner(comdt)
    basedt_clean = extract_base(
      data = comdt_clean,
      base_window_period = base_window_period
    )
    # save to CSV files
    save_file_all_clean = paste0(savefile_name, "_all_clean.csv")
    save_file_base_clean = paste0(savefile_name, "_base_clean.csv")
    write.csv(comdt_clean, save_file_all_clean, na="", row.names=F, fileEncoding="CP949")
    write.csv(basedt_clean, save_file_base_clean, na="", row.names=F, fileEncoding="CP949")
  }

  cat('Data saving is done. \n->', dirname(save_file_all), '\n')
}
