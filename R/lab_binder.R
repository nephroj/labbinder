#' lab_binder() Function
#'
#' This function tie multiple excel files containing laboratory data to one file.
#' @param hospital Select the hospital: seoul, bucheon, or cheonan.
#' @param ptlist_path Patient list file which included "id" and "orderdate" column.
#' @param lab_dir_path Directory path included excel files.
#' @param savefile_name The name of output file name.
#' @param basic_info_path Path of information file.
#' @param clean Make a cleaned data or not.
#' @keywords lab_binder
#' @export
#' @examples
#' lab_binder(
#'   hospital = "seoul",
#'   ptlist_path = "lab/seoul_remain.xlsx",
#'   lab_dir_path = "lab/seoul_remain",
#'   savefile_name = lab_dir_path,
#' )
lab_binder = function(
  hospital,
  ptlist_path,
  lab_dir_path,
  savefile_name,
  basic_info_path = NULL,
  clean = TRUE,
  return = FALSE
) {

  # Load data
  if (is.null(basic_info_path)){
    basic_info_dt = basic_info
  } else{
    basic_info_dt = read_excel(basic_info_path)
  }
  basic_info_hosp = basic_info_dt %>%
    select(!!as.name(hospital), sort1, vname)

  ptlist = read_excel(ptlist_path)
  lab_files = list.files(lab_dir_path)

  # Print the status of data collection
  cat("# Number of patients in list file:", NROW(ptlist), "\n")
  cat("# Number of excel files collected:", length(lab_files), "\n")


  ## ALL DATA
  comdt = data.frame()
  n_of_file = 0

  for (file_name in lab_files) {
    n_of_files = length(lab_files)
    n_of_file = n_of_file + 1
    file = file.path(lab_dir_path, file_name)
    patientid = str_extract(file_name, "\\d+")

    dt1 = suppressMessages(read_excel(file)) %>%
      select(!starts_with("..")) %>%
      select(-"참고치") %>%
      rename(
        !!as.name(hospital) := "검사항목",
        sort1 = "검체명"
      ) %>%
      mutate(
        sort1 = ifelse(str_detect(sort1, "Urine"), "urine", "blood")
      )

    select_first = function(vector) {
      vec_filter_NA = vector[!is.na(vector)]
      return(vec_filter_NA[1])
    }

    dt1_final = basic_info_hosp %>%
      left_join(dt1, by=c(hospital, "sort1")) %>%
      select(-!!as.name(hospital), -sort1) %>%   # remain vname only
      filter(!is.na(vname)) %>%                  # remove the vname not included in info-file
      group_by(vname) %>%
      summarise_all(select_first) %>%
      column_to_rownames("vname") %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var="labdate") %>%
      mutate(
        labdate = as.POSIXct(labdate),
        patientid = as.integer(patientid)
      ) %>%
      arrange(patientid, labdate)

    comdt = bind_rows(comdt, dt1_final)
    cat("[", n_of_file, "/", n_of_files, "]\n")
  }

  ## Arrange variable order
  comdt = comdt %>%
    select(c("patientid", "labdate", basic_info_hosp$vname))

  ## Clean data & Remove empty columns
  comdt_clean = comdt %>%
    lab_cleaner() %>%
    select_if(function(x){any(!is.na(x))})

  if (clean){
    save_file_name_all = paste0("output/", savefile_name, "_all_clean.csv")
    comdt_fin = comdt_clean
  } else{
    save_file_name_all = paste0("output/", savefile_name, "_all_orig.csv")
    comdt_fin = comdt
  }

  if (!return){
    write.csv(comdt_fin, save_file_name_all, na="", row.names=F, fileEncoding="CP949")
    cat("# All laboratory data are saved to", save_file_name_all, "\n")
  }



  ## BASE DATA
  basedt = ptlist %>%
    select(id, orderdate) %>%
    rename(patientid = id) %>%
    left_join(comdt_fin, by="patientid") %>%
    filter(labdate >= orderdate - days(120) & labdate <= orderdate + days(120)) %>%
    mutate(
      lab_diff = difftime(labdate, orderdate, units="days"),
      abs_lab_diff = abs(lab_diff)
    ) %>%
    arrange(patientid, orderdate, abs_lab_diff) %>%
    group_by(patientid, orderdate) %>%
    summarise_all(select_first) %>%
    select(-lab_diff, -abs_lab_diff)

  if (clean){
    save_file_name_base = paste0("output/", savefile_name, "_base_clean.csv")
  } else{
    save_file_name_base = paste0("output/", savefile_name, "_base_orig.csv")
  }

  if (!return){
    write.csv(basedt, save_file_name_base, na="", row.names=F, fileEncoding="CP949")
    cat("# Baseline laboratory data are saved to", save_file_name_base, "\n")
  } else{
    return(list(comdt_fin, basedt))
  }

}


