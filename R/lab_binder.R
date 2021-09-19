#' Lab excel files binder
#'
#' This function tie multiple excel files containing laboratory data to one file.
#' @param hospital Select the hospital: seoul, bucheon, or cheonan.
#' @param ptlist_path Patient list file which included "id" and "orderdate" column.
#' @param lab_dir_path Directory path included excel files.
#' @param basic_info_path Path of information file.
#' @param clean Make a cleaned data or not.
#' @keywords lab_binder
#' @export
#' @examples
#' lab_binder(
#'   hospital = "seoul",
#'   ptlist_path = "lab/seoul_remain.xlsx",
#'   lab_dir_path = "lab/seoul_remain",
#'   basic_info_path = NULL,
#'   clean = TRUE
#' )
lab_binder = function(
  hospital,
  ptlist_path,
  lab_dir_path,
  basic_info_path = NULL,
  clean = TRUE
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
  comdt_pre = data.frame()
  n_of_file = 0
  percent_cut = 10

  for (file_name in lab_files) {
    n_of_files = length(lab_files)
    n_of_file = n_of_file + 1
    file = file.path(lab_dir_path, file_name)
    id = str_extract(file_name, "\\d+")

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

    dt1_final = basic_info_hosp %>%
      left_join(dt1, by=c(hospital, "sort1")) %>%
      select(-!!as.name(hospital), -sort1) %>%   # remain vname only
      filter(!is.na(vname)) %>%                  # remove the vname not included in info-file
      group_by(vname) %>%
      summarise_all(select_first_lab) %>%
      column_to_rownames("vname") %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var="labdate") %>%
      mutate(
        labdate = as.POSIXct(labdate),
        id = as.integer(id)
      ) %>%
      arrange(id, labdate)

    comdt_pre = bind_rows(comdt_pre, dt1_final)
    percent_progress = n_of_file / n_of_files * 100
    if (percent_progress >= percent_cut){
      cat("[ ", percent_cut, "% ]\n", sep="")
      percent_cut = percent_cut + 10
    }
  }

  ## Combine ptlist with labdata / Arrange variable order
  comdt = ptlist %>%
    select(id, orderdate) %>%
    left_join(comdt_pre, by="id") %>%
    select(c("id", "orderdate", "labdate", basic_info_hosp$vname))

  ## Clean data & Remove empty columns
  if (clean){
    comdt = comdt %>%
      lab_cleaner() %>%
      select_if(function(x){any(!is.na(x))})
  }

  return(comdt)
}

