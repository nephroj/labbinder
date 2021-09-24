#' Lab excel files binder
#'
#' This function tie multiple excel files containing laboratory data to one file.
#' @param hospital Select the hospital: seoul, bucheon, or cheonan.
#' @param ptlist_file Patient list file which included "id" and "orderdate" column.
#' @param lab_dir_path Directory path included excel files.
#' @param basic_info_file Path of information file.
#' @param clean Make a cleaned data or not.
#' @keywords lab_binder
#' @export
#' @examples
#' lab_binder(
#'   hospital = "seoul",
#'   ptlist_file = "lab/seoul_remain.xlsx",
#'   lab_dir_path = "lab/seoul_remain",
#'   basic_info_file = NULL,
#'   clean = TRUE
#' )
lab_binder = function(
  hospital,
  ptlist_file,
  lab_dir_path,
  basic_info_file = NULL,
  clean = TRUE
) {

  # Load data
  if (is.null(basic_info_file)){
    basic_info_dt = basic_info
  } else{
    basic_info_dt = suppressMessages(suppressWarnings(read_excel(basic_info_file)))
  }
  basic_info_hosp = basic_info_dt %>%
    select(!!as.name(hospital), sort1, vname)

  # Load ptlist file
  if (str_detect(ptlist_file, "[.]xlsx")) {
    ptlist = suppressMessages(suppressWarnings(read_excel(ptlist_file)))
  } else{
    stop("ptlist file should be a xlsx file.")
  }

  # ptlist file must have "id" and "orderdate" column
  if ("id" %in% colnames(ptlist) & "orderdate" %in% colnames(ptlist)){
    ptlist = ptlist %>%
      select(id, orderdate) %>%
      mutate(id = as.integer(id))
  } else{
    stop('ptlist must have "id" and "orderdate" column.')
  }

  lab_files = list.files(lab_dir_path)
  n_of_files = length(lab_files)

  # Print the status of data collection
  cat("# Number of patients in list file:", NROW(ptlist), "\n")
  cat("# Number of excel files collected:", n_of_files, "\n")

  ## ALL DATA
  comdt_pre = data.frame()
  n_of_file = 0
  percent_cut = 10

  for (file_name in lab_files) {
    n_of_file = n_of_file + 1
    percent_progress = n_of_file / n_of_files * 100
    if (percent_progress >= percent_cut){
      cat("[ ", percent_cut, "% ]\n", sep="")
      percent_cut = percent_cut + 10
    }
    file = file.path(lab_dir_path, file_name)

    # skip if file_name did not contain ".xlsx" or id number
    if (!str_detect(file_name, "[.]xlsx")) {
      cat(file_name, "is not a xlsx file.\n")
      next
    } else if(!str_detect(file_name, "\\d+")){
      cat(file_name, "does not have a id number.\n")
      next
    }

    dt1 = suppressMessages(suppressWarnings(read_excel(file)))
    # Specific column must included in excel file
    if (!"참고치" %in% colnames(dt1) & !"검사항목" %in% colnames(dt1)) {
      cat(file_name, "is not a EMR excel file.\n")
      next
    }

    id = str_extract(file_name, "\\d+")
    dt1 = dt1 %>%
      select(!starts_with("..")) %>%
      select(-"참고치") %>%
      rename(
        !!as.name(hospital) := "검사항목",
        sort1 = "검체명"
      ) %>%
      filter(!str_detect(sort1, "(Body)|(Pleural)|(Peritoneal)|(CSF)|(Joint)")) %>%
      mutate(
        sort1 = ifelse(str_detect(sort1, "(Urine)|(낮소변)|(밤소변)"), "urine", "blood")
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
  }

  ## Check if comdt_pre doesn't have any data.
  if (sum(dim(comdt_pre) == 0) > 0) {
    stop("There is no available data. Check the data files included.")
  }

  ## Combine ptlist with labdata / Arrange variable order
  comdt = ptlist %>%
    left_join(comdt_pre, by="id") %>%
    mutate(hospital = hospital) %>%
    select(c("hospital", "id", "orderdate", "labdate", basic_info_hosp$vname))
  cat("Data combination is done.\n")

  ## Clean data & Remove empty columns
  if (clean){
    comdt = comdt %>%
      lab_cleaner()
  }

  return(comdt)
}

