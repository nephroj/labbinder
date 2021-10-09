#' Lab excel files binder
#'
#' This function tie multiple excel files containing laboratory data to one file.
#' @param hospital Select the hospital: seoul, bucheon, or cheonan.
#' @param ptlist_file Patient list file which included "id" and "orderdate" column.
#' @param lab_dir_path Directory path included excel files.
#' @param lab_info_file Path of information file.
#' @param clean Make a cleaned data or not.
#' @keywords lab_binder
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_all
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_all
#' @importFrom dplyr summarise_at
#' @importFrom dplyr arrange
#' @importFrom dplyr starts_with
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom tidyr spread
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @importFrom stringr str_squish
#' @importFrom stringr str_to_lower
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @export
lab_binder = function(
  hospital,
  ptlist_file,
  lab_dir_path,
  lab_info_file = NULL,
  clean = TRUE
) {

  # Load data
  if (is.null(lab_info_file)){
    lab_info_dt = lab_info
  } else{
    lab_info_dt = suppressMessages(suppressWarnings(read_excel(lab_info_file, col_types="text")))
  }
  lab_info_hosp = lab_info_dt %>%
    select(!!as.name(hospital), sort1, vname)

  # Load ptlist file
  if (str_detect(ptlist_file, "[.]xlsx")) {
    ptlist = suppressMessages(suppressWarnings(read_excel(ptlist_file, col_types="text")))
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
  lab_files = lab_files[str_detect(lab_files, "[.]xlsx")]
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

    # skip if file_name did not contain id number
    if (!str_detect(file_name, "\\d+")){
      cat(file_name, "does not have a id number.\n")
      next
    }

    dt1 = suppressMessages(suppressWarnings(read_excel(file, col_types="text")))
    # Specific column must included in excel file (cham-go-chi, kum-sa-hang-mok)
    if (!"\ucc38\uace0\uce58" %in% colnames(dt1) & !"\uac80\uc0ac\ud56d\ubaa9" %in% colnames(dt1)) {
      cat(file_name, "is not a EMR excel file.\n")
      next
    }

    id = str_extract(file_name, "\\d+")
    dt1 = dt1 %>%
      select(!starts_with("..")) %>%
      select(-"\ucc38\uace0\uce58") %>%                      # cham-go-chi
      rename(
        !!as.name(hospital) := "\uac80\uc0ac\ud56d\ubaa9",   # kum-sa-hang-mok
        sort1 = "\uac80\uccb4\uba85"                         # kum-che-myeong
      ) %>%
      filter(!str_detect(sort1, "(Body)|(Pleural)|(Peritoneal)|(CSF)|(Joint)")) %>%
      mutate(                                                # not-so-byeon, bam-so-byeon
        sort1 = ifelse(str_detect(sort1, "(Urine)|(\ub0ae\uc18c\ubcc0)|(\ubc24\uc18c\ubcc0)"), "urine", "blood")
      )

    dt1_final = lab_info_hosp %>%
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
    stop("There is no available data. Check the EMR data files.")
  }

  ## Combine ptlist with labdata / Arrange variable order
  comdt = ptlist %>%
    left_join(comdt_pre, by="id") %>%
    mutate(hospital = hospital) %>%
    select(c("hospital", "id", "orderdate", "labdate", lab_info_hosp$vname))
  cat("Data combination is done.\n")

  ## Clean data & Remove empty columns
  if (clean){
    comdt = comdt %>%
      lab_cleaner()
  }

  return(comdt)
}


