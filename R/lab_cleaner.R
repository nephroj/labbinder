#' numeric_string() Function
#'
#' return the result of regular expression matching of numeric string
#' @param x Vector
#' @param exact exact matching or not
#' @param extract string extract or detect
#' @keywords numeric_string
#' @export
numeric_string = function(x, exact=F, extract=F){
  if (exact){
    num_str = "^\\d+[.]?\\d*(e|E)?([-]|[+])?\\d*$"
  } else {
    num_str = "\\d+[.]?\\d*(e|E)?([-]|[+])?\\d*"
  }

  if (extract) {
    num_or_not = str_extract(x, num_str)
  } else {
    num_or_not = str_detect(x, num_str)
  }

  return(num_or_not)
}

#' cl_RUA_grade() Function
#'
#' Clean RUA results containing grade values such as 1+, 2+, 3+, and 4+.
#' @param x Vector
#' @keywords cl_RUA_grade
#' @export
cl_RUA_grade = function(x) {
  unique_x = stringr::str_extract(unique(x), "([+]\\d)|(\\d[+]|[+]{2,4})")
  if(length(intersect(unique_x, c("1+", "+1", "2+", "+2", "++", "3+", "+3", "+++"))) > 0){
    # variables with 1+, 2+, 3+, or 4+ grade (like Uprot)
    grade = case_when(
      is.na(x) ~ x,
      str_detect(str_to_lower(x), "([+][+][+][+])|(4[+])|([+]4)") ~ "4+",
      str_detect(str_to_lower(x), "([+][+][+])|(3[+])|([+]3)") ~ "3+",
      str_detect(str_to_lower(x), "([+][+])|(2[+])|([+]2)") ~ "2+",
      str_detect(str_to_lower(x), "(tr)|([+][/][-])") ~ "Trace",
      str_detect(str_to_lower(x), "(neg)|(norm)") ~ "Negative",
      str_detect(str_to_lower(x), "([+])|(1[+])|([+]1)") ~ "1+",
      x %in% c("-") ~ "Negative",
      TRUE ~ paste0(x, "_error")
    )

  } else{
    # variables with positive or negative values (like Unit)
    grade = case_when(
      is.na(x) ~ x,
      str_detect(str_to_lower(x), "n") ~ "Negative",
      str_detect(str_to_lower(x), "p") ~ "Positive",
      x %in% c("-") ~ "Negative",
      x %in% c("+") ~ "Positive",
      TRUE ~ paste0(x, "_error")
    )
  }
  return(grade)
}

#' cl_RUA_micro() Function
#'
#' Clean RUA microscopic exam values.
#' @param x Vector
#' @keywords cl_RUA_micro
#' @export
cl_RUA_micro = function(x) {
  x = str_replace(x, "[(][?][)]", "")
  x = str_replace(x, "[-]{2}", "-")
  x = str_replace_all(x, "\\s+", "")
  result = case_when(
    is.na(x) ~ x,
    str_detect(str_to_lower(x), "grade") ~ x,
    str_detect(str_to_lower(x), "^[.]$") ~ NA_character_,
    str_detect(str_to_lower(x), "many") ~ "Many",
    str_detect(str_to_lower(x), "numer") ~ "Numerous",
    str_detect(x, "^\\d{5}") ~
      paste0(as.character(month(as.Date(suppressWarnings(as.numeric(x)), origin="1899-12-30"))),
             "-", as.character(day(as.Date(suppressWarnings(as.numeric(x)), origin="1899-12-30"))), "_"),
    str_detect(x, "\\d+[-]\\d+") ~ paste0(str_extract(x, "\\d+[-]\\d+"), "_"),
    str_detect(x, "([<]|[>])\\d+") ~ paste0(str_extract(x, "([<]|[>])\\d+"), "_"),

    # str_detect(x, "1Grade") ~ "<1_",
    # str_detect(x, "2Grade") ~ "1-4_",
    # str_detect(x, "3Grade") ~ "5-9_",
    # str_detect(x, "4Grade") ~ "10-29_",
    # str_detect(x, "5Grade") ~ "30-100_",
    # str_detect(x, "6Grade") ~ ">100_",

    TRUE ~ paste0(x, "_error")
  )
  return(result)
}


#' cl_A1c() Function
#'
#' Clean HbA1c values.
#' @param x Vector
#' @keywords cl_A1c
#' @export
cl_A1c = function(x){
  result = ifelse(
    str_detect(x, "\\d{1,2}[.]\\d+\\s*\\D"),
    str_extract(x, "\\d{1,2}[.]\\d+(?=\\s*\\D)"), x)
  return(result)
}

#' cl_serol_marker() Function
#'
#' Clean serologic markers values.
#' @param x Vector
#' @param cutoff cutoff value for positive
#' @keywords cl_serol_marker
#' @export
cl_serol_marker = function(x, cutoff=0){
  if (cutoff == 0){
    x2 = str_replace_all(x, "\\s*", "")
    result = ifelse(numeric_string(x2, exact=T), NA, str_extract(x2, "[a-zA-Z]+"))
    return(result)
  } else {
    x = str_to_lower(as.character(x))
    x_num = as.numeric(numeric_string(x, extract=T))
    result = case_when(
      str_detect(x, "n") ~ "Negative",
      str_detect(x, "p") ~ "Positive",
      numeric_string(x) & x_num <= cutoff ~ "Negative",
      numeric_string(x) & x_num > cutoff ~ "Positive",
      TRUE ~ x
    )
    return(result)
  }
}

#' cl_extract_titer() Function
#'
#' Extract titer values.
#' @param x Vector
#' @keywords cl_extract_titer
#' @export
cl_extract_titer = function(x){
  x2 = str_replace_all(x, "\\s*", "")
  result = numeric_string(x2, extract=T)
  return(result)
}

#' cl_ANA() Function
#'
#' Clean ANA values.
#' @param x Vector
#' @keywords cl_ANA
#' @export
cl_ANA = function(x){
  x2 = str_to_lower(x)
  result = case_when(
    is.na(x) ~ x,
    str_detect(x2, "weakpositive") ~ "WeakPositive",
    str_detect(x2, "positive") ~ "Positive",
    str_detect(x2, "negative") ~ "Negative",
    str_detect(x2, "trace") ~ "Trace",
    str_detect(str_to_lower(x2), "(4[+])|([+]4)") ~ "4+",
    str_detect(str_to_lower(x2), "(3[+])|([+]3)") ~ "3+",
    str_detect(str_to_lower(x2), "(2[+])|([+]2)") ~ "2+",
    str_detect(str_to_lower(x2), "(1[+])|([+]1)") ~ "1+",
    TRUE ~ paste0(x, "_error")
  )
  return(result)
}

#' cl_ANA_titer() Function
#'
#' Clean ANA titer values.
#' @param x Vector
#' @keywords cl_ANA_titer
#' @export
cl_ANA_titer = function(x){
  num_to_titer = function(ana){
    result = case_when(
      str_detect(ana, "3[.]?597") ~ "1:5120_",
      str_detect(ana, "6[.]?944") ~ "1:40_",
      str_detect(ana, "9[.]?722") ~ "1:80_",
      str_detect(ana, "1[.]?527") ~ "1:160_",
      str_detect(ana, "2[.]?638") ~ "1:320_",
      str_detect(ana, "4[.]?861") ~ "1:640_",
      str_detect(ana, "9[.]?305") ~ "1:1280_",
      str_detect(ana, "1[.]?819") ~ "1:2560_",
      TRUE ~ ana
    )
  }
  x2 = str_to_lower(x)
  result = case_when(
    is.na(x) ~ x,
    str_detect(x2, "1[:]\\d{1,5}") ~ paste0(str_extract(x2, "1[:]\\d{1,5}"), "_"),
    str_detect(x2, "neg") ~ "Negative",
    numeric_string(x2) ~ num_to_titer(x2),
    TRUE ~ x
  )
  return(result)
}

#' cl_dysRBC()) Function
#'
#' Clean dysmorphic RBC values.
#' @param x Vector
#' @keywords cl_dysRBC
#' @export
cl_dysRBC = function(x){
  result = case_when(
    str_detect(x, "(unable)|(countable)") ~ "0",
    TRUE ~ str_squish(x))
  return(result)
}

#' cl_remove_symbol() Function
#'
#' Remove the symbols unnecessary
#' @param x Vector
#' @keywords cl_remove_symbol
#' @export
cl_remove_symbol = function(x, y=NULL){
  if (sum(!is.na(x)) == 0){
    return(x)
  }
  x2 = str_replace_all(x, "\\s*", "")
  result = case_when(
    str_detect(x2, "(?<=[<|>])\\d+[.]?\\d*$") ~ str_extract(x2, "(?<=[<|>])\\d+[.]?\\d*"),
    str_detect(x2, "^[.]|[-]{1,4}$") ~ NA_character_,
    str_detect(x2, "^\\d+[.]?\\d*(?=(sec)?((\uc774\uc0c1)|(\uc774\ud558)|(\uBBF8\uB9CC)))") ~     # e-sang, e-ha, mi-man
      str_extract(x2, "^\\d+[.]?\\d*(?=(sec)?((\uc774\uc0c1)|(\uc774\ud558)|(\uBBF8\uB9CC)))"),   # e-sang, e-ha, mi-man
    TRUE ~ x2
  )

  # Print different values
  if (!identical(x2, result)){
    diff1 = x2[!is.na(x2) & (x2 != result | is.na(result))]
    diff2 = result[!is.na(x2) & (x2 != result | is.na(result))]
    diff_dt = data.frame(diff1, diff2) %>%
      group_by(diff1, diff2) %>%
      summarise(n = n()) %>%
      mutate(diff_text = paste0(y, ": ", diff1, " --> ", diff2, " (", n, ")\n"))

    cat(diff_dt$diff_text, sep="")
  }

  result = str_squish(result)
  result_non_num = result[!is.na(result) & !numeric_string(result, exact=T)]
  result_total_len = sum(!is.na(result))
  result_non_num_len = length(result_non_num)

  if (result_total_len == 0){
    result_final = x

  } else if (result_non_num_len / result_total_len < 0.02) {
    if(result_non_num_len != 0) {
      cat(y, ": ", str_c(unique(result_non_num), collapse=", "), " --> NA\n", sep="")
    }
    result[!is.na(result) & !numeric_string(result, exact=T)] = NA
    result_final = as.numeric(result)

  } else{
    result_final = x
  }

  return(result_final)
}

#' cl_RPR_from_RPR2() Function
#'
#' RPR cleaning from RPR2 values
#' @param x1 Vector
#' @param x2 Vector
#' @keywords cl_RPR_from_RPR2
#' @export
cl_RPR_from_RPR2 = function(x1, x2) {
  result = case_when(
    x2 == "Non Reactive" ~ "Negative",
    x2 == "Reactive" ~ "Positive",
    TRUE ~ x1)
  return(result)
}

#' lab_cleaner() Function
#'
#' Cleaning the dataframe.
#' @param data Dataframe to be cleaned
#' @keywords lab_cleaner
#' @export
lab_cleaner = function(data, rm_empty_col=F) {

  start_time = Sys.time()
  column_name = colnames(data)

  # Apply the clean function to the specific variable
  clean_apply = function(data, var, func, ..., varname=var){
    if (var %in% column_name & sum(!is.na(data[[var]])) != 0){
      data = data %>%
        mutate(!!as.name(varname) := func(data[[var]], ...))
    }
    return(data)
  }

  # Mutate var1 values using var2 values
  clean_apply2 = function(data, var1, var2, func, ..., varname=var1){
    if (var1 %in% column_name & var2 %in% column_name & sum(!is.na(data[[var2]])) != 0){
      data = data %>%
        mutate(!!as.name(varname) := func(data[[var1]], data[[var2]], ...))
    }
    return(data)
  }

  data_clean = data %>%
    clean_apply("Uleuko", cl_RUA_grade) %>%
    clean_apply("Uprot", cl_RUA_grade) %>%
    clean_apply("Uketone", cl_RUA_grade) %>%
    clean_apply("Uglc", cl_RUA_grade) %>%
    clean_apply("UUB", cl_RUA_grade) %>%
    clean_apply("Ubil", cl_RUA_grade) %>%
    clean_apply("Uery", cl_RUA_grade) %>%
    clean_apply("Unit", cl_RUA_grade) %>%

    clean_apply("URBC", cl_RUA_micro) %>%
    clean_apply("UWBC", cl_RUA_micro) %>%

    clean_apply("HbA1c", cl_A1c) %>%

    clean_apply("anti_GBM", cl_extract_titer, varname="anti_GBM_titer") %>%
    clean_apply("anti_dsDNA", cl_extract_titer, varname="anti_dsDNA_titer") %>%
    clean_apply("ANCA_PR3", cl_extract_titer, varname="ANCA_PR3_titer") %>%
    clean_apply("ANCA_MPO", cl_extract_titer , varname="ANCA_MPO_titer") %>%

    clean_apply("anti_HIV", cl_serol_marker, cutoff=1) %>%
    clean_apply("anti_HCV", cl_serol_marker, cutoff=1) %>%
    clean_apply("HBsAg", cl_serol_marker, cutoff=1) %>%
    clean_apply("anti_HAV", cl_serol_marker, cutoff=1) %>%
    clean_apply("RPR", cl_serol_marker, cutoff=1) %>%

    clean_apply("anti_HBs", cl_serol_marker, cutoff=10) %>%
    clean_apply("anti_GBM", cl_serol_marker, cutoff=15) %>%

    clean_apply("anti_dsDNA", cl_serol_marker, cutoff=0) %>%
    clean_apply("ANCA", cl_serol_marker, cutoff=0) %>%
    clean_apply("ANCA_PR3", cl_serol_marker, cutoff=0) %>%
    clean_apply("ANCA_MPO", cl_serol_marker, cutoff=0) %>%
    clean_apply("cryoglobulin", cl_serol_marker, cutoff=0) %>%

    clean_apply("ANA", cl_ANA) %>%
    clean_apply("ANAtiter", cl_ANA_titer) %>%
    clean_apply("dysRBC", cl_dysRBC) %>%

    clean_apply2("RPR", "RPR2", cl_RPR_from_RPR2)

  # Apply the cl_remove_function to all columns
  data_clean = data_clean %>%
    purrr::imap_dfc(cl_remove_symbol)

  # Remove the empty columns
  if (rm_empty_col) {
    data_clean = data_clean %>%
      select_if(~any(!is.na(.)))
  }

  end_time = Sys.time()
  time_spent = round(as.numeric((difftime(end_time, start_time, units="secs"))), 1)
  cat("Data cleaning is done (", time_spent, " secs).\n", sep="")
  return(data_clean)
}


#' lab_cleaner_read() Function
#'
#' Cleaning the dataframe.
#' @param file file to be cleaned
#' @keywords lab_cleaner_read
#' @export
lab_cleaner_read = function(file, rm_empty_col=F){
  file_name = basename(file)
  if (stringr::str_detect(file_name, "[.]xlsx")){
    data = readxl::read_excel(file, col_types="text")
  } else if (stringr::str_detect(file_name, "[.]csv")){
    data = readr::read_csv(file, locale=locale(encoding="CP949"), col_types=cols(.default="c"))
  } else {
    stop("Incorrect file type. Use a csv file or a excel file.")
  }
  data_clean = lab_cleaner(data, rm_empty_col)
  return(data_clean)
}


#' clean_compare() Function
#'
#' Compare the cleaned data.frame with the original data.frame
#' @param data_orig a original data.frame
#' @param data_clean a cleaned data.frame
#' @keywords clean_compare
#' @export
clean_compare = function(data_orig, data_clean) {
  options(dplyr.summarise.inform = FALSE)

  # Print different values
  print_diff_vec = function(x, y) {
    x1 = as.character(data_orig[[y]])
    x2 = as.character(data_clean[[y]])

    if (!identical(x1, x2)){
      orig = x1[!is.na(x1) & (x1 != x2 | is.na(x2))]
      cleaned = x2[!is.na(x1) & (x1 != x2 | is.na(x2))]
      diff_dt = data.frame(orig, cleaned) %>%
        filter(
          !numeric_string(orig, exact=T) |
            !numeric_string(cleaned, exact=T) |
            is.na(cleaned)
        ) %>%
        group_by(orig, cleaned) %>%
        summarise(n = n()) %>%
        mutate(
          variable = y
        ) %>%
        select(variable, orig, cleaned, n) %>%
        ungroup()

      return(diff_dt)
    }
  }
  all_diff = data_orig %>%
    purrr::imap(print_diff_vec) %>%
    dplyr::bind_rows()
  return(all_diff)
}
