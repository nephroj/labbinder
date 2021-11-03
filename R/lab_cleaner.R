#' cl_RUA_grade() Function
#'
#' Clean RUA results containing grade values such as 1+, 2+, 3+, and 4+.
#' @param x Vector
#' @keywords cl_RUA_grade
#' @export
cl_RUA_grade = function(x) {
  if(length(intersect(unique(x), c("1+", "2+", "+2", "++", "3+", "+3", "+++"))) > 0){
    # variables with 1+, 2+, 3+, or 4+ grade (like Uprot)
    grade = case_when(
      is.na(x) ~ x,
      str_detect(str_to_lower(x), "([+][+][+][+])|(4[+])|([+]4)") ~ "4+",
      str_detect(str_to_lower(x), "([+][+][+])|(3[+])|([+]3)") ~ "3+",
      str_detect(str_to_lower(x), "([+][+])|(2[+])|([+]2)") ~ "2+",
      str_detect(str_to_lower(x), "([+])|(1[+])|([+]1)") ~ "1+",
      str_detect(str_to_lower(x), "(tr)|([+][/][-])") ~ "Trace",
      str_detect(str_to_lower(x), "(neg)|(norm)") ~ "Negative",
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
    result = str_extract(x2, "[a-zA-Z]+")
    return(result)
  } else {
    x = str_to_lower(as.character(x))
    x_num = as.numeric(str_extract(x, "\\d+[.]{0,1}\\d*e{0,1}[-|+]{0,1}\\d*"))
    result = case_when(
      str_detect(x, "n") ~ "Negative",
      str_detect(x, "p") ~ "Positive",
      str_detect(x, "\\d+[.]{0,1}\\d*E{0,1}[-|+]{0,1}\\d*") & x_num <= cutoff ~ "Negative",
      str_detect(x, "\\d+[.]{0,1}\\d*E{0,1}[-|+]{0,1}\\d*") & x_num > cutoff ~ "Positive",
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
  result = str_extract(x2, "\\d+[.]{0,1}\\d*e{0,1}[-|+]{0,1}\\d*")
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
      str_detect(ana, "3[.]{0,1}597") ~ "1:5120_",
      str_detect(ana, "6[.]{0,1}944") ~ "1:40_",
      str_detect(ana, "9[.]{0,1}722") ~ "1:80_",
      str_detect(ana, "1[.]{0,1}527") ~ "1:160_",
      str_detect(ana, "2[.]{0,1}638") ~ "1:320_",
      str_detect(ana, "4[.]{0,1}861") ~ "1:640_",
      str_detect(ana, "9[.]{0,1}305") ~ "1:1280_",
      str_detect(ana, "1[.]{0,1}819") ~ "1:2560_",
      TRUE ~ ana
    )
  }
  x2 = str_to_lower(x)
  result = case_when(
    is.na(x) ~ x,
    str_detect(x2, "1[:]\\d{1,5}") ~ paste0(str_extract(x2, "1[:]\\d{1,5}"), "_"),
    str_detect(x2, "neg") ~ "Negative",
    str_detect(x2, "\\d+[.]{0,1}\\d*e{0,1}[-|+]{0,1}\\d*") ~ num_to_titer(x2),
    TRUE ~ x
  )
  return(result)
}

#' cl_remove_symbol() Function
#'
#' Remove the symbols unnecessary
#' @param x Vector
#' @keywords cl_remove_symbol
#' @export
cl_remove_symbol = function(x){
  if (length(x[!is.na(x)]) == 0){
    return(x)
  }
  x2 = str_replace_all(x, "\\s*", "")
  result = case_when(
    str_detect(x2, "(?<=[<|>])\\d+[.]{0,1}\\d*") ~ str_extract(x2, "(?<=[<|>])\\d+[.]{0,1}\\d*"),
    str_detect(x2, "^[.]|[-]{1,4}$") ~ NA_character_,
    str_detect(x2, "\\d+[.]{0,1}\\d*(?=(sec){0,1}((\uc774\uc0c1)|(\uc774\ud558)))") ~     # e-sang, e-ha
      str_extract(x2, "\\d+[.]{0,1}\\d*(?=(sec){0,1}((\uc774\uc0c1)|(\uc774\ud558)))"),   # e-sang, e-ha
    TRUE ~ x2
  )
  result = str_squish(result)
  result_non_num = result[!is.na(result) & !str_detect(result, "^\\d+[.]{0,1}\\d*(e|E){0,1}[-|+]{0,1}\\d*$")]
  result_total_len = length(result[!is.na(result)])
  result_non_num_len = length(result_non_num)
  if (result_total_len == 0){
    return(x)
  }
  else if (result_non_num_len / result_total_len < 0.02) {
    if(result_non_num_len != 0) cat(unique(result_non_num), "will be replaced by NA.\n")
    result[!is.na(result) & !str_detect(result, "^\\d+[.]{0,1}\\d*(e|E){0,1}[-|+]{0,1}\\d*$")] = NA
    return(as.numeric(result))
  } else{
    return(x)
  }
}

#' lab_cleaner() Function
#'
#' Cleaning the dataframe.
#' @param data Dataframe to be cleaned
#' @keywords lab_cleaner
#' @export
lab_cleaner = function(data) {
  column_name = colnames(data)

  clean_apply = function(data, var, func, ..., varname=var){
    if (var %in% column_name){
      data = data %>%
        mutate(!!as.name(varname) := func(data[[var]], ...))
    }
    return(data)
  }

  data_clean = data %>%
    clean_apply("Uleuko", cl_RUA_grade) %>%
    clean_apply("Uprot", cl_RUA_grade) %>%
    clean_apply("Uketone", cl_RUA_grade) %>%
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
    clean_apply("ANAtiter", cl_ANA_titer)

  if ("RPR" %in% column_name & "RPR2" %in% column_name){
    data_clean = data_clean %>%
      mutate(
        RPR = case_when(
          RPR2 == "Non Reactive" ~ "Negative",
          RPR2 == "Reactive" ~ "Positive",
          TRUE ~ RPR)
      )
  }
  if ("dysRBC" %in% column_name){
    data_clean = data_clean %>%
      mutate(
        dysRBC = ifelse(str_detect(dysRBC, "(unable)|(countable)"), "0", str_squish(dysRBC))
      )
  }

  data_clean = data_clean %>%
    mutate_all(cl_remove_symbol)

  cat("Data cleaning is done.\n")
  return(data_clean)
}

# lab_cleaner = function(data) {
#   data_clean = data %>%
#     mutate_at(c("Uleuko", "Uprot", "Uglc", "Uketone", "UUB", "Ubil", "Uery", "Unit"), cl_RUA_grade) %>%
#     mutate_at(c("URBC", "UWBC"), cl_RUA_micro) %>%
#     mutate_at(c("HbA1c"), cl_A1c) %>%
#
#     mutate_at(c("anti_GBM", "anti_dsDNA", "ANCA_PR3", "ANCA_MPO"),
#               .funs=list(titer=cl_extract_titer)) %>%
#     mutate_at(c("anti_HIV", "anti_HCV", "HBsAg", "RPR", "anti_HAV"),
#               cl_serol_marker, cutoff=1) %>%
#     mutate_at(c("anti_HBs"), cl_serol_marker, cutoff=10) %>%
#     mutate_at(c("anti_GBM"), cl_serol_marker, cutoff=15) %>%
#     mutate_at(c("anti_dsDNA", "ANCA", "ANCA_PR3", "ANCA_MPO", "cryoglobulin"),
#               cl_serol_marker, cutoff=0) %>%
#     mutate(
#       RPR = ifelse(
#         RPR2 == "Non Reactive", "Negative",
#         ifelse(RPR2 == "Reactive", "Positive", RPR)),
#       dysRBC = ifelse(str_detect(dysRBC, "(unable)|(countable)"), "0", str_squish(dysRBC))
#     ) %>%
#
#     mutate_at(c("ANA"), cl_ANA) %>%
#     mutate_at(c("ANAtiter"), cl_ANA_titer) %>%
#
#     mutate_all(cl_remove_symbol)
#   cat("Data cleaning is done.\n")
#   return(data_clean)
# }

