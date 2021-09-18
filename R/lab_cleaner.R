#' RUA_grade() Function
#'
#' Cleaning of data.
#' @param x Vector
#' @keywords RUA_grade
#' @export
#' @examples
#' RUA_grade(vector)
RUA_grade = function(x) {
  if(length(intersect(unique(x), c("2+", "+2", "++", "3+", "+3", "+++"))) > 0){
    # Uprot과 같이 1+, 2+, 3+ 단계로 이루어진 경우
    grade = case_when(
      is.na(x) ~ x,
      str_detect(str_to_lower(x), "neg|norm") ~ "neg",
      str_detect(str_to_lower(x), "tr") ~ "trace",
      str_detect(str_to_lower(x), "[+][+][+][+]|4[+]|[+]4") ~ "4+",
      str_detect(str_to_lower(x), "[+][+][+]|3[+]|[+]3") ~ "3+",
      str_detect(str_to_lower(x), "[+][+]|2[+]|[+]2") ~ "2+",
      str_detect(str_to_lower(x), "[+]|1[+]|[+]1") ~ "1+",
      x %in% c("-") ~ "neg",
      TRUE ~ "error"
    )
  } else{
    # Unit와 같이 negative or positive로 이루어진 경우
    grade = case_when(
      is.na(x) ~ x,
      str_detect(str_to_lower(x), "n") ~ "neg",
      str_detect(str_to_lower(x), "p") ~ "pos",
      x %in% c("-") ~ "neg",
      x %in% c("+") ~ "pos",
      TRUE ~ "error"
    )
  }
}

#' RUA_micro() Function
#'
#' Cleaning of data.
#' @param x Vector
#' @keywords RUA_micro
#' @export
#' @examples
#' RUA_micro(vector)
RUA_micro = function(x) {
  result = case_when(
    is.na(x) ~ x,
    str_detect(str_to_lower(x), "many") ~ "many",
    str_detect(str_to_lower(x), "numer") ~ "numerous",
    str_detect(x, "^\\d{5}") ~ paste0(as.character(month(as.Date(as.numeric(x), origin="1899-12-30"))),
                                     "-", as.character(day(as.Date(as.numeric(x), origin="1899-12-30"))), "_"),
    TRUE ~ paste0(x, "_")

  )
  return(result)
}


#' A1c_cleaner() Function
#'
#' Cleaning of data.
#' @param x Vector
#' @keywords A1c_cleaner
#' @export
#' @examples
#' A1c_cleaner(vector)
A1c_cleaner = function(x){
  result = ifelse(
    str_detect(x, "\\d{1,2}[.]\\d+\\s*\\D"),
    str_extract(x, "\\d{1,2}[.]\\d+(?=\\s*\\D)"), x)
  return(result)
}





#' lab_cleaner() Function
#'
#' Cleaning of dataframe.
#' @param x Dataframe
#' @keywords lab_cleaner
#' @export
#' @examples
#' lab_cleaner(data)
lab_cleaner = function(data) {
  data_clean = data %>%
    mutate_at(vars(Uleuko, Uprot, Uglc, Uketone, UUB, Ubil, Uery, Unit), list(new = RUA_grade)) %>%
    # mutate_at(vars(), list(Unit_new = RUA_grade_nit)) %>%
    mutate_at(vars(URBC, UWBC), list(new = RUA_micro)) %>%
    mutate_at(vars(HbA1c), list(HbA1c_new = A1c_cleaner))
  return(data_clean)
}

