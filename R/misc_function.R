#' select_first_lab() Function
#'
#' Select first lab value except NA.
#' @param x Vector
#' @keywords select_first_lab
#' @export
#' @examples
#' select_first_lab(vector)
select_first_lab = function(vector) {
  vec_filter_NA = vector[!is.na(vector)]
  return(vec_filter_NA[1])
}


#' compare_clean() Function
#'
#' Compare previous values and cleaned values after applying a specific function.
#' @param data data.frame object
#' @param lab variable to examine
#' @param clean_fun specific function cleaning a variable
#' @keywords compare_clean
#' @export
#' @examples
#' compare_clean(data, "HbA1c", cl_A1c)
compare_clean = function(data, lab, clean_fun, ...){
  vec = data[[lab]]
  tab = as.data.frame(table(vec)) %>%
    rename(!!as.name(lab) := 1)
  prev = unique(vec)
  after_clean = data.frame(prev, corrected = clean_fun(prev, ...)) %>%
    rename(!!as.name(lab) := prev) %>%
    left_join(tab, by=lab)
  return(after_clean)
}

#' select_non_num() Function
#'
#' Show non numeric variables and their values.
#' @param data data.frame object
#' @keywords select_non_num
#' @export
#' @examples
#' select_non_num(data)
select_non_num = function(data){
  not_numeric_dt = data.frame()
  numeric_var = c()
  not_numeric_var = c()

  for (lab in colnames(data)) {
    if (lab %in% c("id", "orderdate", "labdate")) next

    filter_not_num = data %>%
      filter(!str_detect(str_squish(!!as.name(lab)), "^[-]{0,1}\\d+[.]{0,1}\\d*$"))
    not_numeric = NROW(filter_not_num)

    if (not_numeric == 0) {
      numeric_var = c(numeric_var, lab)
    } else{
      # Make dataframe showed the number of not numeric values
      dt1 = data.frame(lab, not_numeric = not_numeric)
      not_numeric_dt = bind_rows(not_numeric_dt, dt1)

      # Print not numeric values of each lab variable
      cat("###", lab, "###\n")
      cat(str_c(unique(filter_not_num[[lab]]), collapse="\n"))
      cat("\n\n")

    }
  }
}
