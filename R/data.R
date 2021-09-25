#' Information about the name of laboratory parameters.
#'
#' A dataset containing the name of laboratory parameters and thier abbreviation.
#'
#' @format A data frame with 156 rows and 5 variables:
#' \describe{
#'   \item{sort1}{types of samples, blood or urine}
#'   \item{cheonan}{lab names used in cheonan}
#'   \item{bucheon}{lab names used in bucheon}
#'   \item{seoul}{lab names used in seoul}
#'   \item{vname}{representative abbreviation}
#' }
"lab_info"


globalVariables(c(
  "id", "orderdate", "labdate", "lab_diff", "abs_lab_diff",
  "labyear", "labmonth", "labmonth_cat", "labmonth_cat", "labmonth_error", "labmonth_str",
  "lab_info", "sort1", "vname", "RPR2", "RPR", "dysRBC"))
