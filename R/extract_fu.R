#' Extract f/u lab data
#'
#' Extracting lab data at specific intervals.
#' @param data Data made by lab_binder function.
#' @param fu_lab_name The name of output file name.
#' @param fu_intv_month Specific interval (months).
#' @param fu_max_month Maximum month you want to see.
#' @param fu_wide Wide format or long format.
#' @keywords extract_fu
#' @export
#' @examples
#' extract_fu(
#'   data = bx_cheonan_all
#'   fu_lab_name = "Cr"
#'   fu_intv_month = 3
#'   fu_max_month = 192
#'   fu_wide = T
#' )
extract_fu = function(
  data,
  fu_lab_name,
  fu_intv_month = 3,
  fu_max_month = 192,
  fu_wide = TRUE
){

  half_intv = fu_intv_month / 2
  max_month_plus_half_intv = fu_max_month + half_intv

  data_cut = data %>%
    select(id, orderdate, labdate, !!as.name(fu_lab_name)) %>%
    mutate(
      labdiff = difftime(labdate, orderdate, units="days"),
      labyear = as.numeric(labdiff / 365.25),
      labmonth = labyear * 12,
      labmonth_cat = cut(labmonth,
                         breaks=seq(-half_intv, max_month_plus_half_intv, by=fu_intv_month),
                         labels=seq(0, fu_max_month, by=fu_intv_month)),
      labmonth_cat = as.numeric(as.character(labmonth_cat)),
      labmonth_error = abs(labmonth_cat - labmonth)
    ) %>%
    filter(!is.na(labmonth_cat)) %>%
    mutate(lab_month = paste0(fu_lab_name, "_", sprintf("%03d", labmonth_cat), "m")) %>%
    arrange(id, orderdate, lab_month, labmonth_error) %>%
    group_by(id, orderdate, lab_month) %>%
    summarise_at(c(fu_lab_name), select_first_lab)

  if (fu_wide) {
    data_cut = data_cut %>%
      spread(lab_month, !!as.name(fu_lab_name))
  }
  return(data_cut)

}


