#' Extract baseline laboratory data
#'
#' Extracting baseline laboratory data.
#' @param data Data acquired by lab_binder function.
#' @param base_window_period Window period (months).
#' @keywords extract_base
#' @export
extract_base = function(
  data,
  base_window_period = 4
){
  ## BASE DATA
  basedt = data %>%
    mutate(
      lab_diff = difftime(labdate, orderdate, units="days"),
      abs_lab_diff = abs(lab_diff)
    ) %>%
    filter(abs_lab_diff <= 30.4375 * base_window_period) %>%
    arrange(id, orderdate, abs_lab_diff) %>%
    group_by(id, orderdate) %>%
    summarise_all(select_first_lab) %>%
    select(-lab_diff, -abs_lab_diff) %>%
    ungroup()

  return(basedt)
}
