#' Extract baseline laboratory data
#'
#' Extracting baseline laboratory data.
#' @param data Data acquired by lab_binder function.
#' @param base_window_period Window period (months).
#' @keywords extract_base
#' @export
#' @examples
#' extract_base(
#'   data = seoul_remain_all,
#'   base_window_period = 4
#' )
extract_base = function(
  data,
  base_window_period = 4
){
  ## BASE DATA
  basedt = data %>%
    filter(labdate >= orderdate - months(base_window_period) & labdate <= orderdate + months(base_window_period)) %>%
    mutate(
      lab_diff = difftime(labdate, orderdate, units="days"),
      abs_lab_diff = abs(lab_diff)
    ) %>%
    arrange(id, orderdate, abs_lab_diff) %>%
    group_by(id, orderdate) %>%
    summarise_all(select_first_lab) %>%
    select(-lab_diff, -abs_lab_diff)

  return(basedt)
}