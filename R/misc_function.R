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
