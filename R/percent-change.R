#' Title
#'
#' @param dat a tibble
#' @param k number of periods over which to calculate the percentage change
#'
#' @return a tibble
#' @export
#'
#' @examples
pchange <- function(dat, k = 1){
  dat <- dat %>%
    dplyr::mutate(
      value = value %>%
        divide_by(dplyr::lag(value, n = k)) %>%
        subtract(1) %>%
        multiply_by(100))

  return(dat)
}
