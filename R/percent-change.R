#' Title
#'
#' @param dat a tibble
#' @param k number of periods over which to calculate the percentage change
#'
#' @return a tibble
#' @export
#' @importFrom rlang .data
#'
pchange <- function(dat, k = 1){

  dat %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(dat,
                  value = .data$value %>%
                    magrittr::divide_by(dplyr::lag(.data$value, n = k)) %>%
                    magrittr::subtract(1) %>%
                    magrittr::multiply_by(100)) %>%
    dplyr::ungroup()

}
