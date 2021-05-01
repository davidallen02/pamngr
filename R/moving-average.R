library(magrittr)

#' Take moving average of time series data
#'
#' @param dat the data set to transform
#' @param k size of the moving average window
#'
#' @return a well formated tibble
#'
#'
#' @export
moving_avg <- function(dat, k){

  dat <- dat %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::mutate(value = .data$value %>% RcppRoll::roll_mean(n = k, fill = NA, align = "right"))

  return(dat)
}
