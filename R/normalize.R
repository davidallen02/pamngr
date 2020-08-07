#' Normalize data
#'
#' @param dat a tibble with datatime dates column
#' @param norm_date character
#'
#' @return a tibble
#'

normalize <- function(dat, norm_date){

  normalize_earliest <- function(x){

    start_value <- x[1]
    y <- x/start_value*100

    return(y)
  }

  dat <- dat %>%
    dplyr::filter(.data$dates >= lubridate::as_datetime(norm_date)) %>%
    dplyr::mutate_if(is.numeric, normalize_earliest)

  return(dat)
}
