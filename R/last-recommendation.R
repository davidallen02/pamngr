#' Get the most recent recommendation
#'
#' @param ticker a string
#'
#' @return a 1x tibble of the most recent recommendation
#' @export
#'
last_recommendation <- function(ticker){

  recommendations <- pamngr::get_recommendations(ticker = ticker)

  last_recommendation <- recommendations %>%
    dplyr::slice_max(.data$DATE, n = 1)

  return(last_recommendation)
}
