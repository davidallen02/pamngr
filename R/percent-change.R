#' Title
#'
#' @param dat a tibble
#' @param k number of periods over which to calculate the percentage change or a character coercible to a date (YYYY-MM-DD)
#' @param trading_days logical. Should compare date be revised to fall on the most recent prior trading day.
#'
#' @return a tibble
#' @export
#' @importFrom rlang .data
#'
pchange <- function(dat, k = 1, trading_days = FALSE){

  if(is.numeric(k)){
    dat <-  dat %>%
      dplyr::group_by(.data$variable) %>%
      dplyr::mutate(value = .data$value %>%
                      magrittr::divide_by(dplyr::lag(.data$value, n = k)) %>%
                      magrittr::subtract(1) %>%
                      magrittr::multiply_by(100)) %>%
      dplyr::ungroup()
  }

  if(is.character(k)){
    compare_date <- k %>% paste("00:00:00") %>% lubridate::as_datetime()

    if(trading_days){
      if(weekdays(compare_date) == "Saturday"){
        compare_date <- compare_date - lubridate::days(1)}

      if(weekdays(compare_date) == "Sunday"){
        compare_date <- compare_date - lubridate::days(2)}
    }

    fcompare_date <- "d" %>%
      paste0(compare_date) %>%
      stringr::str_remove_all("-") %>%
      rlang::sym()

    dat <- dat %>%
      tidyr::pivot_wider(names_from = .data$dates, names_prefix = "d") %>%
      dplyr::rename_all(stringr::str_remove_all,"-") %>%
      dplyr::mutate(compare_date = (!!fcompare_date)) %>%
      dplyr::mutate(dplyr::across(-.data$variable, ~ ./compare_date)) %>%
      dplyr::select(-compare_date) %>%
      tidyr::pivot_longer(cols = -.data$variable, names_to = "dates", values_to = "value") %>%
      dplyr::mutate(
        dates = .data$dates %>%
          stringr::str_remove_all("d") %>%
          lubridate::as_datetime(format = "%Y%m%d"),
        value = .data$value %>% magrittr::subtract(1)
      ) %>%
      dplyr::select(.data$dates, .data$variable, .data$value)
  }


  return(dat)
}
