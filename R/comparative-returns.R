#' Title
#'
#' @param ticker a character. Ticker from buy list
#' @param sector a character. GICS sector of ticker security
#' @param return a character. Type of return to use (price or total)
#'
#' @return a table
#' @export
#'
#'
comparative_return <- function(ticker, sector, return = "total"){

  last <- pamngr::last_recommendation(ticker)$DATE %>% as.character()
  year_ago <- Sys.Date() %>% magrittr::subtract(lubridate::years(1)) %>% as.character()
  filter_date <- year_ago %>% as.POSIXct() %>% magrittr::subtract(lubridate::days(7))


  print(pamngr::last_recommendation(ticker))

  dat <- pamngr::get_data(ticker, type = "Equity", flds = "tot-return-index-gross-dvds") %>%
    dplyr::left_join(
      pamngr::get_data(sector, type = "Equity", flds = "tot-return-index-gross-dvds"),
      by = "dates") %>%
    dplyr::left_join(
      pamngr::get_data("spx", type = "Equity", flds = "tot-return-index-gross-dvds"),
      by = "dates") %>%
    magrittr::set_colnames(c("dates", stringr::str_to_upper(ticker), "Sector","SPX")) %>%
    tidyr::pivot_longer(cols = -.data$dates, names_to = "variable") %>%
    dplyr::filter(.data$dates >= filter_date)

  last_rec <- dat %>%
    dplyr::filter(.data$dates >= filter_date) %>%
    pamngr::pchange(k = last) %>%
    dplyr::slice_max(.data$dates, n = 1) %>%
    dplyr::select(-.data$dates) %>%
    dplyr::mutate(value = .data$value %>% scales::percent(accuracy = 0.1)) %>%
    magrittr::set_colnames(c("Security", "Last Recommendation"))

  ytd <- dat %>%
    dplyr::filter(.data$dates >= "2019-12-01") %>%
    pamngr::pchange(k = "2019-12-31", trading_days = TRUE) %>%
    dplyr::slice_max(.data$dates, n = 1) %>%
    dplyr::select(-.data$dates) %>%
    dplyr::mutate(value = .data$value %>% scales::percent(accuracy = 0.1)) %>%
    magrittr::set_colnames(c("Security", "Year to Date"))


  last_12m <- dat %>%
    pamngr::pchange(k = year_ago, trading_days = TRUE) %>%
    dplyr::slice_max(.data$dates, n = 1) %>%
    dplyr::select(-.data$dates) %>%
    dplyr::mutate(value = .data$value %>% scales::percent(accuracy = 0.1)) %>%
    magrittr::set_colnames(c("Security", "Last 12 Months"))

  dat <- last_rec %>%
    dplyr::left_join(ytd, by = "Security") %>%
    dplyr::left_join(last_12m, by = "Security")



  return(dat)

}
