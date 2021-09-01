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

    period_change <- function(dat, k = lubridate::years(1)){

      min_date <- dat %>%
        dplyr::slice_min(dates, n = 1) %>%
        dplyr::select(dates) %>%
        dplyr::pull() %>%
        print()

      dat <- dat %>%
        dplyr::mutate(
          compare_date = dates %m-% k,
          found = compare_date %in% dates,
          in_range = compare_date >= min_date)


      # while(sum(dat$check) + out_of_range != length(dat$dates)){
#
#       for(i in 1:5){
#
#         dat_yes <- dat %>% dplyr::filter(found | !in_range)
#         dat_no <- dat %>% dplyr::filter(!found & in_range)
#
#         print(length(dat_no$dates))
#
#
#         dat_no <- dat_no %>%
#           dplyr::mutate(
#             compare_date = compare_date %>%
#               magrittr::subtract(lubridate::days(1)),
#             check = compare_date %in% dat$dates
#           )
#
#         dat <- rbind(dat_yes, dat_no)
#       }
#
#       print(dat_no)
#
#       # dat <- dat %>% filter(dates >= min_date)
#
#       return(dat)
#
#     }
#
#     if(stringr::str_detect(k, "lubridate::")){
#       dat <- dat %>%
#         dplyr::mutate(
#           compare_date = dates %>% magrittr::subtract(k),
#           check = compare_date %>% is.na() %>% as.numeric(),
#           compare_date = dates %>%
#             magrittr::subtract(k) %>%
#             magrittr::subtract(lubridate::days(check))
#           )
#
#         )
#
#     } else {
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
        tidyr::pivot_longer(cols      = -.data$variable,
                            names_to  = "dates",
                            values_to = "value") %>%
        dplyr::mutate(
          dates = .data$dates %>%
            stringr::str_remove_all("d") %>%
            lubridate::as_datetime(format = "%Y%m%d"),
          value = .data$value %>% magrittr::subtract(1)
        ) %>%
        dplyr::select(.data$dates, .data$variable, .data$value)
    }

  }


  return(dat)
}
