#' Get Data
#'
#' Retrieves data from either a .xlsx or Bloomberg API and returns a tibble
#'
#' @param ticker character
#' @param path character
#' @param flds character
#' @param start_date character
#' @param names character
#'
#' @return
#' A tibble with a datetime first column
#' @importFrom magrittr %>%
#' @export

get_data <- function(ticker,
                     path = "data.xlsx",
                     flds = "PX_LAST",
                     start_date = "2000-01-01",
                     names = NA){

  # requireNameSpace(magrittr)

  # machine <- Sys.info() %>% extract2("nodename")
  #
  # if(machine %in% c("BBDA","BBJW")){
  #
  #   ticker <- ticker %>% stringr::str_to_upper() %>% paste("Index")
  #
  #   Rblpapi::blpConnect()
  #
  #   dat <- Rblpapi::bdh(
  #     securities = ticker,
  #     fields = flds,
  #     start.date = start_date %>% as.Date()
  #   ) %>%
  #     magrittr::set_colnames(c("dates", flds))%>%
  #     tibble::as_tibble() %>%
  #     dplyr::mutate(
  #       dates = dates %>% lubridate::as_datetime()
  #     )
  #
  #   return(dat)
  #
  # } else {
    if(is.na(names)){names <- "value"}
    if(names == "match"){names <- ticker}

    dat <- readxl::read_excel(path = path, sheet = ticker, skip = 4, na = "#N/A N/A") %>%
      dplyr::select(c("Dates", flds)) %>%
      magrittr::set_colnames(c("dates", flds))

    ticker %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all(" ", "-") %>%
      save(dat, file = paste0("data/", ., ".RData"))

    return(dat)
  # }


}
