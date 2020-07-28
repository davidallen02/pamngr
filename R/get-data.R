get_data <- function(ticker,
                     path = "data.xlsx",
                     flds = "PX_LAST",
                     start_date = "2000-01-01",
                     names = NA){

  library(magrittr)

  machine <- Sys.info() %>% extract2("nodename")

  if(machine %in% c("BBDA","BBJW")){

    ticker <- ticker %>% stringr::str_to_upper() %>% paste("Index")

    Rblpapi::blpConnect()

    dat <- Rblpapi::bdh(
      securities = ticker,
      fields = flds,
      start.date = start_date %>% as.Date()
    ) %>%
      magrittr::set_colnames(c("dates", flds))%>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        dates = dates %>% lubridate::as_datetime()
      )

    return(dat)

  } else {
    if(is.na(names)){names <- "value"}
    if(names == "match"){names <- ticker}

    dat <- readxl::read_excel(path = path, sheet = ticker, skip = 4, na = "#N/A N/A") %>%
      dplyr::select(c("Dates", flds)) %>%
      magrittr::set_colnames(c("dates", flds))


    return(dat)
  }
}