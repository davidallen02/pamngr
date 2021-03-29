#' Get Data
#'
#' Retrieves data from either a .xlsx or Bloomberg API and returns a tibble
#'
#' @param ticker character
#' @param type character
#' @param path character
#' @param flds character
#' @param start_date character
#' @param names character
#'
#' @return
#' A tibble with a datetime first column
#' @importFrom magrittr %>%
#'
#'
#' @export

get_data <- function(ticker,
                     type = "Index",
                     path = "data.xlsx",
                     flds = "PX_LAST",
                     start_date = "2000-01-01",
                     names = NA){


  machine <- Sys.info() %>% magrittr::extract2("nodename")

  if(machine %in% c("BBDA","BBJW")){

    ticker_full <- ticker %>%
      stringr::str_replace_all("-", " ") %>%
      stringr::str_to_upper() %>% paste(type)

    Rblpapi::blpConnect()

    dat <- Rblpapi::bdh(
      securities = ticker_full,
      fields     = flds,
      start.date = start_date %>% as.Date()
    ) %>%
      magrittr::set_colnames(c("dates", flds))%>%
      tibble::as_tibble() %>%
      dplyr::mutate(dates = .data$dates %>% lubridate::as_datetime())

    file_name <- ifelse(type == "Index", ticker, flds) %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all(" ", "-") %>%
      paste0(".RDS")

    saveRDS(dat, paste0("./data/", file_name))

  } else {
    if(type == "Index"){

      file_name <- ticker %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" ", "-")

      dat <- readRDS(paste0("/Users/davidallen/Dropbox/pam/economics/eco-data/data/", file_name, ".RDS"))
    }

    if(type == "Equity"){

      ticker <- ticker %>% stringr::word() %>% stringr::str_to_lower()

      flds <- flds %>% stringr::str_to_lower() %>% stringr::str_replace_all("_", "-")

      file_name <- paste0("~/Dropbox/pam/asset-management/equities/eq-data/output/",
                          ticker, "-", flds,".RDS")
      dat <- readRDS(file_name)
    }
  }

  return(dat)
}
