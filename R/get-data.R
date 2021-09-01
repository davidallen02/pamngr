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
if(ticker == "key"){
  if(Sys.info()["nodename"] == "BBDA"){
    dat <- readRDS("C:/Users/David/PAM Research Dropbox/David/data/eco-data/key.RDS")
  }

  if(Sys.info()["nodename"] == "Davids-Macbook-Pro.local"){
    dat <- readRDS("~/dropbox/work/pam/economics/eco-data/data/key.RDS")
  }
} else{

  machine <- Sys.info() %>% magrittr::extract2("nodename")

  if(machine %in% c("BBDA","BBJW")){

    ticker_full <- ticker %>%
      stringr::str_replace_all("-", " ") %>%
      stringr::str_to_upper() %>%
      paste(type)

    field_full <- flds %>%
      stringr::str_replace_all("-", "_") %>%
      stringr::str_to_upper()

    Rblpapi::blpConnect()

    dat <- Rblpapi::bdh(
      securities = ticker_full,
      fields     = field_full,
      start.date = start_date %>% as.Date()
    ) %>%
      magrittr::set_colnames(c("dates", flds))%>%
      tibble::as_tibble() %>%
      dplyr::mutate(dates = .data$dates %>% lubridate::as_datetime())

    if(type == "Index"){

      field_name <- ifelse(flds == "PX_LAST",
                           "",
                           paste0("-(", flds, ")")) %>%
        stringr::str_replace_all("_", "-")

      file_name <- ticker %>%
        paste0(field_name) %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" ", "-")

      saveRDS(dat, paste0("C:/Users/David/Dropbox/work/pam/economics/eco-data/data/", file_name, ".RDS"))
      saveRDS(dat, paste0("C:/Users/David/PAM Research Dropbox/David/data/eco-data/", file_name, ".RDS"))
    }

    if(type == "Equity"){

      ticker <- ticker %>% stringr::word() %>% stringr::str_to_lower()

      flds <- flds %>% stringr::str_to_lower() %>% stringr::str_replace_all("_", "-")

      file_name <- paste0("C:/Users/David/Dropbox/work/pam/asset-management/equities/eq-data/output/",
                          ticker, "-", flds,".RDS")
      dat <- readRDS(file_name)
    }

  } else {
    if(type == "Index"){

      field_name <- ifelse(flds == "PX_LAST",
                           "",
                           paste0("-(", flds, ")")) %>%
        stringr::str_replace_all("_", "-")

      file_name <- ticker %>%
        paste0(field_name) %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" ", "-")

      dat <- readRDS(paste0("/Users/davidallen/PAM Research Dropbox/David/data/eco-data/",
                            file_name,
                            ".RDS"))
    }

    if(type == "Equity"){

      ticker <- ticker %>%
        stringr::word() %>%
        stringr::str_to_lower()

      flds <- flds %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all("_", "-")

      file_name <- paste0("~/Dropbox/work/pam/asset-management/equities/eq-data/output/",
                          ticker,
                          "-",
                          flds,
                          ".RDS")

      dat <- readRDS(file_name)
    }
  }
}

  return(dat)
}
