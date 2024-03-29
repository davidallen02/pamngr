#' Get Data
#'
#' Retrieves data from either a .xlsx or Bloomberg API and returns a tibble
#'
#' @param ticker character
#' @param type character
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
        paste(stringr::str_to_title(type))

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

        saveRDS(dat, paste0(pamngr::get_path(), "data/eco-data/", file_name, ".RDS"))
      }

      if(type == "Equity"){

        file_name <- ticker %>%
          paste0("-(", flds, ")") %>%
          stringr::str_replace_all("_", "-") %>%
          stringr::str_to_lower()

        saveRDS(dat, paste0(pamngr::get_path(), "data/eq-data/", file_name, ".RDS"))
      }
    } else {
      if(type == "Index"){

        field_name <- ifelse(flds == "PX_LAST","", paste0("-(", flds, ")")) %>%
          stringr::str_replace_all("_", "-")

        file_name <- ticker %>%
          paste0(field_name) %>%
          stringr::str_to_lower() %>%
          stringr::str_replace_all(" ", "-") %>%
          paste0(pamngr::get_path(), "data/eco-data/", ., ".RDS")

        dat <- readRDS(file_name)
      }


      if(type == "Equity"){

        ticker <- ticker %>% stringr::word()

        flds <- flds %>% paste0("-(", ., ")")

        file_name <- ticker %>%
          paste0(flds, ".RDS") %>%
          stringr::str_replace_all("_", "-") %>%
          stringr::str_to_lower()

        dat <- readRDS(file_name)
      }
    }
  }

  return(dat)
}
