#' Update data/key.RDS
#'
#' @param security Either a ticker or economic report name

update_key <- function(security){

  # search for security
  equities <- dir("C:/Users/David/Dropbox/pam/asset-management/equities")
  economics <- dir("C:/Users/David/Dropbox/pam/economics")

  if(security %in% equities){
    path <- "C:/Users/David/Dropbox/pam/asset-management/equities/"
    type <- "Equity"
  } else {
    if(security %in% economics){
      path <- "C:/Users/David/Dropbox/pam/economics"
      type <- "Index"
    } else{
      print(paste(security, "not in either equities or economics folders"))
    }
  }

  path <- path %>% paste0("/", security, "/data")

  Rblpapi::blpConnect()

  securities <- dir(path) %>%
    stringr::str_remove_all(".RDS") %>%
    stringr::str_replace_all("-", " ")

  securities <- securities[securities != "key"]

  securities %>%
    stringr::str_to_upper() %>%
    paste(type) %>%
    Rblpapi::bdp(fields = c("LONG_COMP_NAME", "INDX_SOURCE")) %>%
    tibble::rownames_to_column(var = "security") %>%
    dplyr::mutate(
      security = security %>%
        stringr::str_to_lower() %>%
        stringr::str_replace(" ", "-") %>%
        stringr::word() %>%
        stringr::str_remove_all("-index")) %>%

    saveRDS("./data/key.RDS")

}
