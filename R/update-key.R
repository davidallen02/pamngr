#' Update data/key.RDS
#'
#' @param security Either a ticker or economic report name
update_key <- function(security){

  # search for security
  equities <- dir("R:/David/asset-management/equities")
  economics <- dir("R:/David/economics/data")

  if(security %in% equities){
    path <- "R:/David/asset-management/equities/"
    type <- "Equity"
  } else {
    if(security %in% economics){
      path <- "R:/David/economics/data"
      type <- "Index"
    } else{
      print(paste(security, "not in either equities or economics folders"))
    }
  }

  path <- path %>% paste0(security, "/data")

  Rblpapi::blpConnect()

  securities <- dir(path) %>% stringr::str_remove_all(".RDS")
  securities <- securities[securities != "key"]

  securities %>%
    stringr::str_to_upper() %>%
    paste(type) %>%
    Rblpapi::bdp(fields = c("LONG_COMP_NAME", "INDX_SOURCE")) %>%
    tibble::rownames_to_column(var = "security") %>%
    dplyr::mutate(
      security = security %>% stringr::str_to_lower() %>% stringr::word()) %>%
    saveRDS("./data/key.RDS")

}
