#' Update all data in a directory
#'
#' @param security Blomberg security ID for equity or economic index

update_data <- function(security){

  # search for security
  equities <- dir("R:/David/asset-management/equities")
  economics <- dir("R:/David/economics/data")

  if(security %in% equities){
    path <- "R:/David/asset-management/equities/"
  } else {
    if(security %in% economics){
      path <- "R:/David/economics/data"
    } else{
      print(paste(security, "not in either equities or economics folders"))
    }
  }

  path <- path %>% paste0("/", security, "/data")
  securities <- dir(path) %>% stringr::str_remove_all(".RDS")
  securities <- securities[securities != "key"]

  securities %>% purrr::map(pamngr::get_data)

}
