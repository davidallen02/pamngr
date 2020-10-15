#' Title
#'
#' @param directory the project name where the script lives
#' @param script the script name
#' @param type either "economics" or "equity"
#'
#' @return nothing. just sources the script
#' @export
#'

run_it <- function(directory, script, type = "economics"){

  current_wkdir <- getwd()  # save for later

  machine <- Sys.info() %>% magrittr::extract2("nodename")
  path <- ifelse(test = machine %in% c("BBJW", "BBDA"),
                 yes  = "R:/David/Economics/data/",
                 no   = "/users/davidallen/onedrive/pamgmt/economics/data/")

  dir_path <- path %>% paste0(directory)
  setwd(dir_path)

  script_path <- dir_path %>% paste0("/R/", script, ".R")
  source(script_path)

  setwd(current_wkdir)  # reset working directory
}
