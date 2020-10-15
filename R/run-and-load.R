#' Title
#'
#' @param directory the project name where the script lives
#' @param script the script name
#' @param plot_name the plot name
#'
#' @return  a ggplot2 object
#' @export
#'
#'
run_and_load <- function(directory, script, plot_name = NA){

  pamngr::run_it(directory, script)

  current_wkdir <- getwd()  # save for later

  machine <- Sys.info() %>% magrittr::extract2("nodename")
  path <- ifelse(test = machine %in% c("BBJW", "BBDA"),
                 yes  = "R:/David/Economics/data/",
                 no   = "/users/davidallen/onedrive/pamgmt/economics/data/")

  plot_name <- ifelse(test = is.na(plot_name), yes = script, no = plot_name)
  path <- path %>% paste0(directory, "/output/tam/", plot_name, ".RData")

  load(path)

  return(p)
}
