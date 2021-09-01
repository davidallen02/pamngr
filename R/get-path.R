#' Get path to PAM Research Dropbox path
#'
#' @return
#' A character string to the PAM Research Dropbox path specific to machine
#' @export
#' @importFrom magrittr %>%
#'
#' @export


get_path <- function(){

  machine <- Sys.info() %>% magrittr::extract2("nodename")

  if(machine == "BBDA"){path <- "C:/Users/David/PAM Research Dropbox/"}

  if(machine == "Davids-Macbook-Pro.local"){path <- "~/PAM Research Dropbox/"}

  return(path)

}
