#' Title
#'
#' @param ticker if NA returns the entire recommendations dataset. If ticker is given returns recommendations for the desired security
#'
#' @return a tibble
#'
#'
get_recommendations <- function(ticker = NA){

  machine <- Sys.info()["nodename"]

  if(machine %in% c("BBDA", "BBJW")){
    path <- "R:/David/"
  } else{
    path <- "~/onedrive/pamgmt/"
  }

  path <- path %>% paste0("projects/recommendations-and-ranges/data/recommendations.txt")

  dat <- readr::read_csv(file = path, col_types = "cDnnnnnl")

  if(!is.na(ticker)){dat <- dat %>% dplyr::filter(.data$TICKER == ticker)}

  return(dat)
}
