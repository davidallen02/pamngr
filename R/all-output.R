#' Title
#'
#' @param p ggplot2 object
#' @param image_name string
#' @param save logical
#'
#' @return some plots! saved where you want them
#' @export
#'
#'
all_output <- function(p, image_name, save = TRUE){

  pamngr::ppt_output(p, image_name, save)
  pamngr::tam_output(p, image_name, save = FALSE)


}
