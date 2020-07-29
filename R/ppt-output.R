#' Title
#'
#' @param p a ggplot2 object
#' @param image_name character
#'
#' @return Saves a .png
#' @export
#'
#'
ppt_output <- function(p, image_name){
  ggplot2::ggsave(
    filename = image_name,
    plot     = p,
    width    = 13.33,
    height   = 6.75,
    units    = 'in'
  )
}
