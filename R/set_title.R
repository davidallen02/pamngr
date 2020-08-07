#' Set title of Economic & Market Commentary page
#'
#' @param title a character
#'
#' @return a text grob
#'
set_title <- function(title){
  title <- grid::textGrob(
    title,
    x    = 0,
    y    = 0.5,
    just = "left",
    gp   = grid::gpar(
      fontface = "bold",
      fontsize = 36,
      col      = "#850237"))

  return(title)
}

