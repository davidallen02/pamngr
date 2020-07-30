#' Formats charts for use in Tamarac Economic & Market Commentary
#'
#' @param p ggplot2 object
#' @param image_name string
#' @param save logical
#'
#' @return ggplot2 object
#' @export
#'
#'
tam_output <- function(p, image_name, save = TRUE){

  if(save){save(p, file = paste0("plots/", image_name))}

  p <- p +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1), face = "bold"),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(.9)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(.8)),
      axis.title = ggplot2::element_text(size = ggplot2::rel(.8)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(.8)),
      strip.text = ggplot2::element_text(size = ggplot2::rel(.8))
    )

  save(p, file = paste0("output/tam/", image_name, ".RData"))


}
