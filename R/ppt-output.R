#' Title
#'
#' @param p a ggplot2 object
#' @param image_name character
#' @param save logical
#'
#' @return Saves a .png
#' @export
#'
#'
ppt_output <- function(p, image_name, save = TRUE){

  if(save){save(p, file = paste0("plots/", image_name))}

  p <- p +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(3.25), face = 'bold'),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(2)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
      strip.text = ggplot2::element_text(size = ggplot2::rel(1.5))
    )

  ggplot2::ggsave(
    filename = paste0("output/ppt/", image_name, ".png"),
    plot     = p,
    width    = 13.33,
    height   = 6.75,
    units    = 'in'
  )
}
