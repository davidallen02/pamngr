#' Creates a PAM formatted ggplot object
#'
#' @param p a ggplot object
#' @param x_lab string
#' @param y_lab string
#' @param plot_title string
#' @param plot_subtitle string
#' @param caption logical
#' @param axis_titles logical
#' @param y_format string
#' @param show_legend logical
#' @param faceted string
#'
#' @return a ggplot object
#' @export
#'
#'
pam_plot <- function(p,
                     x_lab = NA,
                     y_lab = NA,
                     plot_title = NA,
                     plot_subtitle = NA,
                     caption = TRUE,
                     axis_titles = FALSE,
                     y_format = "comma",
                     show_legend = TRUE,
                     faceted = NA){

  # Plot title ------------------------------------------------------------------------

  if(is.na(plot_title)){
    p <- p + ggplot2::theme(plot_title = ggplot2::element_blank())
  } else {
    p <- p +
      ggplot2::labs(title = plot_title) +
      ggplot2::theme(
        plot_title = ggplot2::element_text(
          size    = ggplot2::rel(3.25),
          face    = 'bold'
        )
      )
  }

  # Plot subtitle ---------------------------------------------------------------------

  if(is.na(plot_subtitle)){
    p <- p + ggplot2::theme(plot_subtitle = ggplot2::element_blank())
  } else {
    p <- p +
      ggplot2::labs(subtitle  = plot_subtitle) +
      ggplot2::theme(plot_subtitle = ggplot2::element_text(size = ggplot2::rel(2)))
  }


  # Caption ---------------------------------------------------------------------------

  if(caption){
    p <- p + ggplot2::labs(
      caption = paste('Created', format(Sys.Date(), '%B %d, %Y'))
    )
  }

  # Legend ----------------------------------------------------------------------------

  if(show_legend){
    p <- p +
      ggplot2::theme(
        legend.position = 'bottom',
        legend.title    = ggplot2::element_blank(),
        legend.text     = ggplot2::element_text(size = ggplot2::rel(1.5))
      )
  } else {
    p <- p + ggplot2::theme(legend.position = 'none')
  }


  # Axis titles -----------------------------------------------------------------------

  if(isFALSE(axis_titles)){
    p <- p + ggplot2::theme(axis.title = ggplot2::element_blank())
  } else {
    p <- p +
      ggplot2::labs(x = x_lab, y = y_lab) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = ggplot2::rel(1.5))
      )
  }


  # Axis text -------------------------------------------------------------------------
  if(y_format == "comma"){
    p <- p + ggplot2::scale_y_continuous(labels = scales::comma)
  }

  p <- p +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = ggplot2::rel(1.5))
    )

  # Facetting -------------------------------------------------------------------------

  p <- p +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = ggplot2::rel(1.5))
    )

  # if(!is.na(faceted)){
  #   if(faceted == 'row'){
  #     p <- p + ggplot2::facet_grid(row = ggplot2::vars(variable))
  #   } else {
  #     if(faceted  == 'col'){
  #       p <- p + ggplot2::facet_grid(col = ggplot2::vars(variable))
  #     }
  #   }
  #
  # }


  return(p)


}
