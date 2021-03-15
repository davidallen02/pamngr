#' Create a formatted bar plot
#'
#' Creates a bar plot that is formatted for uniformity across output types
#'
#' @param df a data.frame() likely created using a tibble from either get_data() or join_sheets() that is then passed to reshape2::melt(id.vars = "dates")
#'
#' @param x a character indicating the x axis value
#'
#' @param y a character indicating the y axis value
#'
#' @param fill a character indicating the fill color
#'
#' @param ... dots, dots, dots
#' @return a ggplot2 object
#' @export
#'

barplot <- function(df, x = "dates", y = "value",  fill = "variable", ...) {
  p <- df %>%
    ggplot2::ggplot(ggplot2::aes_string(x = x, y = y, fill = fill)) +
    ggplot2::geom_bar(stat = "identity", ...) +
    ggplot2::scale_fill_manual(values =  pam.pal())

  return(p)
}
