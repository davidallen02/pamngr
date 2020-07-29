#' Formatted ggplot2 line plot
#'
#' @param df
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
lineplot <- function(df){

  p <- df %>%
    ggplot2::ggplot(ggplot2::aes("dates", "value", color = "variable")) +
    ggplot2::geom_line(size = 2) +
    ggplot2::scale_color_manual(values = pam.pal())


  return(p)
}
