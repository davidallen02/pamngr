#' Formatted ggplot2 line plot
#'
#' @param df a data.frame with column names consistent with output from reshape2::melt()
#'
#' @return ggplot2 object
#' @export
#'
#'
lineplot <- function(df){

  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(.data$dates, .data$value, color = .data$value)) +
    ggplot2::geom_line(size = 2) +
    ggplot2::scale_color_manual(values = pam.pal())


  return(p)
}
