#' Create a formatted bar plot
#'
#' Creates a bar plot that is formatted for uniformity across output types
#'
#' @param df a data.frame() likely created using a tibble from either get_data() or join_sheets() that is then passed to reshape2::melt(id.vars = "dates")
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
barplot <- function(df){

  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(dates, value, fill = variable)) +
    ggplot2::geom_bar(stat = 'identity', position = 'dodge') +
    ggplot2::scale_fill_manual(values = pam.pal())
  return(p)
}
