library(gridExtra)
#' Steal a legend
#'
#' @param myggplot a ggplot2 object
#'
#' @return a grob holding a legend
#'
#'
#'
get_legend<-function(myggplot){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]

  return(legend)
}
