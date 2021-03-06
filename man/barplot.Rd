% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bar-plot.R
\name{barplot}
\alias{barplot}
\title{Create a formatted bar plot}
\usage{
barplot(df, x = "dates", y = "value", fill = "variable", ...)
}
\arguments{
\item{df}{a data.frame() likely created using a tibble from either get_data() or join_sheets() that is then passed to reshape2::melt(id.vars = "dates")}

\item{x}{a character indicating the x axis value}

\item{y}{a character indicating the y axis value}

\item{fill}{a character indicating the fill color}

\item{...}{dots, dots, dots}
}
\value{
a ggplot2 object
}
\description{
Creates a bar plot that is formatted for uniformity across output types
}
