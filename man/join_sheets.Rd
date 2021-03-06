% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join-sheets.R
\name{join_sheets}
\alias{join_sheets}
\title{Combine multiple securities}
\usage{
join_sheets(sheets)
}
\arguments{
\item{sheets}{a character vector of Bloomberg security names}
}
\value{
a tibble with a datetime first column followed by a column for each element of sheets
}
\description{
Join data for multiple securities into one tibble
}
