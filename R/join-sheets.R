#' Combine multiple securities
#'
#' Join data for multiple securities into one tibble
#'
#' @param sheets a character vector of Bloomberg security names
#'
#' @return a tibble with a datetime first column followed by a column for each element of sheets
#' @export
#'
#' @examples
join_sheets <- function(sheets){

  dat <- pamngr::get_data(sheets[1])

  for(i in 2:length(sheets)){
    dat <- dat %>%
      dplyr::left_join(
        y = pamngr::get_data(sheets[i]),
        by = "dates"
      )
  }

  dat <- dat %>%
    magrittr::set_colnames(c("dates", sheets))

  return(dat)

}
