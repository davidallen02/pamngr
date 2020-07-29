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
