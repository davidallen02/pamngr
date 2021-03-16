buy_list_remove <- function(removing, type = "equity"){

  buy_list_set <- ifelse(type == "equity", "equities.csv", "etfs.csv")
  buy_list_set <- paste0(
    "/users/davidallen/dropbox/pam/projects/holdings-analysis/buy-list-",
    buy_list_set)

  removing <- removing %>% stringr::str_to_lower()

  dat <- readr::read_csv(file = buy_list_set, col_types = "c") %>%
    dplyr::filter(.data$ticker != removing) %>%
    readr::write_csv(file = buy_list_set)





}
