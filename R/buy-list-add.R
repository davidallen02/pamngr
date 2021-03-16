#' Add securities to Buy List
#'
#' @param addition the ticker of the security to add
#' @param type the type of security
#'
#' @return none. writes to file

buy_list_add <- function(addition, type = "equity"){

  buy_list_set <- ifelse(type == "equity", "equities.csv", "etfs.csv")
  buy_list_set <- paste0(
    "/users/davidallen/dropbox/pam/projects/buy-list/buy-list-",
    buy_list_set)

  dat <- readr::read_csv(buy_list_set, col_types = "c") %>%
    dplyr::select(.data$ticker) %>%
    dplyr::pull() %>%
    c(addition) %>%
    stringr::str_to_lower() %>%
    stringr::str_sort() %>%
    tibble::tibble() %>%
    magrittr::set_colnames("ticker") %>%
    readr::write_csv(file = buy_list_set)

}
