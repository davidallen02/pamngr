# securities <- dir("data/") %>%
#   stringr::str_replace_all(".RDS","") %>%
#   stringr::str_replace_all("-"," ") %>%
#   stringr::str_to_upper() %>%
#   paste("Index")
#
# flds <- c("LONG_COMP_NAME",
#           "INDX_SOURCE")
#
# Rblpapi::blpConnect()
#
# Rblpapi::bdp(securities, flds) %>%
#   tibble::rownames_to_column(var = "security") %>%
#   dplyr::mutate(
#     security = security %>% stringr::word() %>% stringr::str_to_lower()
#   ) %>%
#   saveRDS(file = "data/key.RDS")
