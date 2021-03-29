#' Create a new eval from template
#'
#' @param ticker an equity security
#' @param date the eval date
#'
#' @return a markdown document

new_eval <- function(ticker, date = Sys.Date()){

  date <- date %>% as.Date()
  date_file <- date %>% format("%Y%m%d")

  direct_path <- "/Users/davidallen/OneDrive/PAMGMT/asset-management/equities/equity-research/" %>%
    paste0(ticker, "/")
  date_eval <- date %>% format("%B %d, %Y") %>% paste0("  ")

  company_name <- pamngr::get_data(ticker, type = "Equity", flds = "long-comp-name") %>%
    dplyr::pull()
  if(stringr::str_detect(company_name, "/The")){
    company_name <- company_name %>% stringr::str_remove("/The")
    company_name <- paste("The", company_name)
  }
  company_name <- paste0("**", company_name, " (", stringr::str_to_upper(ticker), ")**  ")

  recommendation <- "Buy \\$XX - \\$XX, Eval \\$XX & \\$XX  "
  price_target <- "Price Target: \\$XX  "
  rec_plot <- paste0("![](", "../images/", date_file, "-", ticker, "-recommendation-plot.png)")
  double_space <- "  "

  p <- pamngr::recommendation_plot(ticker)
  ggplot2::ggsave(filename = paste0(ticker,
                                    "/images/",
                                    date_file,
                                    "-",
                                    ticker,
                                    "-recommendation-plot.png"),
                  plot = p,
                  width = 6.5,
                  height = 2.5,
                  units = "in")

  eval_name <- paste(date_file, ticker, "eval.md", sep = "-")
  eval_path <- paste0(direct_path, "evals/", eval_name)

  c(date_eval,
    double_space,
    company_name,
    recommendation,
    price_target,
    double_space,
    rec_plot) %>%
    readr::write_lines(file = eval_path)

}
