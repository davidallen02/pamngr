#' Price charts with recommendation history
#'
#' @param ticker a string
#'
#' @import reshape2
#' @return a ggplot2 object
#'
#'
#'
recommendation_plot <- function(ticker) {

  # Read in complete recommendation for security
  recommendations <- readr::read_csv(
    file = "~/onedrive/pamgmt/projects/recommendations-and-ranges/data/recommendations.txt",
    col_types = "cDnnnnnl") %>%
    dplyr::filter(.data$TICKER == ticker)

  # Prepare recommendation range history for plotting
  ranges <- recommendations %>%
    dplyr::select(-c(.data$TICKER, .data$PRICE_TARGET)) %>%
    dplyr::arrange(.data$DATE) %>%
    dplyr::mutate(start = .data$DATE,
                  end   = c(.data$DATE[-1], Sys.Date())) %>%
    dplyr::select(-.data$DATE) %>%
    reshape2::melt(id.vars = c('start', 'end', 'SOURCE')) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate(variable = paste(.data$SOURCE, .data$variable, sep='_'))

  # Save recommendation history start date
  stdt <- recommendations %>%
    utils::head(1) %>%
    dplyr::select(.data$DATE) %>%
    dplyr::pull()

  # Prepare PAM internal price target data for plotting
  pam_pt <- recommendations %>%
    dplyr::select(.data$DATE, .data$PRICE_TARGET) %>%
    dplyr::bind_rows(data.frame('DATE' = Sys.Date(),
                                'PRICE_TARGET' = recommendations %>%
                                  dplyr::select(.data$PRICE_TARGET) %>%
                                  utils::tail(1))) %>%
    dplyr::arrange(dplyr::desc(.data$DATE))

  # Read in ticker's price and consensus price target historical data
  px_last <- pamngr::get_data(ticker, type = "Equity", flds = "px-last")
  best_target_price <- pamngr::get_data(ticker, type = "Equity", flds = "best-target-price")

  # Join price and  consensus price target data
  prices <- px_last %>%
    dplyr::left_join(best_target_price, by = "dates") %>%
    dplyr::filter(.data$dates >= stdt) %>%
    tidyr::pivot_longer(cols = -.data$dates, names_to = "variable") %>%
    tidyr::drop_na(.data$value) %>%
    dplyr::mutate(dates = .data$dates %>% as.Date())

  # Prepare earnings announcement dates data for plotting
  earnings <- pamngr::get_data(ticker, type = "Equity", flds = "announcement-dt") %>%
    dplyr::mutate(dates = .data$announcement_dt %>% as.character() %>% as.Date(format = "%Y%m%d")) %>%
    dplyr::select(.data$dates) %>%
    dplyr::filter(.data$dates >= stdt) %>%
    dplyr::left_join(px_last, by = "dates") %>%
    dplyr::mutate(dates = .data$dates %>% as.Date())

  # Prepare earnings announcement dates data for plotting
  # earnings <- pamngr::get_data(ticker, type = "Equity", flds = "announcement-dt") %>%
  #   dplyr::mutate(dates = .data$announcement_dt) %>%
  #   dplyr::select(.data$dates) %>%
  #   dplyr::filter(.data$dates >= stdt) %>%
  #   dplyr::left_join(px_last, by = "dates") %>%
  #   dplyr::mutate(dates = .data$dates %>% as.Date())

  # Plot recommendation range history
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = ranges,
                          ggplot2::aes(x     = .data$start,
                                       xend  = .data$end,
                                       y     = .data$value,
                                       yend  = .data$value,
                                       color = .data$variable),
                          size  = 2,
                          alpha = 1/3)

  # Add internal price target data to plot
  p <- p + ggplot2::geom_step(data = pam_pt,
                              ggplot2::aes(.data$DATE, .data$PRICE_TARGET),
                              color = '#850237',
                              size  = 1)

  # Add price and consensus price target lines to plot
  p <- p + ggplot2::geom_line(
    data = prices,
    ggplot2::aes(x = .data$dates, y = .data$value, color = .data$variable),
    size = 1)

  # Add earnings dates to plot
  p <- p +
    ggplot2::geom_point(
      data = earnings,
      ggplot2::aes(x = .data$dates, y= .data$px_last),
      shape = 23,
      size = 4,
      fill = "red") +
    ggplot2::geom_text(
      data = earnings,
      ggplot2::aes(x = .data$dates, y= .data$px_last),
      label = "E",
      color = "white",
      size = 3)


  # Define color palette
  p <- p +
    ggplot2::scale_color_manual(
      values = c(FALSE_EVAL_LO           = 'blue',
                 FALSE_EVAL_HI           = 'blue',
                 TRUE_EVAL_LO            = 'red',
                 TRUE_EVAL_HI            = 'red',
                 FALSE_BUY_LO            = 'green',
                 FALSE_BUY_HI            = 'green',
                 px_last                 = 'black',
                 best_target_price       = 'gold'))

  # Format plot area
  p <- p %>%
    pamngr::pam_plot(
      plot_title  = paste(stringr::str_to_upper(ticker), "Recommendation History"),
      show_legend = FALSE,
      caption     = FALSE)

  return(p)
}

