#' Title
#'
#' @param ticker a string
#'
#' @import reshape2
#' @return a ggplot2 object
#'
#'
#'
recommendation_plot <- function(ticker) {

  # library(magrittr)

  get("recommendations")

  px_last <- pamngr::get_data(ticker, type = "Equity", flds = "PX_LAST")
  # best_target_price <- pamngr::get_data(ticker, type = "Equity", flds = "BEST_TARGET_PRICE")

  path_to_data <- paste0("~/onedrive/pamgmt/asset-management/equities/", ticker, "/data.xlsx")

  stdt <- recommendations %>%
    dplyr::filter(.data$TICKER == ticker) %>%
    dplyr::slice_min(.data$DATE, 1) %>%
    dplyr::select(.data$DATE) %>%
    dplyr::pull()

  p <- ggplot2::ggplot() +

    ggplot2::geom_segment(data = (recommendations %>%
                                    dplyr::filter(.data$TICKER == ticker) %>%
                                    dplyr::select(-c(.data$TICKER,
                                                     .data$PRICE_TARGET)) %>%
                                    dplyr::arrange(.data$DATE) %>%
                                    dplyr::mutate(start = .data$DATE,
                                                  end   = c(.data$DATE[-1],
                                                            Sys.Date())) %>%
                                    dplyr::select(-.data$DATE) %>%
                                    reshape2::melt(id.vars = c('start', 'end', 'SOURCE')) %>%
                                    dplyr::filter(!is.na(.data$value)) %>%
                                    dplyr::mutate(variable = paste(.data$SOURCE, .data$variable, sep='_'))),
                          ggplot2::aes(x     = .data$start,
                                       xend  = .data$end,
                                       y     = .data$value,
                                       yend  = .data$value,
                                       color = .data$variable),
                          size  = 2,
                          alpha = 1/3) +

    ggplot2::geom_step(data = (recommendations %>%
                                 dplyr::filter(.data$TICKER == ticker) %>%
                                 dplyr::select(.data$DATE, .data$PRICE_TARGET) %>%
                                 dplyr::bind_rows(data.frame('DATE' = Sys.Date(),
                                                             'PRICE_TARGET' = recommendations %>%
                                                               dplyr::filter(.data$TICKER == ticker) %>%
                                                               dplyr::select(.data$PRICE_TARGET) %>%
                                                               utils::tail(1))) %>%
                                 dplyr::arrange(dplyr::desc(.data$DATE))),
                       ggplot2::aes(.data$DATE, .data$PRICE_TARGET),
                       color = '#850237',
                       size  = 1)

  p <- p + ggplot2::geom_line(data = (
    readxl::read_excel(path_to_data, sheet = "px_last", skip = 4) %>%
      magrittr::set_colnames(c("dates","PX_LAST")) %>%
      reshape2::melt(id.vars = "dates") %>%
      dplyr::mutate(DATE = .data$dates %>% as.Date()) %>%
      dplyr::filter(.data$DATE >= stdt)),
    ggplot2::aes(x = .data$DATE, y = .data$value, color = .data$variable),
    size = 1) +
    ggplot2::geom_line(data = (
      readxl::read_excel(path_to_data,
                         sheet = "best_target_price",
                         skip = 4,
                         na = "#N/A N/A") %>%
        magrittr::set_colnames(c("dates", "BEST_TARGET_PRICE")) %>%
        reshape2::melt(id.vars = "dates") %>%
        dplyr::mutate(DATE = .data$dates %>% as.Date()) %>%
        dplyr::filter(.data$DATE >= stdt)),
      ggplot2::aes(x = .data$DATE, y = .data$value, color = .data$variable),
      size = 1)


  p <- p +

    ggplot2::scale_color_manual(
      values = c(FALSE_EVAL_LO           = 'blue',
                 FALSE_EVAL_HI           = 'blue',
                 TRUE_EVAL_LO            = 'red',
                 TRUE_EVAL_HI            = 'red',
                 FALSE_BUY_LO            = 'green',
                 FALSE_BUY_HI            = 'green',
                 PX_LAST                 = 'black',
                 BEST_TARGET_PRICE       = 'gold')) +

    ggplot2::ggtitle(paste(stringr::str_to_upper(ticker), 'Recommendation & Range History')) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "none",
      axis.title = ggplot2::element_blank()
    )

  p <- p %>%
    pamngr::pam_plot(
      plot_title = paste(stringr::str_to_upper(ticker), "Recommendation History"),
      show_legend = FALSE,
      caption = FALSE
    )

  return(p)
}

