
exchange_hours <- xml2::read_html(
  "https://en.wikipedia.org/wiki/List_of_stock_exchange_trading_hours"
) %>%
  rvest::html_node("body") %>%
  rvest::html_node("#content") %>%
  rvest::html_node("#bodyContent") %>%
  rvest::html_node("#mw-content-text") %>%
  rvest::html_node(".wikitable") %>%
  rvest::html_table() %>% {
    .[1,grep("^Local", colnames(.))] <- paste0(
      "local_",
      .[1,grep("^Local", colnames(.))]
    )
    .[1,grep("^UTC", colnames(.))] <- paste0(
      "UTC_",
      .[1,grep("^UTC", colnames(.))]
    )
    colnames(.) <- .[1,]
    . <- .[-1,]
    .
  } %>%
  tibble::as_tibble()

usethis::use_data(exchange_hours, overwrite = TRUE)
