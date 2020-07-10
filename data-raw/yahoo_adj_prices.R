## code to prepare `yahoo_adj_prices` dataset goes here

readxl::read_excel(
  "./inst/portfolio_optimization_10stocks_1ETF.xlsx",
  sheet =  "1) Raw Prices",
  skip  = 2
)[,1:12] %>%
  tibble::column_to_rownames("Date") %>%
  xts::as.xts()

usethis::use_data(yahoo_adj_prices, overwrite = TRUE)
