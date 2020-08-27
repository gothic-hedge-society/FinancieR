# Calculate the Sharpe-optimized market portfolio (MP) found for the stocks in 
# stock_data if built on 17 Jan 2019. Assume:
#   1) The return we expect over the next year for each asset is the annualized
#        GMMR of the daily rates of return over the past year.
#   2) The volatilities we expect over the next year for each asset are the
#        annualized daily vols we observed during the previous year.
#   3) The correlations of returns over the next year will be the same as they 
#        were for last year
#   4) Each asset may be acquired at the closing price on 17 Jan 2019.
#   5) We have $500,000 to invest.

historical_rtn <- calculate_historical_returns(
  assets         = stock_data,
  date_range_xts = paste0(as.Date("2019-01-17") - 365, "/2019-01-17")
)

portfolio_2019_01_17 <- calculate_market_portfolio(
  exp_rtn       = log(1 + gmrr(historical_rtn))*252,
  exp_vol       = (
    dplyr::summarize(
      tibble::as_tibble(historical_rtn),
      dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
    ) %>%
      purrr::as_vector()
  ) * sqrt(252),
  exp_cor       = stats::cor(historical_rtn, use = "pairwise.complete.obs"),
  prices        = stock_data %>%
    lapply(function(stock){stock$prices$Close["2019-01-17"]}) %>%
    unlist() %>%
    purrr::compact(),
  portfolio_aum = 500000
)

