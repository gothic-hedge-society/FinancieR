# Use the sample dataset "yahoo_adj_prices", provided with the package, for
# this example. Start with the assumption that the returns that we expect for
# the next period (day) are the means of the historical log returns observed
# for a given date window specified by "start_dt" and "end_dt"
end_dt   <- as.Date(zoo::index(xts::last(yahoo_adj_prices))) # latest date
start_dt <- end_dt - 365*3                                   # 3-yr window
end_dt
start_dt

# From adjusted prices, get the observed daily historical log return
historical_rtn <- yahoo_adj_prices[paste0(start_dt - 1, "/", end_dt)] %>% {
  log(.[-1,] / lag(., k =  1, na.pad = FALSE)[-nrow(.),])
}

# Assume that the returns we expect for the next period (day) are the
# averages of the returns observed during the date window:
exp_rtn <- colMeans(historical_rtn)

# Assume that the volatilities we expect for the next period (day) are the
# standard deviations of the returns observed during the date window:
exp_vol <- dplyr::summarize(
  tibble::as_tibble(historical_rtn),
  dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
) %>%
  purrr::as_vector()

# Assume that the correlations of returns of each asset pair that we expect
# for the next perid (day) are the observed historical correlations:
exp_cor <- stats::cor(historical_rtn, use = "pairwise.complete.obs")

# Calculate the market portfolio:
calculate_market_portfolio(exp_rtn, exp_vol, exp_cor)

### Calculate the market portfolio allowing both long & short positions:
calculate_market_portfolio(exp_rtn, exp_vol, exp_cor, allow_shorts = TRUE)