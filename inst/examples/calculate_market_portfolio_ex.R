# Let's pick an example date for which to create an MP:
mp_date <- "2018-01-09"
# Feel free to change this date to any other date for which you have data.

# Use calculate_historical_returns() to get the daily returns observed for the
# 365 days ending on mp_date:
historical_rtn <- calculate_historical_returns(
  assets         = stock_data,
  date_range_xts = paste0(
    as.Date(mp_date) - 365,
    "/",
    mp_date
  )
)

# Note that a warning message is printed: Linde plc (NYSE: LIN) was the result
#   of a merger between Praxair (NYSE: PX) and Linde AG (FWB: LINU and FWB: LIN)
#   and therefore did not exist during the time range specified.

# We'll assume that the return we expect over the next year for each asset is
# the annualized GMMR of the daily rates of return over the past year:
exp_rtn <- log(1 + gmrr(historical_rtn))*252

# Assume that the volatilities we expect over the next year for each asset are
# the annualized daily vols we observed during the previous year:
exp_vol <- (
  dplyr::summarize(
    tibble::as_tibble(historical_rtn),
    dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
  ) %>%
    purrr::as_vector()
) * sqrt(252)

# Assume that the correlations of returns of each asset pair that we expect
# for the next year will be the same as the previous year:
exp_cor <- stats::cor(historical_rtn, use = "pairwise.complete.obs")

# Calculate the market portfolio:
mp_by_wt <- calculate_market_portfolio(exp_rtn, exp_vol, exp_cor)
mp_by_wt

### Repeat the above, assuming you have $250,000 to invest and that the stocks
### may be bought at their closing prices on mp_date. 
prices <- stock_data %>%
  lapply(function(stock){stock$prices$Close[mp_date]}) %>%
  unlist() %>%
  purrr::compact()
portfolio_aum <- 250000

mp_by_shares <- calculate_market_portfolio(
  exp_rtn,
  exp_vol,
  exp_cor,
  prices        = prices,
  portfolio_aum = portfolio_aum
)
mp_by_shares
