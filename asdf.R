
rm(list=ls())

# Use calculate_returns() to get the daily returns observed for the 365 days
# ending on mp_date:
historical_rtn <- calculate_returns(
  assets         = stock_data,
  date_range_xts = paste0(
    as.Date("2018-01-09") - 12,
    "/",
    "2018-01-09"
  )
)

# # We'll assume that the return we expect over the next year is the annualized 
# # GMMR of the daily rates of return in historical_rtn:
# exp_rtn <- gmrr(historical_rtn)
# 
# # Assume that the volatilities we expect for the next year are the annualized
# # daily vols we observed during the previous year:
# exp_vol <- dplyr::summarize(
#   tibble::as_tibble(historical_rtn),
#   dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
# ) %>%
#   purrr::as_vector()
# 
# # Assume that the correlations of returns of each asset pair that we expect
# # for the next year will be the same as the previous year:
# exp_cor <- stats::cor(historical_rtn, use = "pairwise.complete.obs")
# 
# ### Repeat the above, assuming you have $250,000 to invest and that the stocks
# ### may be bought at their closing prices on mp_date:
# 
# prices <- stock_data %>%
#   vapply(
#     function(stock){
#       stock$prices$Close[mp_date]
#     },
#     numeric(1)
#   )
# portfolio_aum <- 250000
# 
# mp_by_shares <- calculate_market_portfolio(
#   exp_rtn,
#   exp_vol,
#   exp_cor,
#   prices        = prices,
#   portfolio_aum = portfolio_aum
# )
# mp_by_shares
