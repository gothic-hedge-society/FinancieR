rm(list = ls())
context("Creating test objects for CMP: Shares Mode")
yahoo_adj_prices <- testthis::read_testdata("yahoo_adj_prices.rds")

end_date   <- as.Date(zoo::index(xts::last(yahoo_adj_prices))) # latest date
start_date <- end_date - 365*3                                 # 3-yr window

historical_rtn <- yahoo_adj_prices[paste0(start_date - 1, "/", end_date)] %>% {
  log(.[-1,] / lag(., k =  1, na.pad = FALSE)[-nrow(.),])
}

exp_rtn        <- colMeans(historical_rtn)
exp_vol        <- dplyr::summarize(
  tibble::as_tibble(historical_rtn),
  dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
) %>%
  purrr::as_vector()
exp_cor        <- stats::cor(historical_rtn, use = "pairwise.complete.obs")
historical_rtn <- yahoo_adj_prices[
  paste0(start_date - 1, "/", end_date)
] %>% {
  log(.[-1,] / lag(., k =  1, na.pad = FALSE)[-nrow(.),])
}
portfolio_aum <- round(runif(1, 100000, 5000000) + runif(1), digits = 2)
prices        <- yahoo_adj_prices[sample(zoo::index(yahoo_adj_prices), 1)] %>% {
  stats::setNames(as.numeric(.), colnames(.)) 
}

####### Calculate market portfolios --------------------------------------------
# mp_by_shares              <- calculate_market_portfolio(
#   exp_rtn, exp_vol, exp_cor,
#   prices        = prices,
#   portfolio_aum = portfolio_aum
# )
mp_by_wt_shorts           <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, rfr = 0.0000822
)

