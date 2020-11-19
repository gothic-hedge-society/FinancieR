
context("Creating test objects for CMP: Weights Mode")
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

####### Calculate market portfolios --------------------------------------------
mp_by_wt        <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, rfr = 0.0000822
)

###### Match Excel (historical Yahoo! calcs) -----------------------------------
context("MP calcs match Excel (weights mode, longs)")
test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer when applied",
    " to yahoo sample data: Longs Only"
  ),
  {
    expect_true(
      all(
        abs(
          mp_by_wt$weights -
            c(
              "AAPL" = 0.148547, "WMT"  = 0.183588, "RMD"  = 0.135547,
              "AMZN" = 0.375110, "CMG" = 0.157209
            )
        ) <= 0.01
      )
    )
    expect_true(abs(mp_by_wt$sharpe - 0.0741772) <= 0.0001)
  }
)
