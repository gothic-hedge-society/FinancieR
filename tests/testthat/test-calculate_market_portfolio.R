
# context("Creating test objects for CMP: Weights Mode")
yahoo_adj_prices <- testthis::read_testdata("yahoo_adj_prices.rds")

end_date   <- as.Date(zoo::index(xts::last(yahoo_adj_prices))) # latest date
start_date <- end_date - 365*3                                 # 3-yr window

historical_rtn <- yahoo_adj_prices[paste0(start_date - 1, "/", end_date)] %>% {
  log(.[-1,] / lag(., k =  1, na.pad = FALSE)[-nrow(.),])
}

####### Calculate market portfolio ---------------------------------------------
mp_yahoo_from_bewp <- FinancieR::calculate_market_portfolio(
  exp_rtn = colMeans(historical_rtn), 
  exp_vol = dplyr::summarize(
    tibble::as_tibble(historical_rtn),
    dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
  ) %>%
    purrr::as_vector(), 
  exp_cor = stats::cor(historical_rtn, use = "pairwise.complete.obs"), 
  rfr     = 0.0000822
)

###### Match Excel (historical Yahoo! calcs) -----------------------------------
context("MP calcs match Excel (Yahoo data) from BEWP")
test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer when applied",
    " to yahoo sample data"
  ),
  {
    expect_true(
      all(
        abs(
          mp_yahoo_from_bewp$weights -
            c(
              "AAPL" = 0.148547, "WMT"  = 0.183588, "RMD"  = 0.135547,
              "AMZN" = 0.375110, "CMG" = 0.157209
            )
        ) <= 0.01
      )
    )
    expect_true(abs(mp_yahoo_from_bewp$sharpe - 0.0741772) <= 0.0001)
  }
)

context("MP calcs match Excel (Yahoo data) from random startoff")
random_startoff <- runif(sample(1:ncol(historical_rtn), 1)) %>% 
  c(rep(0, ncol(historical_rtn) - length(.))) %>%
  sample() %>% {
    . / sum(.)
  } %>%
  stats::setNames(colnames(historical_rtn))

mp_yahoo_from_random_startoff <- FinancieR::calculate_market_portfolio(
  exp_rtn  = colMeans(historical_rtn),
  exp_vol  = dplyr::summarize(
    tibble::as_tibble(historical_rtn),
    dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
  ) %>%
    purrr::as_vector(),
  exp_cor  = stats::cor(historical_rtn, use = "pairwise.complete.obs"),
  rfr      = 0.0000822,
  startoff = random_startoff
)

test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer when applied",
    " to yahoo sample data: random startoff"
  ),
  {
    expect_true(
      all(
        abs(
          mp_yahoo_from_random_startoff$weights -
            c(
              "AAPL" = 0.148547, "WMT"  = 0.183588, "RMD"  = 0.135547,
              "AMZN" = 0.375110, "CMG" = 0.157209
            )
        ) <= 0.01
      )
    )
    expect_true(abs(mp_yahoo_from_random_startoff$sharpe - 0.0741772) <= 0.0001)
  }
)
