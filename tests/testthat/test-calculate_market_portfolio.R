
yahoo_adj_prices <- testthis::read_testdata("yahoo_adj_prices.rds")

end_date   <- as.Date(zoo::index(xts::last(yahoo_adj_prices))) # latest date
start_date <- end_date - 365*3                                 # 3-yr window

historical_rtn <- yahoo_adj_prices[paste0(start_date - 1, "/", end_date)] %>% {
  log(.[-1,] / lag(., k =  1, na.pad = FALSE)[-nrow(.),])
}

exp_rtn <- colMeans(historical_rtn)

exp_vol <- dplyr::summarize(
  tibble::as_tibble(historical_rtn),
  dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
) %>%
  purrr::as_vector()

exp_cor <- stats::cor(historical_rtn, use = "pairwise.complete.obs")
historical_rtn <- yahoo_adj_prices[
  paste0(start_date - 1, "/", end_date)
] %>% {
  log(.[-1,] / lag(., k =  1, na.pad = FALSE)[-nrow(.),])
}

mp_by_wt <- calculate_market_portfolio(exp_rtn, exp_vol, exp_cor, 0.0000822)

context("MP calcs match Excel: weights")
test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer in 'Weights ",
    "mode' when applied to yahoo sample data: Longs Only"
  ),
  {
    # Testing that the portfolio weightings match
    expect_true(
      all(
        abs(
          mp_by_wt$weights -
            c(
              "AAPL" = 0.148547,           "XOM"  = 0,
              "LNC"  = 0,                  "MRK"  = 0,
              "WMT"  = 0.183588,           "HOG"  = 0,
              "RMD"  = 0.135547,           "AMZN" = 0.375110,
              "CMG"  = 0.157209,           "FB"   = 0,
              "IVV"  = 0
            )
        ) <= 0.01
      )
    )
    
    expect_true(abs(mp_by_wt$sharpe - 0.0741772) <= 0.0001)
    
  }
)

context("MP calcs match Excel: weights + shorts")

mp_by_wt_shorts <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, 0.0000822, allow_shorts = TRUE
)

test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer in 'Weights ",
    "mode' when applied to yahoo sample data: Longs & Shorts"
  ),
  {
    
    # Testing that the portfolio weightings match
    expect_true(
      all(
        abs(
          mp_by_wt_shorts$weights -
            c(
              "AAPL"   = 0.22149982386928,  "XOM"    = 0,
              "LNC"    = 0,                 "MRK"    = 0,
              "WMT"    = 0.085905853006483, "HOG"    = 0,
              "RMD"    = 0.10223481640288,  "AMZN"   = 0.1089034518828,
              "CMG"    = 0.107268170374074, "FB"     = 0,
              "IVV"    = 0,                 "s_AAPL" = 0,           
              "s_XOM"  = 0.178211569663418, "s_LNC"  = 0.053400812220571,                  
              "s_MRK"  = 0,                 "s_WMT"  = 0,
              "s_HOG"  = 0.09064410541401,  "s_RMD"  = 0,
              "s_AMZN" = 0,                 "s_CMG"  = 0,
              "s_FB"   = 0.051931431523259, "s_IVV"  = 0
            )
        ) <= 0.01
      )
    )
    
    expect_true(abs(mp_by_wt_shorts$sharpe - 0.118638) <= 0.0001)
    
  }
)

context("Cash Balance: shares mode")

portfolio_aum <- runif(1, 100000, 5000000) + runif(1)
prices        <- yahoo_adj_prices[sample(zoo::index(yahoo_adj_prices), 1)] %>% {
  stats::setNames(as.numeric(.), colnames(.)) 
}

mp_by_shares <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, prices = prices, portfolio_aum = portfolio_aum 
)

test_that(
  "Cash balances for mp by shares, no shorting",
  expect_true(
    round(
      as.numeric(mp_by_shares$shares %*% as.matrix(prices)) + mp_by_shares$cash,
      digits = 2
    ) == round(portfolio_aum, digits = 2)
  )
)

mp_by_shares_shorts <- calculate_market_portfolio(
  exp_rtn, 
  exp_vol, 
  exp_cor, 
  allow_shorts  = TRUE,
  prices        = prices, 
  portfolio_aum = portfolio_aum 
)

prices_shorts <- c(
  prices, stats::setNames(prices, paste0("s_", names(prices)))
)

test_that(
  "Cash balances for mp by shares, with shorting",
  expect_true(
    round(
      as.numeric(
        mp_by_shares_shorts$shares %*% as.matrix(prices_shorts)
      ) + mp_by_shares_shorts$cash,
      digits = 2
    ) == round(portfolio_aum, digits = 2)
  )
)

context("Not longing & shorting the same stock")
test_that(
  "Weights basis",
  expect_length(
    intersect(
      mp_by_wt_shorts$weights[
        grep("^s_", names(mp_by_wt_shorts$weights), value = TRUE) 
      ] %>% {
        names(.[which(. > 0)])
      },
      mp_by_wt_shorts$weights[
        setdiff(
          names(mp_by_wt_shorts$weights),
          grep("^s_", names(mp_by_wt_shorts$weights), value = TRUE) 
        )
      ] %>% {
        names(.[which(. > 0)])
      }
    ),
    0
  )
)
test_that(
  "Shares basis",
  expect_length(
    intersect(
      mp_by_shares_shorts$shares[
        grep("^s_", names(mp_by_shares_shorts$shares), value = TRUE) 
      ] %>% {
        names(.[which(. > 0)])
      },
      mp_by_shares_shorts$shares[
        setdiff(
          names(mp_by_shares_shorts$shares),
          grep("^s_", names(mp_by_shares_shorts$shares), value = TRUE) 
        )
      ] %>% {
        names(.[which(. > 0)])
      }
    ),
    0
  )
)
  