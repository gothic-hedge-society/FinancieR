
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
mp_by_wt_not_c            <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, 
  rfr     = 0.0000822, 
  compact = FALSE
)
mp_by_wt_shorts_not_c     <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, 
  rfr          = 0.0000822, 
  allow_shorts = TRUE, 
  compact      = FALSE
)
mp_by_wt                  <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, 
  rfr     = 0.0000822
)
mp_by_wt_shorts           <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, 
  rfr          = 0.0000822, 
  allow_shorts = TRUE
)

###### Match Excel (historical Yahoo! calcs) -----------------------------------
context("MP calcs match Excel (weights mode, NOT compact)")
test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer in 'Weights ",
    "mode' when applied to yahoo sample data: Longs Only, not compact"
  ),
  {
    expect_true(
      all(
        abs(
          mp_by_wt_not_c$weights -
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
    expect_true(abs(mp_by_wt_not_c$sharpe - 0.0741772) <= 0.0001)
  }
)
test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer in 'Weights ",
    "mode' when applied to yahoo sample data: Longs & Shorts, not compact"
  ),
  {
    expect_true(
      all(
        abs(
          mp_by_wt_shorts_not_c$weights -
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
    expect_true(abs(mp_by_wt_shorts_not_c$sharpe - 0.118638) <= 0.0001)
  }
)
context("MP calcs match Excel (weights mode, compact)")
test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer in 'Weights ",
    "mode' when applied to yahoo sample data: Longs Only"
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
test_that(
  paste0(
    "calculate_market_portfolio returns the known correct answer in 'Weights ",
    "mode' when applied to yahoo sample data: Longs & Shorts"
  ),
  {
    expect_true(
      all(
        abs(
          mp_by_wt_shorts$weights -
            c(
              "AAPL" = 0.22149982386928,   "WMT"  = 0.085905853006483, 
              "RMD"  = 0.10223481640288,   "AMZN" = 0.1089034518828,
              "CMG"  = 0.107268170374074,  "XOM"  = -0.178211569663418, 
              "LNC"  = -0.053400812220571, "HOG"  = -0.09064410541401,
              "FB"   = -0.051931431523259
            )
        ) <= 0.01
      )
    )
    expect_true(abs(mp_by_wt_shorts$sharpe - 0.118638) <= 0.0001)
  }
)

###### Not longing & shorting same stocks --------------------------------------
context("Not longing & shorting same stock")
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
