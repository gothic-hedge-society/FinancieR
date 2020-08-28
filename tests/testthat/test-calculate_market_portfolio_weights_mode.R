
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
# historical_rtn_shorts <- cbind(
#   historical_rtn,
#   (historical_rtn * - 1) %>%
#     magrittr::set_colnames(paste0("s_", colnames(historical_rtn)))
# )
# exp_rtn_shorts        <- colMeans(historical_rtn)
# exp_vol_shorts        <- dplyr::summarize(
#   tibble::as_tibble(historical_rtn),
#   dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
# ) %>%
#   purrr::as_vector()
# exp_cor_shorts        <- stats::cor(
#   historical_rtn, use = "pairwise.complete.obs"
# )

####### Calculate market portfolios --------------------------------------------
mp_by_wt        <- calculate_market_portfolio(
  exp_rtn, exp_vol, exp_cor, rfr = 0.0000822
)
# mp_by_wt_shorts <- calculate_market_portfolio(
#   exp_rtn_sh, exp_vol, exp_cor, rfr = 0.0000822
# )

###### Match Excel (historical Yahoo! calcs) -----------------------------------
context("MP calcs match Excel (weights mode, longs)")
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
              "AAPL" = 0.148547, "AMZN" = 0.375110, "CMG" = 0.157209,
              "RMD"  = 0.135547, "WMT"  = 0.183588
            )
        ) <= 0.01
      )
    )
    expect_true(abs(mp_by_wt$sharpe - 0.0741772) <= 0.0001)
  }
)
# context("MP calcs match Excel (weights mode, with shorting)")
# test_that(
#   paste0(
#     "calculate_market_portfolio returns the known correct answer in 'Weights ",
#     "mode' when applied to yahoo sample data: Longs & Shorts"
#   ),
#   {
#     expect_true(
#       all(
#         abs(
#           mp_by_wt_shorts$weights -
#             c(
#               "AAPL" = 0.22149982386928,   "WMT"  = 0.085905853006483, 
#               "RMD"  = 0.10223481640288,   "AMZN" = 0.1089034518828,
#               "CMG"  = 0.107268170374074,  "XOM"  = -0.178211569663418, 
#               "LNC"  = -0.053400812220571, "HOG"  = -0.09064410541401,
#               "FB"   = -0.051931431523259
#             )
#         ) <= 0.01
#       )
#     )
#     expect_true(abs(mp_by_wt_shorts$sharpe - 0.118638) <= 0.0001)
#   }
# )
