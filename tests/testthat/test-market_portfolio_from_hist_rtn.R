# context("Accuracy")
# 
# test_that(
#   paste0(
#     "market_portfolio_from_hist_rtn returns the known correct answer",
#     " when applied to yahoo sample data"
#   ),
#   {
#     # Testing that the portfolio weightings match
#     expect_equal(
#       round(
#         market_portfolio_from_hist_rtn(
#           readRDS("testdata/yahoo_historical_returns.rds")
#           )$weights,
#         digits = 2
#       ),
#       round(
#         c(
#           "AAPL"  = 0,                  "XOM"  = 0,
#           "LNC"   = 0.164042275212927,  "MRK"  = 0.0213442142997278,
#           "WMT"   = 0.193624316647811,  "HOG"  = 0,
#           "RMD"   = 0.218488712444243,  "AMZN" = 0.252739552619184,
#           "CMG"   = 0,                  "FB"   = 0.140082179771349,
#           "SP500" = 0.00967874900475859
#         ),
#         digits = 2
#       )
#     )
#     expect_equal(
#       round(
#         market_portfolio_from_hist_rtn(
#           readRDS("testdata/yahoo_historical_returns.rds")
#         )$sharpe,
#         digits = 6
#       ),
#       round(
#         0.427164179365774,
#         digits = 6
#       )
#     )
#   }
# )
