context("Accuracy")

test_that(
  "GMRR returns the known correct answer when applied to yahoo sample data",
  expect_equal(
    readRDS("testdata/yahoo_historical_returns.rds") %>%
      gmrr() %>%
      round(digits = 4),
    c(
      "AAPL" =  0.0124, "XOM" = 0.0035, "LNC"   = 0.0211, "MRK"  = 0.0089,
      "WMT"  =  0.0082, "HOG" = 0.0026, "RMD"   = 0.0167, "AMZN" = 0.0258,
      "CMG"  = -0.0053, "FB"  = 0.0270, "SP500" = 0.0107
    )
  )
)
