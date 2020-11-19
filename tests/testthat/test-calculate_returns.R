# Do a few checks on returns that were calculated by spreadsheet.

context("TXN & Hurricane Sandy")
test_that(
  paste0(
    "calculate_historical_returns() correctly handles Texas Instruments' Hurricane ",
    "Sandy missing dividend for all returns_method options."
  ),
  expect_equivalent(
    c("ln", "log2", "log10", "pct_diff", "multiple") %>%
      vapply(
        function(rtn_method){
          zoo::coredata(
            calculate_historical_returns(
              assets         = stock_data["TXN"],
              date_range_xts = "2012-10",
              returns_method = rtn_method
            )["2012-10-31"]
          )
        },
        numeric(1)
      ) %>%
      round(digits = 5),
    round(
      c(
        "ln"        = -0.021671592641372,
        "log2"      = -0.0312654992318732,
        "log10"     = -0.00941185309820299,
        "pct_diff"  = -0.0214384508990318,
        "multiple"  =  0.978561549100968
      ),
      digits = 5
    )
  )
)

context("Apple's 7-1 Split in 2014")
test_that(
  "calculate_historical_returns() correctly handles Apple's 7-to-1 split in 2014.",
  expect_equivalent(
    c("ln", "log2", "log10", "pct_diff", "multiple") %>%
      vapply(
        function(rtn_method){
          calculate_historical_returns(
            assets         = stock_data["AAPL"],
            date_range_xts = "2014-06",
            returns_method = rtn_method 
          )["2014-06-09"] 
        },
        numeric(1)
      ) %>%
      round(digits = 5),
    c(
      "ln"        = 0.0158746908251272,
      "log2"      = 0.0229023377290565,
      "log10"     = 0.0068942906272729,
      "pct_diff"  = 0.0160013631364528,
      "multiple"  = 1.01600136313645
    ) %>%
      round(digits = 5)
  )
)

context("calculate_historical_returns() examples work")
test_that(
  "The first example in calculate_historical_returns() documentation works.",
  expect_identical(
    log(
      as.numeric(stock_data$T$prices$Close["2014-07-21"]) /
        as.numeric(stock_data$T$prices$Close["2014-07-18"])
    ),
    as.numeric(
      calculate_historical_returns(
        assets         = stock_data[c("AAPL", "T")],
        date_range_xts = "2014"
      )["2014-07-21", "T"]
    )
  )  
)

test_that(
  "The second example in calculate_historical_returns() documentation works.",
  expect_identical(
    log(
      (
        as.numeric(stock_data$T$prices$Close["2014-07-08"]) + 
          as.numeric(stock_data$T$dividends$DividendAmount["2014-07-08"])
      ) / as.numeric(stock_data$T$prices$Close["2014-07-07"])
    ),
    as.numeric(
      calculate_historical_returns(
        assets         = stock_data[c("AAPL", "T")],
        date_range_xts = "2014"
      )["2014-07-08", "T"]
    )
  )  
)

test_that(
  "The third example in calculate_historical_returns() documentation works.",
  expect_identical(
    log(
      (
        as.numeric(stock_data$AAPL$prices$Close["2014-06-09"]) *
          as.numeric(stock_data$AAPL$splits$Denominator["2014-06-09"])
      ) / as.numeric(stock_data$AAPL$prices$Close["2014-06-06"])
    ),
    as.numeric(
      calculate_historical_returns(
        assets         = stock_data[c("AAPL", "T")],
        date_range_xts = "2014"
      )["2014-06-09", "AAPL"]
    )
  )  
)

gd_cb_ivv_returns <- calculate_historical_returns(
  assets         = stock_data[c("GD", "CB", "IVV")],
  date_range_xts = "2020-08-05/2020-08-14"
)

context("Missing Data")
test_that(
  "calculate_historical_returns() fails gracefully if missing data.",
  {
    expect_message(
      calculate_historical_returns(
        assets         = stock_data,
        date_range_xts = paste0(
          as.Date("2018-01-09") - 12,
          "/",
          "2018-01-09"
        )
      )
    )
    expect_silent(
      calculate_historical_returns(
        assets         = stock_data,
        date_range_xts = paste0(
          as.Date("2018-01-09") - 12,
          "/",
          "2018-01-09"
        ),
        silent         = TRUE
      )
    )
    expect_length(
      colnames(
        calculate_historical_returns(
          assets         = stock_data,
          date_range_xts = paste0(
            as.Date("2018-01-09") - 12,
            "/",
            "2018-01-09"
          ),
          silent         = TRUE
        )
      ),
      9
    )
    expect_null(
      calculate_historical_returns(
        assets         = stock_data$LIN,
        date_range_xts = paste0(
          as.Date("2018-01-09") - 365,
          "/",
          "2018-01-09"
        )
      )
    )
  }
)
