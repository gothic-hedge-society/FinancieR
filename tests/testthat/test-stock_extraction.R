context("Extraction works correctly on class \"stock\"")
test_that(
  "Stock extract works with date only",
  {
    expect_identical(
      stock_data$IVV["2019-06-11"],
      testthis::read_testdata("IVV_date_only.rds")
    )
    expect_identical(
      stock_data$IVV["2019-06"],
      testthis::read_testdata("IVV_june_2019.rds")
    )
  }
)
test_that(
  "Stock extract works with TXN and Hurricane Sandy",
  expect_identical(
    stock_data$TXN["2012-10-25/2012-11-05", "Close"],
    testthis::read_testdata("txn_hurricane_sandy.rds")
  )
)
test_that(
  "Stock extract handles the PX-LIN merger",
  {
    expect_identical(
      stock_data$PX["2018-10-29/2018-11-05", "Close"],
      testthis::read_testdata("PX_merger.rds")
    )
    expect_identical(
      stock_data$LIN["2018-10-29/2018-11-05", c("Close", "DividendAmount")],
      testthis::read_testdata("LIN_merger.rds")
    )
  }
)
test_that(
  "Stock extract handles Apple's split in 2014",
  expect_identical(
    stock_data$AAPL["2014-06-05/2014-06-11", c("Open", "Close")],
    testthis::read_testdata("AAPL_split.rds")
  )
)
