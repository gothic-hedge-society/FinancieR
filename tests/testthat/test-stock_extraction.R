
context("Building extraction test objects")
IVV_date_only       <- stock_data$IVV["2019-06-11"]
IVV_june_2019       <- stock_data$IVV["2019-06"]
TXN_hurricane_sandy <- stock_data$TXN["2012-10-25/2012-11-05", "Close"]
PX_merger           <- stock_data$PX["2018-10-29/2018-11-05", "Close"]
LIN_merger          <- stock_data$LIN[
  "2018-10-29/2018-11-05", c("Close", "DividendAmount")
]
AAPL_split <- stock_data$AAPL["2014-06-05/2014-06-11", c("Open", "Close")]

context("Extraction works correctly on class \"stock\"")
test_that(
  "Stock extract works with date only",
  {
    expect_identical(
      IVV_date_only, testthis::read_testdata("IVV_date_only.rds")
    )
    expect_identical(
      IVV_june_2019, testthis::read_testdata("IVV_june_2019.rds")
    )
  }
)
test_that(
  "Stock extract works with TXN and Hurricane Sandy",
  expect_identical(
    TXN_hurricane_sandy, testthis::read_testdata("TXN_hurricane_sandy.rds")
  )
)
test_that(
  "Stock extract handles the PX-LIN merger",
  {
    expect_identical(PX_merger, testthis::read_testdata("PX_merger.rds"))
    expect_identical(LIN_merger, testthis::read_testdata("LIN_merger.rds"))
  }
)
test_that(
  "Stock extract handles Apple's split in 2014",
  expect_identical(AAPL_split, testthis::read_testdata("AAPL_split.rds"))
)
