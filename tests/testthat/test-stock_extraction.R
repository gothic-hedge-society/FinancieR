
context("Building extraction test objects")
IVV_date_only       <- stock_data$IVV["2019-06-11"]
IVV_june_2019       <- stock_data$IVV["2019-06"]
TXN_hurricane_sandy <- stock_data$TXN["2012-10-25/2012-11-05", "Close"]
# PX_merger           <- stock_data$PX["2018-10-28/2018-11-04", "Close"]
# LIN_merger          <- stock_data$LIN[
#   "2018-10-28/2018-11-04", c("Close", "DividendAmount")
# ]
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
# test_that(
#   "Stock extract handles the PX-LIN merger",
#   {
#     expect_identical(PX_merger, testthis::read_testdata("PX_merger.rds"))
#     expect_identical(LIN_merger, testthis::read_testdata("LIN_merger.rds"))
#   }
# )
test_that(
  "Stock extract handles Apple's split in 2014",
  expect_identical(AAPL_split, testthis::read_testdata("AAPL_split.rds"))
)
# test_that(
#   "Stock exctract fails gracefully",
#   {
#     expect_message(
#       stock_data$LIN[
#         paste0(as.Date("2018-01-09") - 365, "/2018-01-09"), c("Close", "Close")
#       ]
#     )
#     expect_silent(
#       stock_data$LIN[
#         paste0(as.Date("2018-01-09") - 365, "/2018-01-09"),
#         c("Close", "Close"),
#         silent = TRUE
#       ]
#     )
#     expect_null(
#       stock_data$LIN[
#         paste0(as.Date("2018-01-09") - 365, "/2018-01-09"), c("Close", "Close")
#       ]
#     )
#     expect_equal(
#       nrow(stock_data$PX["2018-01-01/2018-10-31", c("Close", "Close")]), 211
#     )
#   }
# )
# test_that(
#   "Stock extract handles PX and LIN only, and PX-LIN matches rbind of the two",
#   {
#     PX_extract   <- stock_data$PX["2018-10-26/2018-10-30", c("Close", "Close")]
#     LIN_extract  <- stock_data$LIN["2018-10-31/2018-11-05", c("Close", "Close")]
#     both_extract <- stock_data$PX["2018-10-26/2018-11-05", c("Close", "Close")]
#     expect_equal(
#       xts::rbind.xts(PX_extract, LIN_extract) %>% {
#         storage.mode(.) <- "character"
#         xts::cbind.xts(
#           .,
#           xts::xts(
#             cbind(
#               "symbol"     = c("PX", "PX", "PX", "LIN", "LIN", "LIN", "LIN"),
#               "multiplier" = c("1",   "1", "1",  "1",   "1",   "1",   "1")
#             ),
#             order.by = zoo::index(.)
#           )
#         )
#       },
#       both_extract
#     )
#   }
# )

test_that(
  "Extract works if divs occur in the future",
  {
    mp_date      <- Sys.Date()
    for(stock_ticker in names(stock_data)){
      if(
        any(as.Date(zoo::index(stock_data[[stock_ticker]]$dividends)) > mp_date)
      ){break()}
    }
    expect_s3_class(
      stock_data[[stock_ticker]][
        paste0(as.Date(mp_date) - 3*365, "/", mp_date), c("Close", "Close")
      ],
      "xts"
    )
  }
)
