
context("Known data errors corrected")
test_that(
  "IVV has correct closing price for 22 Jun 2015",
  {
    expect_true(as.logical(stock_data$IVV$prices$Close["2015-06-22"] == 214.12))
    expect_true(
      as.logical(stock_data$IVV$prices$PreviousClose["2015-06-23"] == 214.12)
    )
  }
)

context("Trading Dates")

test_that(
  "No intersection between dates which have data and trading_dates() output",
  expect_length(
    intersect(
      trading_dates(),
      stock_data %>%
        lapply(function(asset){as.character(zoo::index(asset$prices))}) %>%
        unlist() %>%
        unique()
    ),
    0
  )
)
