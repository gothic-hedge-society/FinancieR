
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

context("MnA Events")
test_that(
  "PX acquired by LIN on 2018-10-31",
  {
    expect_identical(
      stock_data$PX$MnA,
      tibble::tibble(
        "company"     = "PX",
        "acquired_by" = "LIN",
        "multiple"    = 1,
        "eff_date"    = "2018-10-31",
        "type"        = "merger"
      ) %>% {
        xts::xts(
          dplyr::select(., -"eff_date"),
          order.by = as.Date(.$eff_date)
        )
      }
    )
    expect_identical(
      stock_data$LIN$MnA,
      tibble::tibble(
        "company"  = "LIN",
        "acquired" = "PX",
        "multiple" = 1,
        "eff_date" = "2018-10-31",
        "type"     = "merger"
      ) %>% {
        xts::xts(
          dplyr::select(., -"eff_date"),
          order.by = as.Date(.$eff_date)
        )
      }
    )
  }
)
