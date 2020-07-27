context("Accuracy")

test_that(
  "xts_merge_align_next() gives correct result for TXN during Hurricane Sandy",
  {
    
    cols <- c("Open", "High", "Low", "Close", "Volume", "DividendAmount")
    
    calculated_answer <- xts_merge_align_next(
      xts1         = stock_data$TXN$prices["2012-10"],
      xts2         = stock_data$TXN$dividends$DividendAmount["2012-10"],
      agg_function = base::sum,
      na.fill      = 0
    )[c("2012-10-26", "2012-10-31"), cols]
    
    known_correct_answer <- xts::xts(
      matrix(
        c(
          28.09, 28.90, 28.92, 28.90, 28.055, 28.060, 28.92, 28.09, 13015509, 
          9626516, 0.0, 0.21
        ),
        ncol = 6,
        dimnames = list(NULL, cols)
      ),
      order.by = as.Date(c("2012-10-26", "2012-10-31"))
    )
    
    expect_equivalent(calculated_answer, known_correct_answer)
    
  }
  
)
