context("Accuracy")

test_that(
  "xts_merge_align_next() gives correct result for TXN during Hurricane Sandy",
  {
    
    xts1 <- xts::rbind.xts(
      sample_historical_data$TXN$OHLCV[max(
        which(
          zoo::index(sample_historical_data$TXN$OHLCV) < zoo::index(
            xts::first(sample_historical_data$TXN$OHLCV["2012-10-31"])
          )
        )
      ),],
      sample_historical_data$TXN$OHLCV["2012-10-31"]
    )
    
    xts2 <- sample_historical_data$TXN$dividends
    
    xts_merge_align_next_result <- xts_merge_align_next(
      xts1 = xts1, 
      xts2 = xts2, 
      agg_function = sum, 
      na.rm = TRUE
    )
    
    xts_merge_align_next_known_answer <- zoo::as.zoo(
      matrix(
        c(
          28.92, 28.90, 28.055,   28.060,  28.09, 28.90, 
          28.92, 28.09, 13015509, 9626516, NA,    0.21
        ),
        ncol = 6,
        dimnames = list(
          NULL, 
          c("High", "Low", "Open", "Close", "Volume", "DividendAmount")
        )
      ),
      order.by = as.Date(c("2012-10-26", "2012-10-31"))
    )
    
    expect_identical(
      xts_merge_align_next_result,
      xts_merge_align_next_known_answer
    )
    
  }
  
)
