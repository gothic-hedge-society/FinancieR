context("Accuracy")

test_that(
  "xts_merge_align_next() gives correct result for TXN during Hurricane Sandy",
  {
    xts_merge_align_next_result <- xts_merge_align_next(
      xts1 = xts::rbind.xts(
        sample_historical_data$TXN$OHLCV[max(
          which(
            zoo::index(sample_historical_data$TXN$OHLCV) < zoo::index(
              xts::first(sample_historical_data$TXN$OHLCV["2012-10-31"])
            )
          )
        ),],
        sample_historical_data$TXN$OHLCV["2012-10-31"]
      ), 
      xts2 = sample_historical_data$TXN$dividends, 
      agg_function = sum, 
      na.rm = TRUE
    )
  
    cat(crayon::bold("\nxts_merge_align_next_result:\n"))
    print(xts_merge_align_next_result)
    
    xts_merge_align_next_known_answer <- xts::as.xts(
      matrix(
        c(
          28.92, 28.90, 28.055,   28.060,  28.09, 28.90, 
          28.92, 28.09, 13015509, 9626516, NA,    0.21
        ),
        ncol = 6,
        dimnames = list(
          c("2012-10-26", "2012-10-31"), 
          c("High", "Low", "Open", "Close", "Volume", "DividendAmount")
        )
      )
    )
    
    cat(crayon::bold("\nxts_merge_align_next_known_answer:\n"))
    print(xts_merge_align_next_known_answer)
    
    expect_identical(
      xts_merge_align_next_result,
      xts_merge_align_next_known_answer
    )
    
  }

)
