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
    
    cat(crayon::bold("\nxts1:\n"))
    print(xts1)
    
    xts2 <- sample_historical_data$TXN$dividends
    
    cat(crayon::bold("\nhead(xts2):\n"))
    print(head(xts2))
    
    xts2 <- xts2[paste(
      as.Date(min(zoo::index(xts1))),
      as.Date(max(zoo::index(xts1))),
      sep = "/"
    )]
    
    cat(crayon::bold("\nxts2:\n"))
    print(xts2)
    
    for(
      disjunction_date in setdiff(
        as.character(as.Date(zoo::index(xts2))), 
        as.character(as.Date(zoo::index(xts1)))
      )
    ){
      
      cat(crayon::bold("\ndisjunction_date:\n"))
      print(disjunction_date)
      
      new_row <- vapply(
        # vapply works column-wise when passed an XTS
        xts2[paste(
          disjunction_date,
          as.Date(min(zoo::index(xts1[paste0(disjunction_date, "/")]))),
          sep = "/"
        )],
        FUN = sum,
        FUN.VALUE = numeric(1)
      )
      
      cat(crayon::bold("\nnew_row:\n"))
      print(new_row)
      
      mtx_dimnames <- list(
        as.character(
          as.Date(min(zoo::index(xts1[paste0(disjunction_date, "/")])))
        ), 
        colnames(xts2)
      )
      
      cat(crayon::bold("\nmtx_dimnames:\n"))
      print(mtx_dimnames)
      
      xts2 <- xts::rbind.xts(
        xts2,
        xts::as.xts(
          matrix(new_row, nrow = 1, ncol = ncol(xts2), dimnames = mtx_dimnames) 
        )
      )
      
      cat(crayon::bold("\nxts2:\n"))
      print(xts2)
      
    }
    
    final_merge <- xts::merge.xts(xts1, xts2, all = c(TRUE, FALSE))
    
    cat(crayon::bold("\nfinal_merge:\n"))
    print(final_merge)
    
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
