# make a stock block (an xts) from a "stock" class object.
stock_blockify <- function(x, i, j){

  requireNamespace("xts", quietly = TRUE)

  stock <- structure(x, class = "list") %>% {
    .[setdiff(names(.), "MnA")]
  } %>%
    lapply(
      function(stock_component){
        {
          stock_component
          if(is.null(j)){
            stock_component[i]
          } else {
            selected_cols <- c(
              intersect(
                c(j, "DividendAmount", "Numerator", "Denominator"),
                c(colnames(stock_component))
              )
            )
            if(isTRUE(length(selected_cols) > 0)){
              stock_component[i, selected_cols]
            } else {
              NULL
            }
          }
        }
      }
    ) %>% {
      .[
        which(
          vapply(
            names(.),
            function(component_name){
              isTRUE(nrow(.[[component_name]]) > 0)
            },
            FUN.VALUE = logical(1)
          )
        )
      ]
    }

  stock_block <- stock$prices

  # Handle divs
  if(isTRUE(nrow(stock$dividends) > 0)){
    numeric_div_cols     <- find_numeric_columns(stock$dividends)
    non_numeric_div_cols <- setdiff(
      colnames(stock$dividends),
      numeric_div_cols
    )
    if(isTRUE(length(numeric_div_cols) > 0)){
      stock_block <- xts_merge_align_next(
        xts1         = stock_block,
        xts2         = stock$dividends[,numeric_div_cols],
        agg_function = base::sum,
        na.fill      = 0
      )
    }
    if(isTRUE(length(non_numeric_div_cols) > 0)){
      storage.mode(stock_block) <- "character"
      stock_block <- xts_merge_align_next(
        xts1         = stock_block,
        xts2         = stock$dividends[,non_numeric_div_cols],
        agg_function = function(x){x},
        na.fill      = ""
      )
    }
  }
  # Handle Splits
  if(isTRUE(nrow(stock$splits) > 0)){
    stock_block <- xts_merge_align_next(
      xts1         = stock_block,
      xts2         = stock$splits,
      na.fill      = 1,
      agg_function = base::prod
    )
  }

  stock_block

}

# Get which columns (char vec) are numeric.
find_numeric_columns <- function(frame_obj){
  colnames(frame_obj) %>%
    stats::setNames(.,.) %>%
    vapply(
      function(col_name){
        suppressWarnings(!all(is.na(as.numeric(frame_obj[,col_name]))))
      },
      FUN.VALUE = logical(1)
    ) %>% {
      names(.)[which(.)]
    }
}
