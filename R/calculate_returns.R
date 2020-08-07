#' Calculate Returns
#'
#' Calculate period-over-period historical returns for a list of assets, taking
#' into account splits and dividends.
#'
#' @param assets a list of sub-lists named and corresponding to a given
#'  asset, for example, "AAPL". Each sublist contains split, dividend, and
#'  aggregated prices data as xts objects. An example of an object that may be
#'  passed as \emph{asset_data} is [stock_data].
#' @param date_range_xts A string that specifies the date range for which
#'   \emph{calculate_daily_returns}() will calculate returns. Use xts subsetting
#'   notation to specify the date range; see examples in \link[xts]{xts}. If
#'   missing or NULL, then the full date range of OHLCV prices is used.
#' @param buy_at Character: Specifies the 'initial price' to be used for
#'  calculating returns. Must be one of "open", "high", "low", "close", or
#'  "price", defaults to "Close". Not case sensitive.
#' @param sell_at Character: Specifies the 'final price' to be used for
#'  calculating returns. Must be One of "open", "high", "low", "close", or
#'  "price", defaults to "Close". Not case sensitive.
#' @param returns_method Character vector specifying how the returns should be
#'  calculated, defaults to \strong{\emph{"ln}}. Choices are: 
#'  \itemize{
#'    \item{\strong{"ln"}}: Log return; \emph{ln(sell_price / buy_price)} where
#'      \emph{ln()} denotes the natural logarithm (base \emph{e}).
#'    \item{\strong{"log2"}}: Log return; \emph{log(sell_price / buy_price)} 
#'      where \emph{log()} denotes the logarithm to base \emph{2}.
#'    \item{\strong{"log10"}}: Log return; \emph{log(sell_price / buy_price)} 
#'      where \emph{log()} denotes the logarithm to base \emph{10}.
#'    \item{\strong{"pct_diff"}}: Percent difference; 
#'      \emph{(sell_price - buy_price)/buy_price}. NOT multiplied by 100.
#'    \item{\strong{"multiple"}}: Price multiple; \emph{sell_price / buy_price}. 
#'  }
#'  
#' @return An xts object. Each element is the return observed on the date given
#'   by the xts' index with respect to the previous period (taking into
#'   account any splits and dividends that may have occurred) for the asset 
#'   specified by the element's column name. 
#'  
#' @example inst/examples/calculate_returns_ex.R
#'
#' @export
#'
calculate_returns <- function(
  assets,
  date_range_xts = NULL,
  buy_at         = "Close",
  sell_at        = "Close",
  returns_method = "ln"
){
  
  # Need xts namespace
  requireNamespace("xts", quietly = TRUE)
  
  if(inherits(assets, "stock")){
    assets <- stats::setNames(list(assets), nm = attr(assets, "Symbol"))
  }
  
  assets[sort(names(assets))] %>%
    purrr::map(
      function(asset){
        
        if(any(names(asset) == "MnA")){
          asset      <- asset[date_range_xts, c(buy_at, sell_at)]
          asset_name <- paste(
            unique(as.character(asset$symbol)), collapse = "-"
          )
          asset      <- asset[, find_numeric_columns(asset)] 
        } else {
          asset_name <- attr(asset, "Symbol")
          asset      <- asset[date_range_xts, c(buy_at, sell_at)]
        }
        storage.mode(asset) <- "numeric"
        
        buy_price  <- xts::lag.xts(asset[, buy_at], k = 1, na.pad = FALSE)
        sell_price <- asset[, sell_at][-1, ]
        
        if(all(c("Denominator", "Numerator") %in% colnames(asset))){
          sell_price <- sell_price * asset$Denominator / asset$Numerator
        }
        
        if(any(colnames(asset) == "DividendAmount")){
          sell_price <- sell_price + asset$DividendAmount
        }
        
        if(any(colnames(asset) == "multiplier")){
          sell_price <- sell_price * asset$multiplier
        }
        
        switch(
          returns_method,
          "ln"       = log(sell_price / buy_price),
          "log2"     = log2(sell_price / buy_price),
          "log10"    = log10(sell_price / buy_price),
          "pct_diff" = ((sell_price / buy_price) - 1),
          "multiple" = sell_price / buy_price
        ) %>% {
          colnames(.) <- asset_name
          .
        }
        
      }
    ) %>%
    purrr::reduce(xts::merge.xts)
  
}
