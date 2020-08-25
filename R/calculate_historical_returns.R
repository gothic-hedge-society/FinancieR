#' Calculate Historical Time-series Returns
#'
#' Calculate period-over-period historical returns for a list of assets.
#' \emph{calculate_historical_returns}() takes dividends, splits, and mergers &
#' acquisitions into account, as well as short fees if returns on short
#' positions are to be included in the output.
#' \emph{calculate_historical_returns} is intended for use in calculating
#' correlations, averages, and volatility of returns; therefore, it does not
#' take taxes or transaction fees into account.
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
#' @param short_fees Optional. Either a named numeric vector or a numeric length
#'   1 (i.e., a single number). Specify this parameter to cause
#'   \emph{calculate_historical_returns}() to include the returns observed for short
#'   selling each asset. \emph{calculate_historical_returns}() will assume that, for every
#'   return reported in the output, the expected return on a short sale equals
#'   the expected return on the long sale times (1 - short fees):
#'    \deqn{
#'      \langle E_r,short \rangle = \langle E_r,long \rangle \times 
#'        (short\_fees - 1)
#'    }{ASCII representation}
#'    
#'    If a \strong{single number} (numeric vector of length 1): The short fees
#'    for every asset in \emph{assets} are assumed to be equal to the number
#'    passed in as \emph{short_fees}. Use this option if you expect the short
#'    fees for each asset to be the same.
#'    
#'    If \strong{numeric vector length > 1}: The names of \emph{short_fees} must
#'    correspond to the names of \emph{assets}, and each element must be the
#'    short fee expected for each asset. Use this option if the short fees for
#'    each asset is expected to be significantly different.
#'    
#'    See the "\strong{The Cost of Shorting}" section for more information.
#'    
#' @inheritParams [.stock
#'  
#' @return An xts object. Each element is the return observed on the date given
#'   by the xts's index with respect to the previous period, taking into
#'   account any splits, dividends or M&A events that may have occurred in
#'   between, for the asset specified by the element's column name. 
#'  
#' @example inst/examples/calculate_historical_returns_ex.R
#'
#' @export
#'
calculate_historical_returns <- function(
  assets,
  date_range_xts = paste0("2012/", as.character(Sys.Date())),
  buy_at         = "Close",
  sell_at        = "Close",
  returns_method = "ln",
  short_fees     = NULL,
  silent         = FALSE
){
  
  # Need xts namespace
  requireNamespace("xts", quietly = TRUE)
  
  if(inherits(assets, "stock")){
    assets <- stats::setNames(list(assets), nm = attr(assets, "Symbol"))
  }
  
  if(!is.null(short_fees)){
    if(length(short_fees) > 1){
      short_fees <- short_fees[names(assets)]  
    }
    shorts <- TRUE
  }
  
  assets[sort(names(assets))] %>%
    purrr::map(
      function(asset){
        
        if(any(names(asset) == "MnA")){
          
          asset_name <- attr(asset, "Symbol")
          asset      <- asset[date_range_xts, c(buy_at, sell_at), silent = TRUE]
          
          if(!is.null(asset)){
            if(any(colnames(asset) == "symbol")){
              asset_name <- paste(
                unique(as.character(asset$symbol)), collapse = "-"
              )              
            }
            
            asset <- asset[, find_numeric_columns(asset), silent = TRUE]
            
          } else {
            
            if(!silent){
              usethis::ui_info(
                paste0(
                  "No data available for ", crayon::bold(asset_name),
                  " during time range ",    crayon::bold(date_range_xts),
                  "."
                )
              )
            }
            
            return(NULL)
            
          }
          
        } else {
          asset_name <- attr(asset, "Symbol")
          asset      <- asset[date_range_xts, c(buy_at, sell_at), silent = TRUE]
        }
        
        if(is.null(asset)){
          if(!silent){
            usethis::ui_info(
              paste0(
                "No data available for ", crayon::bold(asset_name),
                " during time range ",    crayon::bold(date_range_xts),
                "."
              )
            )
          } 
          return(NULL)
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
    purrr::compact() %>% {
      if(length(.) > 0){
        purrr::reduce(., xts::merge.xts) %>% {
          if(!is.null(short_fees)){
            if(length(short_fees) > 1){
              short_fees <- matrix(
                short_fees,
                nrow     = nrow(.),
                ncol     = length(short_fees),
                byrow    = TRUE,
                dimnames = list(list(), names(short_fees))
              )[, colnames(.)]  
            }
            short_rtn <- -. - short_fees
            colnames(short_rtn) <- paste0("s_", colnames(.))
            . <- xts::cbind.xts(., short_rtn)
          }
          .
        }
      }
    }
  
}
