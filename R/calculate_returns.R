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
  
  assets[sort(names(assets))] %>%
    purrr::imap(
      function(asset, asset_name){
        
        prices_xts <- if(is.null(date_range_xts)){
          asset$prices
        } else {
          asset$prices[date_range_xts]
        } %>% {
          xts::merge.xts(
            xts::lag.xts(.[,buy_at, drop = FALSE], k = 1),
            .[,sell_at]
          )
        } %>% {
          .[-unique(which(is.na(.), arr.ind = TRUE)[,"row"]),]
        } %>%
          magrittr::set_colnames(c("buy_price", "sell_price"))
        
        divs_xts <- if(is.null(date_range_xts)){
          asset$dividends$DividendAmount
        } else {
          asset$dividends$DividendAmount[date_range_xts]
        } %>% {
          storage.mode(.) <- "numeric"
          .
        }
        
        splits_xts <- if(is.null(date_range_xts)){
          asset$splits
        } else {
          asset$splits[date_range_xts]
        }
        
        # If there are dividends for this asset, merge the dividend amounts with
        # the prices data. Only keep dividends whose ex-dates are within the
        # range of the historical price data.
        if(isTRUE(nrow(divs_xts) > 0)){
          prices_xts <- xts_merge_align_next(
            xts1         = prices_xts,
            xts2         = divs_xts,
            agg_function = base::sum,
            na.fill      = 0
          )
        } else {
          prices_xts$DividendAmount <- 0
        }
        
        # Similar process for splits.
        if(isTRUE(nrow(splits_xts) > 0)){
          prices_xts <- xts_merge_align_next(
            xts1         = prices_xts,
            xts2         = splits_xts,
            na.fill      = 1,
            agg_function = base::prod
          )
        } else {
          prices_xts$Denominator <- prices_xts$Numerator <- 1
        }
        
        prices_xts$sell_price <- (
          prices_xts$sell_price * prices_xts$Denominator / prices_xts$Numerator
        ) + prices_xts$DividendAmount
        
        prices_xts <- prices_xts[,c("buy_price", "sell_price")]
        
        
        switch(
          returns_method,
          "ln"       = log(prices_xts$sell_price / prices_xts$buy_price),
          "log2"     = log2(prices_xts$sell_price / prices_xts$buy_price),
          "log10"    = log10(prices_xts$sell_price / prices_xts$buy_price),
          "pct_diff" = ((prices_xts$sell_price / prices_xts$buy_price) - 1),
          "multiple" = prices_xts$sell_price / prices_xts$buy_price
        ) %>%
          xts::as.xts() %>% {
            colnames(.) <- asset_name
            .
          }
        
      }
    ) %>%
    purrr::reduce(xts::merge.xts)
  
}
