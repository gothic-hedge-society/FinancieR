#' Calculate Daily Returns
#' 
#' Calculate day-over-day historical returns for a list of assets, taking into 
#'  account splits and dividends.
#'   
#' @param assets a list of sub-lists named and corresponding to a given 
#'  asset, for example, "AAPL". Each sublist contains split, dividend, and 
#'  aggregated OHLCV data as xts objects. An example of an object that may be
#'  passed as \code{asset_data} is [sample_historical_data].
#' @param date_range_xts A string that specifies the date range for which 
#'  \code{calculate_daily_returns} will calculate returns. Gets passed to an
#'  xts, so simply use xts subsetting notation. If missing or NULL, then the 
#'  full date range of OHLCV is used.
#' @param buy_at Character: Specifies the 'initial price' to be used for 
#'  calculating returns. Must be One of "open", "high", "low", "close", or 
#'  "price". Case sensitive.
#' @param sell_at Character: Specifies the 'final price' to be used for 
#'  calculating returns. Must be One of "open", "high", "low", "close", or 
#'  "price". Case sensitive.
#' @param include_volume Boolean: include volume data in output? Default TRUE.
#' @param include_buy_sell Boolean: include in the output the buy & sell prices 
#'  that were used to calculate each return? Default FALSE.
#' @param returns_method Character vector specifying how the returns should be 
#'  calculated. Choices are: "ln", "log2", "log10", "pct_diff", and "multiple".
#'  Selecting multiple choices is allowed. Defaults to all.
#'  
#' @return depends on value passed to \code{include_volume}:
#'  \itemize{
#'   \item \strong{DEFAULT}, \code{include_volume == FALSE}: returns an xts 
#'    object. Each element is the daily log return -- taking into account splits 
#'    and dividends -- of the asset specified in the column name, realized on 
#'    the date specified in the xts index relative to the previous trading day.
#'   \item \code{include_volume == TRUE}: returns a list of two xts objects 
#'    named \code{"daily_returns"} and \code{"daily_volume"}. 
#'    \code{"daily_returns"} is exactly the same xts object that is returned if
#'    \code{include_volume == TRUE}, as described above. \code{"daily_volume"}
#'    has the same structure, date indices, and colnames \code{"daily_returns"},
#'    but contains the trade volume observed for a given asset on a given day.
#'  }
#' @export
calculate_daily_returns <- function(
  assets, 
  date_range_xts = NULL,
  buy_at  = c("close", "open", "high", "low", "price"), 
  sell_at = c("close", "open", "high", "low", "price"),
  include_volume   = TRUE,
  include_buy_sell = FALSE,
  returns_method   = c("ln", "log2", "log10", "pct_diff", "multiple")
){
  
  requireNamespace("xts")
  
  buy_at         <- match.arg(buy_at)
  sell_at        <- match.arg(sell_at)
  returns_method <- match.arg(returns_method, several.ok = TRUE)
  
  lapply(
    assets,
    function(asset){
      asset$OHLCV  %>% {
        if(is.null(date_range_xts)){
          .
        } else {
          
          previous_data_indices <- which(
            zoo::index(.) < zoo::index(xts::first(.[date_range_xts]))
          )
          
          if(length(previous_data_indices > 0)){
            xts::rbind.xts(
              .[max(previous_data_indices),],
              .[date_range_xts]
            )            
          } else {
            .
          }
          
        }
      } %>% {
        # If there are dividends for this asset, merge the dividend amounts with 
        #   the OHLCV data. Only keep dividends whose ex-dates are within the 
        #   range of the historical price data.
        if(
          isTRUE(nrow(asset$dividends) > 0) &&
          any(
            zoo::index(asset$dividends) >= zoo::index(xts::first(.)) &
            zoo::index(asset$dividends) <= zoo::index(xts::last(.))
          )
        ){
          xts_merge_align_next(
            xts1 = ., 
            xts2 = asset$dividends, 
            agg_function = sum, 
            na.rm = TRUE
          )
        } else {
          .$DividendAmount <- 0
          .
        }
      } %>% {
        # Similar process for splits.
        if(
          isTRUE(nrow(asset$splits) > 0) &&
          any(
            zoo::index(asset$splits) >= zoo::index(xts::first(.)) &
            zoo::index(asset$splits) <= zoo::index(xts::last(.))
          )
        ){
          xts_merge_align_next(
            xts1 = ., 
            xts2 = asset$splits, 
            agg_function = base::prod, 
            na.rm = TRUE
          )
        } else {
          .$Denominator <- .$Numerator <- 1
          .            
        }
      } %>%
        as.data.frame() %>%
        tibble::as_tibble(rownames = "Date") %>%
        tidyr::replace_na(
          list(
            DividendAmount = 0,
            Denominator    = 1,
            Numerator      = 1
          )
        ) %>%
        # Get an XTS of the effective daily buy price & sell price
        (function(x){
          
          xts::merge.xts(
            data.frame(
              dplyr::select(x, dplyr::matches(buy_at, ignore.case = TRUE)) %>%
                magrittr::set_colnames("buy_price"),
              row.names = x$Date
            ) %>%
              xts::as.xts() %>%
              xts::lag.xts(k = 1),
            data.frame(
              (
                (dplyr::select(x, dplyr::matches(sell_at, ignore.case = TRUE)) *
                   x$Denominator / x$Numerator) + x$DividendAmount) %>%
                magrittr::set_colnames("sell_price"),
              row.names = x$Date
            ) %>%
              xts::as.xts()
          ) %>%
            stats::na.omit()
          
        }) %>%
        as.data.frame() %>%
        tibble::as_tibble(rownames = "Date") %>% {
          if("ln" %in% returns_method){
            return(
              tibble::add_column(
                .,
                "ln_rtn" = log(.["sell_price"] / .["buy_price"]) * 100
              )
            )
          }
          .
        } %>% {
          if("log2" %in% returns_method){
            return(
              tibble::add_column(
                .,
                "log2_rtn" = log2(.["sell_price"] / .["buy_price"]) * 100
              )
            )
          }
          .
        } %>% {
          if("log10" %in% returns_method){
            return(
              tibble::add_column(
                .,
                "log10_rtn" = log10(.["sell_price"] / .["buy_price"]) * 100
              )
            )
          }
          .
        } %>% {
          if("pct_diff" %in% returns_method){
            return(
              tibble::add_column(
                .,
                "pct_diff" = ((.["sell_price"] / .["buy_price"]) - 1) * 100
              )
            )
          }
          .
        } %>% {
          if("multiple" %in% returns_method){
            return(
              tibble::add_column(
                .,
                "multiple" = .["sell_price"] / .["buy_price"]
              )
            )
          }
          .
        } %>% {
          if(include_buy_sell){
            .
          } else {
            dplyr::select(., -dplyr::ends_with("_price"))
          } 
        } %>%
        tibble::column_to_rownames("Date") %>%
        xts::as.xts() 
    } 
  )
  
}
