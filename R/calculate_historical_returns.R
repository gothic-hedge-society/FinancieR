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
#' @param short_fees Optional. Ether a single numeric value or a named numeric
#'   vector for which each element is a percentage short fee and each name is an
#'   asset that appears in \emph{assets} to which the short fee applies. Specify
#'   this parameter to cause \emph{calculate_historical_returns}() to include
#'   the returns observed for short selling each asset.
#'   \emph{calculate_historical_returns}() will assume that, for every return
#'   reported in the output, the expected return on a short sale equals the
#'   expected return on the long sale times (1 - short fees).
#'
#'   If a \strong{single number} (numeric vector of length 1): The short fees
#'   for every asset in \emph{assets} are assumed to be equal to the number
#'   passed in as \emph{short_fees}, and a new column for the returns of
#'   shorting each asset will be included in the output. Use this option if you
#'   expect the short fees for each asset to be the same.
#'
#'   If \strong{numeric vector length > 1}: The names of \emph{short_fees} must
#'   correspond to the names of \emph{assets}, and each element must be the
#'   short fee expected for each asset. Use this option if the short fees for
#'   each asset is expected to be significantly different. If an asset's name
#'   does not appear in the \emph{short_fees} vector, then a column containing 
#'   the returns from shorting that asset won't be included in the output.
#'
#'   See the "\strong{The Cost of Shorting}" section for more information.
#'    
#' @param silent Boolean, default FALSE, which allows messages. To suppress 
#'   messages, set to TRUE.
#'   
#' @param enforce_date_range Boolean, default TRUE. If TRUE, then only assets
#'   for which returns are available during the FULL data range will be included
#'   in the output. Set to FALSE to allow NA values in the output where no 
#'   historical data exists.
#'  
#'  #' @section The Cost of Shorting:
#'  You would only short an asset if the return you expect for buying that asset
#'  is negative. You may think that the expected return for shorting an asset is
#'  simply the return you expect for longing the asset times -1. In reality, the
#'  return you'll get from a short sale will be a fraction of the asset times -1
#'  because your broker charges fees on short position in exchange for providing
#'  the shorting service. In addition, the capital gains from short selling may
#'  taxed at a different rate than those realized on your long positions.
#'  
#'  The key additional costs of shorting are:
#'  
#'  \describe{
#'   \item{Dividends}{
#'      You don't earn dividends on a short position. In fact, it's the
#'      opposite: if you're short a stock on a dividend's ex-date, then you must
#'      actually \emph{pay} the dividend to the owner of the stock.
#'      \emph{\link{calculate_historical_returns}}() takes this into account
#'      itself when shorting is included.
#'    }
#'    \item{Short Fees}{
#'      Your brokerage will charge a fee on a short position for every day the
#'      position is open. This fee is based on the availability of assets for
#'      shorting, and varies from asset to asset and through time. Usually the
#'      fee is calculated on each trading day and debited from the trading
#'      account on at the beginning of each month.
#'      \emph{\link{calculate_historical_returns}}() takes this into account
#'      when the \emph{short_fees} parameter is specified.
#'    }
#'    \item{Taxes}{
#'      Capital gains taxes on your short sales can often be taxed at a higher
#'      rate than what you might expect for long sales. If \strong{uncovered},
#'      the short position will always be taxed as a short-term capital gain
#'      because the holding period is considered by the IRS to begin on the day
#'      when the short position was closed out (bought to cover). If
#'      \strong{covered}, then the holding period is considered to be the
#'      holding period of the \emph{substantially different securities} that can
#'      be converted to the stock itself. Taxes should be taken into account in
#'      the \emph{exp_rtn} parameter as passed to
#'      \emph{calculate_market_portfolio}. Taxes will depend on your situation:
#'      whether you expect a short-term or long-term investment, your income
#'      bracket, whether or not you're an institution, etc. You must figure that
#'      out for yourself, and you might find that, once everything is taken into
#'      account, it's just not worth it to short assets in a lot of situations.
#'      Stay smart!
#'    }
#'  }
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
  date_range_xts     = paste0("2012/", as.character(Sys.Date())),
  buy_at             = "Close",
  sell_at            = "Close",
  returns_method     = "ln",
  short_fees         = NULL,
  enforce_date_range = TRUE,
  silent             = FALSE
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
  
  assets[sort(names(assets))] %>% {
    
    if(enforce_date_range){
      
      needed_dates <- trading_dates() %>% {
        as.character(
          zoo::index(
            xts::xts(rep("", length(.)), order.by = as.Date(.))[date_range_xts]
          )
        )
      }
      
      . <- .[
        vapply(
          .,
          function(asset){
            identical(
              setdiff(needed_dates, as.character(zoo::index(asset$prices))),
              character(0)
            )
          },
          logical(1)
        ) %>% {
          if(!silent){
            usethis::ui_info(
              paste0(
                "Full historical data is not available for the date range ",
                crayon::bold(date_range_xts), " for the following assets:\t",
                paste(crayon::bold(names(.)[which(!.)]), collapse = ", "),
                ".\nThese assets were removed from output."
              )
            )
          }
          names(.)[which(.)]
        }
      ]
      
    }
    
    .
    
  } %>% purrr::map(
    function(asset){
      
      asset_name <- attr(asset, "Symbol")
      asset      <- asset[date_range_xts, c(buy_at, sell_at), silent = TRUE]
      
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
      
      if(any(colnames(asset) == "symbol")){
        asset_name <- paste(
          unique(as.character(asset$symbol)), collapse = "-"
        )              
      }
      
      asset <- asset[, find_numeric_columns(asset), silent = TRUE]
      
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
