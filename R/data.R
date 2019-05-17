shd_description <- function(
  rox_list = list(
    "@description Contains daily high, low, open, close, and volume ",
    "histories, plus dividend and split events, for ",
    length(FinancieR::sample_historical_data), 
    " (somewhat randomly) selected stocks."
  )
){ do.call("paste0", rox_list) }

currency_identifiers_table <- function(
  rox_list = list(
    "@description Names, symbols, and country of origin for currencies ",
    "downloaded from \\url{https://www.xe.com/symbols.php} stored in a tibble"
  )
){ do.call("paste0", rox_list) }

stock_exchange_hours_table <- function(
  rox_list = list(
    "@description Operating hours and related information for worldwide ",
    "exchanges. Downloaded from \\url{https://www.xe.com/symbols.php} stored",
    "in a tibble: ",   
    tabular(FinancieR::stock_exchange_hours)
  )
){ do.call("paste0", rox_list) }

#' @title Sample Historical Data
#' 
#' @eval shd_description()
#'   
#' @format A list in which each element is another list, and is named using the
#'   identifier of the asset to which its constituent data corresponds (e.g., 
#'   the element named "AAPL" contains data for Apple, Inc.). 
#'   
#'   Each of the constituent lists elements contains the following elements:
#'   
#'  \describe{
#'   \item{\strong{\code{dividends}}:}{
#'     An xts object having a single column -- \code{DividendAmount} -- giving
#'       the total value of any and all dividends that went ex-div on the date
#'       given by the index of the xts. 
#'   }
#'   \item{\strong{\code{OHLCV}}:}{
#'     A 5-column xts object having column names \code{High}, \code{Low}, 
#'     \code{Open}, \code{Close}, \code{Volume}. Each element is the daily 
#'     historical price (or volume) recorded for the day specified by the index.
#'   }
#'   \item{\strong{\code{splits}}:}{
#'     A 2-column xts object having column names \code{Denominator}, and 
#'     \code{Numerator}, where \code{Denominator} refers to equivalent stock 
#'     AFTER split, and \code{Numerator} refers to stock BEFORE split; e.g, a 
#'     "2-for-1" split would show \code{Denominator} = 2, \code{Numerator} = 1. 
#'     The index of the xts object corresponds to the ex-date of the split 
#'     described by \code{Denominator} and \code{Numerator}.
#'   }
#'  }
#'  
#' @examples
#'   low_prices_c <- sample_historical_data$C$hlocv$Low["2014-06-01/2015"]
#'   ## Daily low prices for Citibank from 01 June 2014 to the end of 2015
#'
#'   hlocv_stag   <- sample_historical_data$STAG$hlocv["2012"]
#'   ## High, Low, Open, Close, Volume data for STAG for 2012
#'  
#'   hlocv_gd     <- sample_historical_data$GD$hlocv["2017-03", c("High", "Low")]
#'   ## High & Low prices for GD for March 2017
#'  
#'   divs_son     <- sample_historical_data$SON$dividends["2017"]
#'   ## Dividends paid by SON in 2017
#'  
#'   splits_aapl  <- sample_historical_data$AAPL$splits
#'   ## Stock splits for AAPL
#'  
"sample_historical_data"

#' Daily Yield Curve Rates
#' 
#' Daily rates downloaded from the US Department of the Treasury.
#' \url{https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield}
#'
#' @format An xts object containing daily yield curve rates for the following
#'   time periods, which appear as column names in the xts: 
#'     1 Mo, 2 Mo, 3 Mo, 6 Mo, 1 Yr, 2 Yr, 3 Yr, 5 Yr, 7 Yr, 10 Yr, 20 Yr, 30 Yr
"daily_treasury_yield_curve_rates"

#' Currency Identifiers
#'
#' @eval currency_identifiers_table()
#' 
"currency_identifiers"

#' Exchange Hours
#' 
#' @eval stock_exchange_hours_table()
#' 
"stock_exchange_hours"
