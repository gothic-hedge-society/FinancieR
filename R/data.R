shd_description <- function(
  rox_list = list(
    "@description Contains daily high, low, open, close, and volume ",
    "histories, plus dividend and split events, for ",
    length(FinancieR::stock_data), 
    " (somewhat randomly) selected stocks."
  )
){ do.call("paste0", rox_list) }

currency_identifiers_table <- function(
  rox_list = list(
    "@description Names, symbols, and country of origin for currencies ",
    "downloaded from \\url{https://www.xe.com/symbols.php} stored in a tibble"
  )
){ do.call("paste0", rox_list) }

#' @title Stock Data
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
#'   ## Daily low prices for Chubb from 01 June 2014 to the end of 2015
#'   chubb_low <- stock_data$CB$prices$Low["2014-06-01/2015"]
#'   tail(chubb_low)
#'
#'   ## All prices data for Pepsico for 2012
#'   pep_2012 <- stock_data$PEP$prices["2012"]
#'   tail(pep_2012)
#'  
#'   ## High & Low prices for GD for March 2017
#'   gd_high_and_low <- stock_data$GD$prices["2017-03", c("High", "Low")]
#'   tail(gd_high_and_low)
#'  
#'   ## Dividends paid by Johnson & Johnson in 2017
#'   stock_data$JNJ$dividends["2017"]
#'  
#'   ## Stock splits for AAPL
#'   stock_data$AAPL$splits
#'  
"stock_data"

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
#' Operating hours and related information for worldwide exchanges. Downloaded
#' from \href{https://www.xe.com/symbols.php}{Xe's World Currency Symbols} and
#' stored as a tibble.
#' 
#' @examples stock_exchange_hours
#' 
"stock_exchange_hours"
