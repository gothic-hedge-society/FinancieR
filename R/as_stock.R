identifiers_param <- function(
  rox_list = list(
    "@param identifiers a named character vector whose names must be one of: 
    \\strong{",
    paste0(FinancieR:::identifier_names, collapse = ", "),
    "}. The value of the first identifier will be used as the name of the 
    result returned. You may specify as many identifiers as you want, but you 
    must specify at least one."
  )
){ do.call("paste0", rox_list) }

#' Convert to Class "\strong{\code{stock}}"
#' 
#' Coerce a list containing historical financial data for a stock, a stock's 
#'  options chain, or a commodity's price forward curve into an R object having
#'  class \strong{asset}.
#'  
#' @eval identifiers_param()
#' @param exchange the stock exchange on which the data were observed. Must be
#'  one of the values appearing in \code{stock_exchange_hours$ID}; e.g., "NYSE",
#'  "NASDAQ", etc. Not case-sensitive.
#' @param OHLCV an xts object containing 5 columns: \strong{Open}, 
#'  \strong{High}, \strong{Low}, \strong{Close}, and \strong{Volume} data for a 
#'  stock. Not case-sensitive.
#' @param dividends an xts object containing at least one column named 
#'  \strong{Cash} or \strong{Stock}, plus any number of additional columns
#'  desired in the result returned (such as "announced_date", "frequency", etc),
#'  indexed by \strong{Ex Date}. The value of an element appearing in the 
#'  \strong{Cash} column is the cash amount, per share, that went ex-div on the
#'  date specified for index. Ditto for \strong{Stock}.
#' @param splits an xts object containing two columns named \strong{Denominator}
#'  and \strong{Numerator}. See \emph{Details}.
#' @param ... additional data to include, such as \code{float}, 
#'  \code{earnings_announcement}, \code{warrants}, etc.
#' 
#' @section Rules
#' 

as_stock <- function(identifiers, OHLCV, dividends, splits, exchange){
  
  identifiers <- match.arg(
    arg        = names(identifiers), 
    choices    = identifier_names,
    several.ok = TRUE
  )
  
}
