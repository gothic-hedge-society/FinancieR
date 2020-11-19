parameter_j <- function(){
  paste0(
    "@param j Character vector specifying column names to be included in the ",
    "output. Available column names are:\n\n",
    stock_data %>%
      lapply(
        function(stock){
          structure(stock, class = "list") %>%
            lapply(
              function(stock_component){
                colnames(stock_component)
              }
            )
        }
      ) %>%
      unlist() %>%
      unique() %>% {
        paste0("\"", paste(., collapse = "\", \""), "\"")
      }
  )
}
#' Using `[` on Objects of Class "stock"
#'
#' You can quickly retrieve price, dividend, and split data useing the `[`
#' operator on an object of class \emph{stock}, much like subsetting a matrix or
#' list in base R.
#'
#' @param x an object of class "\emph{stock}".
#'
#' @param i an \code{\link[xts]{xts}} subsetting string following the same rules
#'   as for an \code{\link[xts]{xts}} object: "\strong{2015}" will return data
#'   for all of 2015, "\strong{2018-04}" will return data for March of 2018
#'   only, "\strong{2016-09-19/2016-12-31}" will return data from 19 Sep 2016 to
#'   the end of the year, etc. If NULL, then the full time range of available
#'   data for the selected stock will be returned.
#'
#' @eval parameter_j()
#'
#' @param silent Boolean. If FALSE (Default), messages will be displayed for
#'   special cases that might produce unexpected behavior; e.g., if no data
#'   is available for the date range specified. If TRUE, these messages will
#'   not be displayed.
#'
#' @details The rules are:
#' \enumerate{
#'  \item If a dividend took place during the specified date range, then the
#'    column "\strong{DividendAmount}" will always be included in the output.
#'  \item If a split took place during the specified date range, then the
#'    columns "\strong{Denominator}" and "\strong{Numerator}" will always be
#'    included in the output. "\strong{Denominator}" refers to the number of
#'    shares AFTER the split and "\strong{Numerator}" number of shares BEFORE
#'    the split; for example, a 2-to-1 split would have "\strong{Denominator}"
#'    = 2 and "\strong{Numerator}" = 1.
#'    }
#'
#' @return an xts object, or NULL if no data is available for the specified
#'   asset within the specified date range.
#'
#' @example inst/examples/extract_stock_ex.R
#'
#' @aliases extract_stock
#'
#' @export
#'
`[.stock` <- function(x, i, j = NULL, silent = FALSE){

  stock_extract_try <- tryCatch(
    {

      if(is.null(i)){ i <- paste0(trading_dates()[1], "/") }

      stock_block <- stock_blockify(x, i, j)

      if(is.null(stock_block)){
        if(silent){
          return(NULL)
        } else {
          usethis::ui_oops(
            paste0(
              "No data found for ",
              attr(x, "Symbol"),
              " during date range ",
              i,
              "."
            )
          )
        }
      }

      stock_block

    },
    error = function(e){e}
  )
  if(inherits(stock_extract_try, "error")){
    NextMethod()
  } else {
    stock_extract_try
  }

}
