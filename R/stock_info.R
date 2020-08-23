#' Stock Info
#'
#' Get all available information about a stock printed cleanly as a tibble. 
#' \emph{stock_info}() is not a data object; rather, it's a function that 
#' scans a data object (\emph{stock_data}) and compiles and returns the gathered
#' information in a readable format.
#' 
#' @section Disclaimer: Absolutely no promise, guarantee, or representation is
#'   made by the authors of FinancieR concerning the accuracy or reliability of
#'   \emph{stock_data}, or any other data in the FinancieR package.
#'   
#' @param stocks List of stocks following the same structure as
#'   \link{stock_data}. Defaults to \emph{stock_data}.
#'   
#' @param symbols Character vector containing symbols to be included in the
#'   output. Each element of \emph{symbols} must appear as a name of a stock
#'   element in \emph{stocks}. Defaults to \emph{names(stocks)}.
#'
#' @return  A \link[tibble]{tibble} containing available descriptive data for
#'   each member of \code{stock_data}. Not case-sensitive.
#'   
#' @examples 
#'   stock_info()
#'
#' @export
#'
stock_info <- function(stocks = stock_data, symbols = names(stocks)){
  symbols %>%
    unique() %>%
    toupper() %>% {
      if(length(setdiff(., names(stocks))) > 0){
        usethis::ui_oops(
          paste0(
            "The following supplied symbols do not appear as names in ",
            crayon::italic("stocks"),
            " and will be ignored: ",
            paste(
              crayon::bold(setdiff(., names(stocks))), 
              collapse = ", "
            )
          )
        )
      }
      intersect(., names(stock_data))
    } %>%
    lapply(
      function(stock_symbol){
        attributes(
          stock_data[[stock_symbol]]
        )[
          setdiff(
            names(attributes(stock_data[[stock_symbol]])), 
            c("names", "class")
          )
        ] %>%
          tibble::as_tibble_row() %>%
          dplyr::select(Symbol, Name, dplyr::everything())
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows)
}
