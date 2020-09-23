#' Trading Dates
#'
#' Returns a date vector containing all the dates for which a historical stock
#' price was reported by \strong{any} stock in \emph{stock_data}.
#'
#' @format date vector, ordered from oldest to most recent.
#'
trading_dates <- function(){
  stock_data %>%
    lapply(
      function(stock){
        as.character(as.Date(zoo::index(stock$prices)))
      }
    ) %>%
    unlist(use.names = FALSE) %>%
    unique() %>%
    sort() %>%
    as.Date()
}
