#' Rebalance an Existing Portfolio
#' 
#' Given an existing portfolio and a set of expected returns, volatilities, and
#' a correlation matrix, calculate the trades required and the MP that would
#' result if the portfolio were rebalanced at specified market prices. Assets
#' passed in via the parameters \emph{exp_rtn}, \emph{exp_vol}, and
#' \emph{exp_cor} specify the universe, or choices available, for inclusion in
#' the market portfolio, but not every asset passed in will necessarily appear
#' in the optimized MP.
#' 
#' @inheritParams calculate_market_portfolio
#'   
#' @example inst/examples/rebalance_portfolio_ex.R
#'   
#' @export
#' 
rebalance_portfolio <- function(portfolio, exp_rtn, exp_vol, exp_cor, prices){
 
  
   
}
