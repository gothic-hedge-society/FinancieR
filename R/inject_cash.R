#' Add Cash to an Existing Portfolio
#' 
#' \strong{this function is under development and will be added soon}.
#' 
#' Calculate the optimum allocation of new cash injected into an existing
#' portfolio without rebalancing. Assets passed in via the parameters
#' \emph{exp_rtn}, \emph{exp_vol}, and \emph{exp_cor} specify the universe, or
#' choices available, for inclusion in the market portfolio, but not every asset
#' passed in will necessarily appear in the optimized MP.
#' 
#' \emph{inject_cash}() is the "inverse" of \link{withdraw_cash}().
#' 
#' @param positions Named numeric vector giving the number of shares of each
#'   asset held in an existing portfolio.
#'   
#' @param new_cash Numeric, length 1, specifying the amount of \strong{new} cash
#'   intended to be invested into the portfolio.
#'   
#' @inheritParams calculate_market_portfolio
#'   
#' @example inst/examples/inject_cash_ex.R
#'   
#' @export
#'   
inject_cash <- function(
  positions, 
  prices,
  exp_rtn,
  exp_vol,
  exp_cor,
  rfr      = 0.000027397,
  new_cash
){
  
}
