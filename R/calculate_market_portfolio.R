#' Calculate Market Portfolio
#' 
#' Calculate the Sharpe-optimal market portfolio available for a set of assets
#' given the expected returns, volatilities, and correlations of returns for
#' each asset.
#' 
#' @param exp_rtn Named numeric vector for which each element is the return
#'   expected for the asset specified by the element's name.
#'   
#' @param exp_vol Named numeric vector for which each element is the volatility
#'   expected for the asset specified by the element's name. 
#' 
#' @param exp_cor Named numeric matrix specifying the expected covariance of
#'   returns for each asset pair.
#' 
#' @param rfr The risk-free rate (in decimal form; i.e., to specify a rate of
#'   "3%" use "0.03", \emph{not} "3") that \strong{YOU} can earn on cash with
#'   reasonable liquidity constraints that \strong{YOU} can tolerate. For big
#'   banks and in economic textbooks, this rate is usually the current rate on
#'   3-month T-bills (or even higher). That might be just fine for you if you
#'   plan on rebalancing once a year. For a trader running a strategy that can't
#'   tolerate cash being tied up for 3 months at a time, the \emph{rfr} should
#'   be set to whatever interest rate your brokerage is giving you on cash in
#'   your trading account. Used in calculation of the
#'   \href{https://www.investopedia.com/articles/07/sharpe_ratio.asp}{Sharpe
#'   Ratio}. Defaults to \strong{0.0027397}% daily return (about 1% annually).
#'   If you specify a different value for \emph{rfr}, \strong{make sure its time
#'   basis matches the one used for you other inputs}, (i.e., if \emph{exp_rtn}
#'   contains monthly returns, use monthly risk-free rate)!
#'   
#' @param prices Optional: a named numeric vector for which each name is the
#'   identifier of an asset and each element is the current price of that asset
#'   for which a market portfolio is to be calculated on a shares basis. See the
#'   "Returns" section for more info.
#'   
#' @param portfolio_aum Optional: numeric, length 1, giving the total amount of
#'   assets under management for which a market portfolio is to be calculated.
#'   In other words, \emph{portfolio_aum} = "total cash at risk".
#'    
#' @details 
#' 
#'   All arguments which are percentages (\emph{exp_rtn}, \emph{exp_vol}, and
#'   \emph{rfr}) must be supplied in decimal form; i.e., to specify "12%", use
#'   0.12, not 12.
#'   
#'   It should go without saying, but make sure that every asset is represented
#'   in the three inputs: names of \emph{exp_rtn}, \emph{exp_vol}, and the row &
#'   colnames of \emph{exp_cor}.
#' 
#'   This function works by finding the Sharpe-optimum equal-weighted portfolio
#'   that can be created using the assets passed in. Using that portfolio as a
#'   starting point, the function finds the assets A and B such that
#'   reallocating ("selling") a small amount (\emph{step}) of A into asset B
#'   ("buying") results in a portfolio whose Sharpe is greater than all other
#'   possible assets A and B. The portfolio variable is updated.
#'   
#'   This process is repeated with smaller and smaller \emph{step} sizes. When
#'   it is not possible to reallocate \emph{step} amount of any asset A into any
#'   other asset B so as to create a portfolio having a better Sharpe, the
#'   Market Portfolio has been reached and the function returns the value.
#'   
#'   \strong{Weights} mode:
#'   If either \emph{prices} or \emph{portfolio_aum} is unspecified in the call
#'   to \emph{calculate_market_portfolio}(), then the function operates in
#'   "\emph{Weights Mode}" and will attempt to answer the question: "What is the
#'   the optimum fractional, real-valued weight of each asset in the
#'   Sharpe-optimal MP?" Because the Sharpe ratio and the optimum weights are
#'   real numbers, this operation can be computed with arbitrary precision, to
#'   infinte decimal places. \emph{calculate_market_portfolio}() was written to
#'   be clearly understood and run reasonably fast, and testing indicates that
#'   the weights are reliable to within +/- 1% (i.e., "0.18" to "0.20" for a
#'   reported weight of "0.19") of the optimal value. As such, weights reported
#'   by \emph{weights mode} should be considered somewhat approximate.
#' 
#'   \strong{Shares} mode: 
#'   A much different situation arises when we ask the question: "Given $5MM in
#'   capital, what's the optimum portfolio that can be built from a set of
#'   assets trading at given prices?" Note that the formulation of this question
#'   requires both \emph{prices} and \emph{portfolio_aum} to be provided.
#'   Because shares can only be bought in integer values, and because the sum of
#'   the capital allocated to each asset (including "cash") must equal
#'   \emph{portfolio_aum}, the number of values available for the \emph{shares}
#'   vector is countably finite and it is possible to find an exact solution.
#' 
#' @return 
#'   If \strong{weights basis} (\emph{prices} and \emph{portfolio_aum} 
#'   not specified): #'   A list describing the market portfolio (MP). Contains
#'   the following four elements:
#'   
#'   \describe{
#'     \item{\code{sharpe}, numeric:}{Sharpe Ratio of MP}
#'     \item{\code{weights}, named numeric vector:}{The values of 
#'       \code{weights} range from 0 to 1 and denote the fraction of the total
#'       MP value allocated to the asset whose identifier is that value's name.}
#'     \item{\code{ex_return}, numeric:}{The return that is expected (i.e., 
#'     predicted/forecast) for the MP over the next time interval.}
#'     \item{\code{ex_volatility}:}{The volatility that is expected (i.e., 
#'     predicted/forecast) for the MP over the next time interval.}
#'   }
#'   
#'   If \strong{shares basis} (\emph{prices} and \emph{portfolio_aum}
#'   specified): \emph{calculate_market_portfolio}() will optimize the portfolio
#'   assuming that  \emph{portfolio_aum} is available for investing and that the
#'   price of each asset is given by \emph{prices}. Returns a list describing
#'   the market portfolio (MP) which contains the for elements described above,
#'   plus three more:
#'   
#'   \describe{
#'     \item{shares:}{Named numeric vector giving the number of shares of each
#'     asset calculated for the market portfolio}
#'     \item{prices:}{Named umeric vector giving the prices used by 
#'     calculate_market_portfolio() to determine the MP.}
#'     \item{cash:}{Numeric, length 1. Leftover cash not invested in assets.}
#'   }
#'   
#' @example inst/examples/calculate_market_portfolio_ex.R
#' 
#' @export
#' 
calculate_market_portfolio <- function(
  exp_rtn,
  exp_vol,
  exp_cor,
  rfr                = 0.000027397,
  prices             = NULL,
  portfolio_aum      = NULL,
  shortable_shares   = NULL,
  initial_margin     = NULL,
  maintenance_margin = NULL
){
  
  # Make sure names & elements are in order to avoid disaster
  exp_vol      <- exp_vol[names(exp_rtn)]
  exp_cor      <- exp_cor[names(exp_rtn), names(exp_rtn)]
  allow_shorts <- any(grepl("^_s", names(exp_rtn)))
  
  if(allow_shorts){
    exp_rtn <- c(exp_rtn, "cash" = 0)
    exp_vol <- c(exp_vol, "cash" = 0)
    exp_cor <- rbind(
      cbind(
        exp_cor,
        magrittr::set_colnames(exp_cor * -1, paste0("s_", colnames(exp_cor)))
      ),
      cbind(
        magrittr::set_rownames(exp_cor * -1, paste0("s_", colnames(exp_cor))),
        exp_cor
      )
    )
  }
  # create covariance matrix of returns
  exp_cov <- exp_cor * (as.matrix(exp_vol) %*% exp_vol)
  
  # Step 1: Find the highest-Sharpe, EQUALLY-WEIGHTED portfolio that can be 
  # created by selecting from the assets provided.
  mp <- best_equal_weighted_portfolio(exp_rtn, exp_cov, rfr)
  
  # Step 2: Refine the rough portfolio found in step 1.
  
  while(TRUE){
    
    # Initialize loop vars
    step   <- min(mp$weights[mp$weights != 0]) / 10
    counts <- 0
    
    mp_refined <- refine_weights(mp, exp_rtn, exp_cov, rfr, step)
    
    # If we got a better sharpe ratio in `buy_sell_matrix`, keep it & update!
    if(max(buy_sell_matrix) > portfolio_sharpe){
      
      add_to_asset    <- rownames(buy_sell_matrix)[
        which(buy_sell_matrix == max(buy_sell_matrix), arr.ind = TRUE)[1]
      ]
      
      take_from_asset <- colnames(buy_sell_matrix)[
        which(buy_sell_matrix == max(buy_sell_matrix), arr.ind = TRUE)[2]
      ]
      
      portfolio_weights[add_to_asset] <- portfolio_weights[
        add_to_asset
      ] + step
      
      portfolio_weights[take_from_asset] <- portfolio_weights[
        take_from_asset
      ] - step
      
      portfolio_sharpe <-  (
        as.numeric(exp_rtn %*% as.matrix(portfolio_weights)) - rfr
      ) / sqrt(
        as.numeric(
          (portfolio_weights %*% exp_cov) %*% as.matrix(portfolio_weights)
        )
      )
      
      
    } else {
      # drop `step` by a factor of 10, but only do this `count` number of times.
      step <- min(portfolio_weights[portfolio_weights != 0]) / 10
      counts <- counts + 1
      
      if(counts >= 10){
        break()
      }
    }
  }
  
  mp <- list(
    "sharpe"        = portfolio_sharpe,
    "weights"       = compactify(portfolio_weights),
    "ex_return"     = as.numeric(exp_rtn %*% as.matrix(portfolio_weights)),
    "ex_volatility" = sqrt(
      as.numeric(
        (portfolio_weights %*% exp_cov) %*% as.matrix(portfolio_weights)
      )
    )
  )
  
  if(is.null(prices) || is.null(portfolio_aum)) return(mp)
  
  rm(
    list = c(
      "buy_sell_matrix", "candidate_portfolios", "add_to_asset", "counts", 
      "take_from_asset", "step", "portfolio_sharpe", "portfolio_weights"
    )
  )
  
  mp$shares <- (portfolio$weights * portfolio_aum / prices) %>% {
    .[which(. < 0)] <- ceiling(.[which(. < 0)])
    .[which(. > 0)] <- floor(.[which(. > 0)])
    compactify(.)
  }
  mp$prices        <- prices
  mp$weights       <- mp$shares * prices / portfolio_aum
  mp$ex_return     <- as.numeric(exp_rtn %*% as.matrix(mp$weights))
  mp$ex_volatility <- sqrt(
    as.numeric((mp$weights %*% exp_cov) %*% as.matrix(mp$weights))
  )
  mp$cash          <- portfolio_aum - (mp$shares %*% mp$prices)
  
  
  while(TRUE){
    mp_pocket <- calc_mp_pocket(mp, exp_rtn, exp_vol, exp_cov)
    if(mp_pocket$sharpe > mp$sharpe){
      mp <- mp_pocket
    } else {
      break()
    }
  }
  
  mp
  
}
