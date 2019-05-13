#' Market Portfolio (MP) from Historical Returns
#' 
#' Calculate optimal portfolio weighting fractions
#' 
#' @param returns_xts An xts object whose values are period-over-period returns
#'   observed for the assets specified by the identifiers in the column names.
#' 
#' @param rf The current prevailing risk-free rate.
#' 
#' @return A list describing the market portfolio (MP) found by 
#'   \code{market_portfolio_from_hist_rtn()}, having four elements:
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
#' @details Returns are NOT assumed to be in 'percent form': i.e., make sure that in 
#' whatever xts is passed as \code{returns_xts}, a return of \emph{12\%} is 
#' represented as \emph{0.12}.
#' 
#' @export
market_portfolio_from_hist_rtn <- function(returns_xts, rf = 0.0025){
  
  requireNamespace("xts")
  
  # calcuate expected returns using geometric mean of the data for which
  #   the user has determined the time range.
  asset_expected_return <- gmrr(returns_xts = returns_xts)
  
  # create covariance matrix of returns
  covariance_matrix <- stats::cov(returns_xts, use = "pairwise.complete.obs")
  
  # initialize `portfolio_sharpe` to an impossibly low value
  portfolio_sharpe     <- -9999
  
  # initialize `portfolio_weights` to 0 for all assets
  portfolio_weights    <- stats::setNames(
    rep(0, ncol(returns_xts)),
    names(returns_xts)
  )
  
  # initialize divisor to 1
  divisor <- 1
  
  
  # Step 1: Create a rough initial portfolio.
  
  while(TRUE){
    
    candidate_portfolios <- vapply(
      # vapply: start stepping through assets that haven't been added to the 
      #   portfolio yet. 
      setdiff(
        names(returns_xts), 
        names(portfolio_weights[portfolio_weights != 0])
      ),
      function(asset){
        
        # initialize `weights` to the higher-scoped `portfolio_weights`
        weights <- portfolio_weights
        
        # try an equal-weighted portfolio consisting of the assets that are 
        #   already in the portfolio, plus `asset`.
        weights[c(
          names(weights[weights != 0]),
          asset
        )] <- 1 / divisor
        
        # calculate expected return for the portfolio given by `weights`
        expected_return <- sum(asset_expected_return * weights)
        
        # calculate expected volatility for the portfolio given by `weights`
        expected_volatility <- sum(
          tcrossprod(as.numeric(weights)) * covariance_matrix
        ) %>%
          sqrt()
        
        # return a tibble containing the sharpe ratio of the portfolio and the
        #   portfolio weightings.
        tibble::lst(
          "sharpe"  = (expected_return - rf) / expected_volatility,
          "weights" = weights
        )
      },
      FUN.VALUE = list(numeric(1), numeric(length(portfolio_weights)))
    ) 
    
    # If your best portfolio is better than the current record, store it.
    if(portfolio_sharpe < max(unlist(candidate_portfolios["sharpe",]))){
      
      divisor <- divisor + 1
      
      portfolio_weights <- candidate_portfolios[[
        "weights",
        which.max(unlist(candidate_portfolios["sharpe",]))
        ]]
      
      portfolio_sharpe <- max(unlist(candidate_portfolios["sharpe",]))
      
    } else {
      # If none of your portfolios beat your current one, then stop.
      break()
    }
  }
  
  # Initialize loop vars
  step <- min(portfolio_weights[portfolio_weights != 0]) / 10
  counts <- 0
  
  
  # Step 2: Refine the rough portfolio found in step 1.
  
  while(TRUE){
    
    # make `buy_sell_matrix`: a matrix whose row names are all the assets that
    #   appear in returns_xts, and whose column names are all the assets whose 
    #   `portfolio_weights` are >= `step`. The values of `buy_sell_matrix` are 
    #   the sharpe ratios that result if you start with `portfolio_weights` and 
    #   SELL `step` worth of the asset given buy the column index, and BUY 
    #   `step` worth of the asset in the row index.
    # Obviously buying and selling the same asset is not useful -- these cells 
    #   are given the value -999 in `buy_sell_matrix`.
    
    buy_sell_matrix <- vapply(
      # Step through the assets whose portfolio weights are >= to the amount
      #   we'll be adding/subtracting (otherwise the'll get negative weights)
      names(portfolio_weights[portfolio_weights >= step]),
      function(take_from_asset){
        
        # initialize `weights` to the higher-scoped `portfolio_weights`
        weights <- portfolio_weights
        
        # take weight `step` FROM an asset.
        weights[take_from_asset] <- weights[take_from_asset] - step
        
        # create the column for `buy_sell_matrix`, and make sure it's ordered
        #   in the same order as `portfolio_weights`.
        c(
          stats::setNames(-999, take_from_asset),
          vapply(
            setdiff(names(portfolio_weights), take_from_asset),
            function(add_to_asset, wts = weights){
              wts[add_to_asset] <- wts[add_to_asset] + step
              (sum(asset_expected_return * wts) - rf) / sqrt(
                sum(
                  tcrossprod(as.numeric(wts)) * covariance_matrix
                )
              )
            },
            FUN.VALUE = numeric(1)
          )
        )[names(portfolio_weights)]
        
      },
      FUN.VALUE = numeric(length(portfolio_weights))
    )
    
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
        sum(
          asset_expected_return * portfolio_weights
        ) - rf
      ) / sqrt(
        sum(
          tcrossprod(as.numeric(portfolio_weights)) * covariance_matrix
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
  
  list(
    "sharpe"        = portfolio_sharpe,
    "weights"       = portfolio_weights,
    "ex_return"     = sum(asset_expected_return * portfolio_weights),
    "ex_volatility" = sqrt(
      sum(
        tcrossprod(as.numeric(portfolio_weights)) * covariance_matrix
      )
    )
  )
  
}



