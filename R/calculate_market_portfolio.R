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
#' @param rf The risk-free rate to use in calculation of
#'   \href{https://www.investopedia.com/articles/07/sharpe_ratio.asp}{Sharpe Ratio}, 
#'   in decimal form. Defaults to \strong{0.00822}% daily return (about 3%
#'   annually). If you specify a different value for \emph{rf}, \strong{make
#'   sure its time basis matches the one used for you other inputs}, (i.e., if
#'   \emph{exp_rtn} contains monthly returns, use monthly risk-free rate)!
#'   
#' @param allow_shorts Defaults to FALSE; set to TRUE to allow shorting of all
#'   the assets. There are MANY ways to do this, but by default
#'   \emph{calculate_market_portfolio}() simply treats shorts as another asset
#'   whose expected return equals negative the expected return of going long. 
#' 
#' @return A list describing the market portfolio (MP). Has four elements:
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
#'   If \emph{total_capital} and \emph{prices} are specified, then two more
#'   elements are included in the list output:
#'   \describe{
#'     \item{\code{shares}, named numeric:}{
#'     Integer number of shares to be bought 
#'     }
#'     \item{\code{cash_balance}, named numeric vector:}{The values of 
#'       \code{weights} range from 0 to 1 and denote the fraction of the total
#'       MP value allocated to the asset whose identifier is that value's name.}
#'     \item{\code{ex_return}, numeric:}{The return that is expected (i.e., 
#'     predicted/forecast) for the MP over the next time interval.}
#'     \item{\code{ex_volatility}:}{The volatility that is expected (i.e., 
#'     predicted/forecast) for the MP over the next time interval.}
#'   }
#'   
#' @details 
#' 
#'   All arguments which are percentages (\emph{exp_rtn}, \emph{exp_vol}, and
#'   \emph{rf}) must be supplied in decimal form; i.e., to specify "12%", use
#'   0.12, not 12.
#'   
#'   It should go without saying, but make sure that every asset is represented
#'   in the three inputs: names of \emph{exp_rtn}, \emph{exp_vol}, and the row &
#'   colnames of \emph{exp_cov}.
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
#' @examples
#' 
#' # Use the sample dataset "yahoo_adj_prices", provided with the package, for 
#' # this example. Start with the assumption that the returns that we expect for 
#' # the next period (day) are the means of the historical log returns observed 
#' # for a given date window specified by "start_dt" and "end_dt"
#' end_dt   <- as.Date(zoo::index(xts::last(yahoo_adj_prices))) # latest date
#' start_dt <- end_dt - 365*3                                   # 3-yr window
#' end_dt
#' start_dt
#' 
#' # From adjusted prices, get the observed daily historical log return
#' historical_rtn <- yahoo_adj_prices[paste0(start_dt - 1, "/", end_dt)] %>% {
#'   log(.[-1,] / lag(., k =  1, na.pad = FALSE)[-nrow(.),])
#' }
#' 
#' # Assume that the returns we expect for the next period (day) are the
#' # averages of the returns observed during the date window:
#' exp_rtn <- colMeans(historical_rtn)
#' 
#' # Assume that the volatilities we expect for the next period (day) are the
#' # standard deviations of the returns observed during the date window:
#' exp_vol <- dplyr::summarize(
#'   tibble::as_tibble(historical_rtn),
#'   dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
#' ) %>%
#'   purrr::as_vector()
#' 
#' # Assume that the correlations of returns of each asset pair that we expect
#' # for the next perid (day) are the observed historical correlations:
#' exp_cor <- stats::cor(historical_rtn, use = "pairwise.complete.obs")
#' 
#' # Calculate the market portfolio:
#' calculate_market_portfolio(exp_rtn, exp_vol, exp_cor)
#' 
#' ### Calculate the market portfolio allowing both long & short positions:
#' calculate_market_portfolio(exp_rtn, exp_vol, exp_cor, allow_shorts = TRUE)
#' 
#' @export
#' 
calculate_market_portfolio <- function(
  exp_rtn,
  exp_vol,
  exp_cor,
  rf             = 0.0000822,
  allow_shorts   = FALSE
){
  
  # Make sure names & elements are in order to avoid disaster
  exp_vol <- exp_vol[names(exp_rtn)]
  exp_cor <- exp_cor[names(exp_rtn), names(exp_rtn)]
  
  if(allow_shorts){
    
    exp_rtn <- c(
      exp_rtn, stats::setNames(exp_rtn * -1, paste0("s_", names(exp_rtn)))
    )
    
    exp_vol <- c(
      exp_vol, stats::setNames(exp_vol, paste0("s_", names(exp_vol)))
    )
    
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
  
  # initialize `portfolio_sharpe` to zero, which represents investing all
  # capital into risk-free assets only.
  portfolio_sharpe  <- 0
  
  # initialize `portfolio_weights` to 0 for all assets
  portfolio_weights    <- stats::setNames(
    rep(0, ncol(exp_cov)),
    colnames(exp_cov)
  )
  
  # Step 1: Find the highest-Sharpe, EQUALLY-WEIGHTED portfolio that can be 
  # created by selecting from the assets provided.
  
  while(TRUE){
    
    # Take all of the assets that AREN'T included in portfolio_weights...
    candidate_portfolios <- setdiff(
      names(exp_rtn), 
      names(portfolio_weights[portfolio_weights != 0])
    ) %>%
      vapply(
        
        # ...and for each one of those, add it to portfolio_weights, and weight
        # everything equally.
        function(asset){
          
          
          # initialize `weights` to the higher-scoped `portfolio_weights`
          weights <- portfolio_weights
          
          # make `weights` an equal-weighted portfolio consisting of the assets
          # that are already in the portfolio, plus `asset`.
          weights[c(names(weights[weights != 0]), asset)] <- 1 / (
            length(which(weights != 0)) + 1
          )
          
          # calculate expected return for the portfolio given by `weights`
          expected_return <- as.numeric(exp_rtn %*% as.matrix(weights))
          
          # calculate expected volatility for the portfolio given by `weights`
          expected_volatility <- sqrt(
            as.numeric((weights %*% exp_cov) %*% as.matrix(weights))
          )
          
          tibble::lst(
            "sharpe"  = (expected_return - rf) / expected_volatility,
            "weights" = weights
          )
          
        },
        FUN.VALUE = list(numeric(1), numeric(length(portfolio_weights)))
      )
    
    # If your best portfolio is better than the current record, store it.
    if(portfolio_sharpe < max(unlist(candidate_portfolios["sharpe",]))){
      
      portfolio_weights <- candidate_portfolios[[
        "weights",
        which.max(unlist(candidate_portfolios["sharpe",]))
      ]]
      
      portfolio_sharpe <- max(unlist(candidate_portfolios["sharpe",]))
      
    } else {
      # If none of your portfolios beat your current one, then stop adding
      # new assets, exit the while() and move on.
      break()
    }
    
  }
  
  # Initialize loop vars
  step <- min(portfolio_weights[portfolio_weights != 0]) / 10
  counts <- 0
  
  # Step 2: Refine the rough portfolio found in step 1.
  
  while(TRUE){
    
    # make `buy_sell_matrix`: a matrix whose row names are all the assets that
    #   appear in the inputs, and whose column names are all the assets whose 
    #   `portfolio_weights` are >= `step`. The values of `buy_sell_matrix` are 
    #   the Sharpe ratios that result if you start with `portfolio_weights` and 
    #   SELL `step` worth of the asset given buy the column index, and BUY 
    #   `step` worth of the asset in the row index.
    # Obviously buying and selling the same asset is not useful -- these cells 
    #   are given the value -999 in `buy_sell_matrix`.
    
    buy_sell_matrix <- vapply(
      # Step through the assets whose portfolio weights are >= to the amount
      #   we'll be adding/subtracting (otherwise they'll get negative weights)
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
              
              as.numeric(exp_rtn %*% as.matrix(wts) - rf) / sqrt(
                as.numeric((wts %*% exp_cov) %*% as.matrix(wts))
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
        as.numeric(exp_rtn %*% as.matrix(portfolio_weights)) - rf
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
  
  list(
    "sharpe"        = portfolio_sharpe,
    "weights"       = portfolio_weights,
    "ex_return"     = as.numeric(exp_rtn %*% as.matrix(portfolio_weights)),
    "ex_volatility" = sqrt(
      as.numeric(
        (portfolio_weights %*% exp_cov) %*% as.matrix(portfolio_weights)
      )
    )
  )
  
}



