
# Calculates the best equally-weighted portfolio available
best_equal_weighted_portfolio <- function(rtn, cov_mtx, rfr){
  
  # The "record" (best) portfolio found so far
  mp_pocket <- list(
    "sharpe"  = 0,
    "weights" = stats::setNames(rep(0, length(rtn)), names(rtn)),
    "exp_rtn" = 0,
    "exp_vol" = 0
  )
  
  # Initialize a portfolio that will guarantee entering the while loop
  mp        <- mp_pocket
  mp$sharpe <- -1
  
  while(mp$sharpe < mp_pocket$sharpe){
    mp <- mp_pocket
    
    # Take all of the assets that AREN'T included in portfolio_weights...
    for(
      asset in setdiff(
        names(rtn),  
        names(mp$weights[mp$weights != 0])
      ) 
    ){
      # ...and for each one of those, add it to portfolio_weights, and weight
      # everything equally.
      
      # make `weights` an equal-weighted portfolio consisting of the assets
      # that are already in the portfolio, plus `asset`.
      weights <- mp$weights %>% {
        .[c(names(.[. != 0]), asset)] <- 1 / (
          length(which(. != 0)) + 1
        )
        .
      }
      # calculate expected return for the portfolio given by `weights`
      exp_rtn <- as.numeric(rtn %*% as.matrix(weights))
      # calculate expected volatility for the portfolio given by `weights`
      exp_vol <- sqrt(
        as.numeric((weights %*% cov_mtx) %*% as.matrix(weights))
      )
      # calculate expected Sharpe for the portfolio given by `weights`
      sharpe <- (exp_rtn - rfr) / exp_vol
      
      # store the portfolio if its Sharpe is superior
      if(sharpe > mp$sharpe){
        mp_pocket$sharpe  <- sharpe
        mp_pocket$weights <- weights
        mp_pocket$exp_rtn <- exp_rtn
        mp_pocket$exp_vol <- exp_vol
      }
    }
  }
  
  mp
  
}

refine_weights <- function(mp_pocket, rtn, cov_mtx, rfr, stp){
  
  mp_pocket <<- mp_pocket
  rtn       <<- rtn
  cov_mtx   <<- cov_mtx
  rfr       <<- rfr
  stp      <<- stp
  stop("yolo")
  
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
            
            as.numeric(exp_rtn %*% as.matrix(wts) - rfr) / sqrt(
              as.numeric((wts %*% exp_cov) %*% as.matrix(wts))
            )
            
          },
          FUN.VALUE = numeric(1)
        )
      )[names(portfolio_weights)]
      
    },
    FUN.VALUE = numeric(length(portfolio_weights))
  )
}


compactify <- function(portfolio_vec){
  portfolio_vec %>% {
    if(cpct){
      . <- .[which(. != 0)]
      if(any(grepl("^s_", names(.)))){
        .[grep("^s_", names(.))] <- .[grep("^s_", names(.))] * -1
        names(.)[grep("^s_", names(.))] <- gsub(
          "^s_", "", grep("^s_", names(.), value = TRUE)
        )
        .
      }
      .
    } else {
      .
    }
  }
}

calc_mp_pocket <- function(portfolio, rtn, vol, cov){
  portfolio <<- portfolio
  rtn       <<- rtn
  vol       <<- vol
  cov       <<- cov
  stop("you are here.")
  # mp$shares <- (portfolio$weights * portfolio_aum / prices) %>% {
  #   .[which(. < 0)] <- ceiling(.[which(. < 0)])
  #   .[which(. > 0)] <- floor(.[which(. > 0)])
  #   compactify(.)
  # }
  # mp$prices        <- prices
  # mp$weights       <- mp$shares * prices / portfolio_aum
  # mp$ex_return     <- as.numeric(exp_rtn %*% as.matrix(mp$weights))
  # mp$ex_volatility <- sqrt(
  #   as.numeric((mp$weights %*% exp_cov) %*% as.matrix(mp$weights))
  # )
  # mp$cash          <- portfolio_aum - (mp$shares %*% mp$prices)
}
