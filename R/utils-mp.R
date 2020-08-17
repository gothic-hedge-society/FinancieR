
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

refine_weights <- function(mkt_p, rtn, vol, cov_mtx, rfr){
  
  # mkt_p   <<- mkt_p
  # rtn     <<- rtn
  # cov_mtx <<- cov_mtx
  # rfr     <<- rfr
  # stp     <<- stp
  # stop("wadup")
  
  # make `buy_sell_matrix`: a matrix whose row names are all the assets that
  #   appear in the inputs, and whose column names are all the assets whose 
  #   `mkt_p$weights` are >= `stp`. The values of `buy_sell_matrix` are 
  #   the Sharpe ratios that result if you start with `mkt_p$weights` and 
  #   SELL `stp` worth of the asset given buy the column index, and BUY 
  #   `stp` worth of the asset in the row index.
  # Obviously buying and selling the same asset is not useful -- these cells 
  #   are given the value -999 in `buy_sell_matrix`.
  
  # Step through the assets whose portfolio weights are >= to the amount
  #   we'll be adding/subtracting (otherwise they'll get negative weights)
  
  # Initialize loop vars
  stp    <- min(mkt_p$weights[mkt_p$weights != 0]) / 10
  counts <- 0
  
  while(TRUE){
    
    buy_sell_matrix <- names(mkt_p$weights[mkt_p$weights >= stp]) %>%
      vapply(
        function(take_from_asset){
          
          # initialize `weights` to the higher-scoped `mkt_p$weights`
          weights <- mkt_p$weights
          
          # take weight `stp` FROM an asset.
          weights[take_from_asset] <- weights[take_from_asset] - stp
          
          # create the column for `buy_sell_matrix`, and make sure it's ordered
          #   in the same order as `mkt_p$weights`.
          c(
            stats::setNames(-999, take_from_asset),
            vapply(
              setdiff(names(mkt_p$weights), take_from_asset),
              function(add_to_asset, wts = weights){
                
                wts[add_to_asset] <- wts[add_to_asset] + stp
                
                as.numeric(rtn %*% as.matrix(wts) - rfr) / sqrt(
                  as.numeric((wts %*% cov_mtx) %*% as.matrix(wts))
                )
                
              },
              FUN.VALUE = numeric(1)
            )
          )[names(mkt_p$weights)]
          
        },
        FUN.VALUE = numeric(length(mkt_p$weights))
      )
    
    # If we got a better Sharpe ratio in `buy_sell_matrix`, keep it & update!
    if(max(buy_sell_matrix) > mkt_p$sharpe){
      
      add_to_asset    <- rownames(buy_sell_matrix)[
        which(buy_sell_matrix == max(buy_sell_matrix), arr.ind = TRUE)[1]
      ]
      
      take_from_asset <- colnames(buy_sell_matrix)[
        which(buy_sell_matrix == max(buy_sell_matrix), arr.ind = TRUE)[2]
      ]
      
      mkt_p$weights[add_to_asset]    <- mkt_p$weights[add_to_asset] + stp
      mkt_p$weights[take_from_asset] <- mkt_p$weights[take_from_asset] - stp
      mkt_p$sharpe <- (
        as.numeric(rtn %*% as.matrix(mkt_p$weights)) - rfr
      ) / sqrt(
        as.numeric(
          (mkt_p$weights %*% cov_mtx) %*% as.matrix(mkt_p$weights)
        )
      )
      
    } else {
      # drop `stp` by a factor of 10, but only do this `count` number of times.
      stp    <- min(mkt_p$weights[mkt_p$weights != 0]) / 10
      counts <- counts + 1
      
      if(counts >= 10){
        break()
      }
      
    }
    
  }
  
  mkt_p$exp_rtn <- as.numeric(rtn %*% as.matrix(mkt_p$weights))
  mkt_p$exp_vol <- sqrt(
    as.numeric((mkt_p$weights %*% cov_mtx) %*% as.matrix(mkt_p$weights))
  )
  mkt_p$weights <- round(mkt_p$weights[which(mkt_p$weights != 0)], 9) %>% {
    .[which(. != 0)]
  }
  
  mkt_p
  
}

refine_shares <- function(mkt_p, rtn, vol, cov_mtx, rfr, prc, aum){
  mkt_p   <<- mkt_p
  rtn     <<- rtn
  vol     <<- vol
  cov_mtx <<- cov_mtx 
  rfr     <<- rfr 
  prc     <<- prc 
  aum     <<- aum
  stop("you are here.")
  
  mkt_p$shares <- (mkt_p$weights * aum / prc) %>% {
    .[which(. < 0)] <- ceiling(.[which(. < 0)])
    .[which(. > 0)] <- floor(.[which(. > 0)])
    .
  }
  mkt_p$prices       <- prc
  mkt_$weights       <- mkt_p$shares * prc / aum
  mp$exp_rtn       <- as.numeric(rtn %*% as.matrix(mk_p$weights))
  mp$exp_vol   <- sqrt(
    as.numeric((mkt_p$weights %*% cov_mtx) %*% as.matrix(mkt_p$weights))
  )
  mkt_p$cash          <- aum - (mkt_p$shares %*% mkt_p$prices)
  
  mkt_p
  
}

refine_shorts <- function(mkt_p, rtn, vol, cov_mtx, rfr, i_mgn, shtbl_shrs){
  mkt_p      <<- mkt_p
  rtn        <<- rtn
  vol        <<- vol
  cov_mtx    <<- cov_mtx
  rfr        <<- rfr
  i_mgn      <<- i_mgn
  shtbl_shrs <<- shtbl_shrs
  stop("you are here.")
  
  # calc_mp_pocket <- function(portfolio, rtn, vol, cov){
  #   portfolio <<- portfolio
  #   rtn       <<- rtn
  #   vol       <<- vol
  #   cov       <<- cov
  #   stop("you are here.")
  #   # mp$shares <- (portfolio$weights * portfolio_aum / prices) %>% {
  #   #   .[which(. < 0)] <- ceiling(.[which(. < 0)])
  #   #   .[which(. > 0)] <- floor(.[which(. > 0)])
  #   # }
  #   # mp$prices        <- prices
  #   # mp$weights       <- mp$shares * prices / portfolio_aum
  #   # mp$ex_return     <- as.numeric(exp_rtn %*% as.matrix(mp$weights))
  #   # mp$ex_volatility <- sqrt(
  #   #   as.numeric((mp$weights %*% exp_cov) %*% as.matrix(mp$weights))
  #   # )
  #   # mp$cash          <- portfolio_aum - (mp$shares %*% mp$prices)
  # }
  
  # while(TRUE){
  #   mp_pocket <- calc_mp_pocket(mp, exp_rtn, exp_vol, exp_cov)
  
  #   if(mp_pocket$sharpe > mp$sharpe){
  #     mp <- mp_pocket
  #   } else {
  #     break()
  #   }
  #   
  # }
  
}
