#' Calculate Market Portfolio
#' 
#' Calculate the Sharpe-optimal market portfolio available for a set of assets
#' given the expected returns, volatilities, and correlations of returns for
#' each asset.
#'  
#' @param exp_rtn Named numeric vector for which each element is the return
#'   expected for the next period for the asset specified by the element's name.
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
#' @section The Cost of Shorting:
#'  You would only short an asset if the return you expect for buying that asset
#'  is negative. You may think that the expected return for shorting an asset is
#'  simply the return you expect for longing the asset times -1. In reality, the
#'  return you'll get from a short sale will be a fraction of the asset times -1
#'  because your broker charges fees on short position in exchange for providing
#'  the shorting service. In addition, the capital gains from short selling may
#'  taxed at a different rate than those realized on your long positions.
#'  
#'  The key additional costs of shorting are:
#'  
#'  \describe{
#'   \item{Dividends}{
#'      You don't earn dividends on a short position. In fact, it's the
#'      opposite: if you're short a stock on a dividend's ex-date, then you must
#'      actually \emph{pay} the dividend to the owner of the stock.
#'      \emph{\link{calculate_historical_returns}}() takes this into account
#'      itself when shorting is included.
#'    }
#'    \item{Short Fees}{
#'      Your brokerage will charge a fee on a short position for every day the
#'      position is open. This fee is based on the availability of assets for
#'      shorting, and varies from asset to asset and through time. Usually the
#'      fee is calculated on each trading day and debited from the trading
#'      account on at the beginning of each month.
#'    }
#'    \item{Taxes}{
#'      Capital gains taxes on your short sales can often be taxed at a higher
#'      rate than what you might expect for long sales. If \strong{uncovered},
#'      the short position will always be taxed as a short-term capital gain
#'      because the holding period is considered by the IRS to begin on the day
#'      when the short position was closed out (bought to cover). If
#'      \strong{covered}, then the holding period is considered to be the
#'      holding period of the \emph{substantially different securities} that can
#'      be converted to the stock itself. Taxes must be taken into account in
#'      the \emph{exp_rtn} parameter as passed to
#'      \emph{calculate_market_portfolio}.
#'      
#'      Taxes will depend on your situation: whether you expect a short-term or
#'      long-term investment, your income bracket, whether or not you're an
#'      institution, etc. You must figure that out for yourself, and you might
#'      find that, once everything is taken into account, it's just not worth it
#'      to short assets in a lot of situations. Stay smart!
#'    }
#'  }
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
#'   This function answers the question: "What is the the optimum fractional,
#'   real-valued weight of each asset in the Sharpe-optimal MP?" Because the
#'   Sharpe ratio and the optimum weights are real numbers, this operation can
#'   be computed with arbitrary precision, to infinte decimal places.
#'   \emph{calculate_market_portfolio}() was written to be clearly understood
#'   and run reasonably fast, and testing indicates that the weights are
#'   reliable to within +/- 1% (i.e., "0.18" to "0.20" for a reported weight of
#'   "0.19") of the optimal value. As such, weights reported by \emph{weights
#'   mode} should be considered somewhat approximate.
#' 
#' @return 
#'   A list describing the market portfolio (MP) having four elements:
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
#' @example inst/examples/calculate_market_portfolio_ex.R
#' 
#' @export
#' 
calculate_market_portfolio <- function(
    from_data    = FinancieR::historical_data, 
    n_days       = 365, 
    balance_date = as.Date(
      unlist(historical_data[1,1], use.names = FALSE),
      origin = "1970-01-01"
    ), 
    exp_rtn   = NULL, 
    exp_vol   = NULL, 
    exp_cor   = NULL, 
    rfr       = 0, 
    startoff  = NULL, 
    silent    = FALSE,
    precision = 7
){
  
  library('doParallel')
  on.exit(parallel::stopCluster(cl))
  
  if(any(is.null(c(exp_rtn, exp_vol, exp_cor)))){
    # create exp_ vars from from_data, balance_date and n_days.
    balance_date <- as.Date(balance_date)
    historical_rtn_subset <- from_data %>%
      dplyr::filter(date >= balance_date - n_days & date < balance_date) %>% {
        .[,-unique(which(is.na(.), arr.ind = TRUE)[,"col"])]
      } %>% {
        .[
          ,
          c(
            "date",
            (.[1, grep("_close", colnames(.))] < 750) %>% {
              gsub("_close", "_rtn", colnames(.)[which(.)])
            } 
          )
        ]
      } %>% {
        colnames(.) <- gsub("_rtn", "", colnames(.))
        .
      }
    
    exp_rtn <- historical_rtn_subset %>%
      dplyr::select(-date) %>%
      FinancieR::gmrr()
    
    exp_vol <- historical_rtn_subset %>%
      dplyr::select(-date) %>%
      dplyr::summarize(
        dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
      ) %>%
      purrr::as_vector()
    
    exp_cor <- historical_rtn_subset %>%
      dplyr::select(-date) %>%
      stats::cor(use = "pairwise.complete.obs")
    
    rm(historical_rtn_subset)
    
  } else {
    # then exp_vol, exp_cor, and exp_cov must be provided.
    # Make sure names & elements are in order to avoid disaster
    exp_vol      <- exp_vol[names(exp_rtn)]
    exp_cor      <- exp_cor[names(exp_rtn), names(exp_rtn)]
  } 
  
  # create covariance matrix of returns
  exp_cov <- exp_cor * (as.matrix(exp_vol) %*% exp_vol)  
  
  calc_sharpe <- function(wt_vec){
    as.numeric(
      exp_rtn %*% as.matrix(wt_vec) - rfr
    ) / sqrt(
      as.numeric((wt_vec %*% exp_cov) %*% as.matrix(wt_vec))
    )
  }
  
  if(is.null(startoff)){
    # Step 1: Find the highest-Sharpe, EQUALLY-WEIGHTED portfolio that can be 
    # created by selecting from the assets provided.
    
    if(!silent){
      start_time <- Sys.time()
      usethis::ui_todo(
        paste0("Starting BEWP calculation: ", crayon::bold(start_time))
      )      
    }
    
    bewp <- best_equal_weighted_portfolio(exp_rtn, exp_cov, rfr) 
    
    if(!silent){
      start_time <- Sys.time()
      usethis::ui_todo(
        paste0("Starting BEWP calculation: ", crayon::bold(start_time))
      )      
      end_time <- Sys.time()
      usethis::ui_done(crayon::bold("BEWP calculation complete."))
      (end_time - start_time) %>% 
        round(3) %>% 
        paste0(crayon::bold("Run time: "), ., " ", attr(., "units")) %>%
        usethis::ui_info()
    }
    
    startoff <- bewp
    
  } else {
    # Make sure startoff names & elements are in order to avoid disaster
    if(!any(names(startoff) == "weights")){
      startoff <- list(
        "weights" = startoff, 
        "sharpe" = calc_sharpe(startoff)
      )
    }
    
    startoff$weights <- names(exp_rtn) %>%
      setdiff(names(startoff$weights)) %>% {
        stats::setNames(rep(0, length(.)), .)
      } %>% 
      c(startoff$weights) %>% {
        .[names(exp_rtn)]
      } %>%
      round(precision + 1)
  }
  
  # Step 2: Refine the rough portfolio found in step 1.
  
  if(!silent){
    start_time <- Sys.time()
    usethis::ui_todo(
      paste0("Starting MP calculation: ", crayon::bold(start_time))
    )
  }
  
  mp <- startoff
  rm(startoff)
  
  # make `buy_sell_matrix`: a matrix whose row names are all the assets that
  #   appear in the inputs, and whose column names are all the assets whose 
  #   `mp$weights` are >= `stp`. The values of `buy_sell_matrix` are 
  #   the Sharpe ratios that result if you start with `mp$weights` and 
  #   SELL `stp` worth of the asset given buy the column index, and BUY 
  #   `stp` worth of the asset in the row index.
  # Obviously buying and selling the same asset is not useful -- these cells 
  #   are given the value -999 in `buy_sell_matrix`.
  
  # Step through the assets whose portfolio weights are >= to the amount
  #   we'll be adding/subtracting (otherwise they'll get 5negative weights)
  
  # Initialize loop vars
  
  steps    <- 1/10^(1:(precision + 1))
  stp_flag <- TRUE
  stp      <- steps[which((max(mp$weights) / steps) > 1)[1]]
  
  usethis::ui_info(paste0("starting step: ", crayon::bold(stp)))
  
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  while(TRUE){
    
    buy_sell_matrix <- matrix(
      nrow     = length(mp$weights),
      ncol     = length(mp$weights),
      dimnames = list(
        "take_from" = names(mp$weights), 
        "add_to" = names(mp$weights)
      )
    ) %>%
      bigstatsr::as_FBM()
    
    start_time_2 <- Sys.time()
    
    res <- foreach(take_from = which(mp$weights >= stp)) %dopar% {
      wts <- mp$weights
      wts[take_from] <- wts[take_from] - stp
      vapply(
        names(wts),
        function(add_to){
          wts[add_to]    <- wts[add_to] + stp
          calc_sharpe(wts)      
        },
        numeric(1)
      )
    } %>%
      purrr::reduce(rbind) %>%
      magrittr::set_rownames(names(mp$weights[which(mp$weights >= stp)]))
    
    calc_time <- signif(Sys.time() - start_time_2, 3) 
    
    buy_sell_matrix <- res
    
    best_sharpe <- max(buy_sell_matrix)
    
    # If we got a better Sharpe ratio in `buy_sell_matrix`, keep it & update!
    if((best_sharpe - mp$sharpe) > steps[length(steps)]){
      
      take_from_asset <- rownames(buy_sell_matrix)[
        which(buy_sell_matrix == best_sharpe, arr.ind = TRUE)[1]
      ]
      
      add_to_asset    <- colnames(buy_sell_matrix)[
        which(buy_sell_matrix == best_sharpe, arr.ind = TRUE)[2]
      ]
      
      mp$weights[add_to_asset]    <- mp$weights[add_to_asset]    + stp
      mp$weights[take_from_asset] <- mp$weights[take_from_asset] - stp
      
      mp$sharpe <- calc_sharpe(mp$weights)
      
      if(!silent){
        usethis::ui_info(
          paste0(
            "New Sharpe: ", 
            crayon::bold(mp$sharpe),
            "; calc time: ",
            calc_time, 
            " ", 
            attr(calc_time, "units")
          )
        )  
      }
      
    } else {
      
      # new_step <- steps[which((max(mp$weights) / steps) > 1)[1]]
      # 
      # if(new_step > stp & stp_flag){
      #   stp      <- new_step
      #   stp_flag <- FALSE
      # } else {
      #   stp <- stp / 2
      # }
      
      stp <- stp / 2
      
      if(!silent){
        usethis::ui_line()
        cli::cli_rule()
        usethis::ui_info(
          paste0(
            crayon::bold("No better Sharpe"), 
            "; best Sharpe: ",
            best_sharpe
          )
        )
        usethis::ui_info(
          paste0(
            "Tried ", 
            crayon::bold(nrow(buy_sell_matrix)*ncol(buy_sell_matrix)), 
            " combinations."
          )
        )
        usethis::ui_info(paste0("New step: ", crayon::bold(stp)))  
        cli::cli_rule()
        usethis::ui_line()
      }
      
      if(stp <= steps[length(steps)]){
        break()
      }
      
    }
    
  }
  
  end_time <- Sys.time()
  usethis::ui_done(crayon::bold("MP calculation complete."))
  (end_time - start_time) %>% 
    round(3) %>% 
    paste0(crayon::bold("Run time: "), ., " ", attr(., "units")) %>%
    usethis::ui_info()
  
  # Step 3: Clean up mp object
  mp$weights    <- mp$weights %>% {
    .[which(round(-log(., 10)) < (precision - 1))]
  } %>% {
    . * sum(.) / sum(.)
  } %>% 
    round(precision)
  mp$sharpe     <- names(exp_rtn) %>%
    setdiff(names(mp$weights)) %>% {
      stats::setNames(rep(0, length(.)), .)
    } %>% 
    c(mp$weights) %>% {
      .[names(exp_rtn)]
    } %>%
    calc_sharpe() %>%
    round(precision)
  mp$universe   <- names(exp_rtn)
  mp$target_rtn <- exp_rtn[names(mp$weights)]
  mp$target_vol <- exp_vol[names(mp$weights)]
  mp$exp_rtn    <- exp_rtn
  mp$exp_vol    <- exp_vol
  mp <- mp[
    c(
      "universe", "target_rtn", "target_vol", "weights", "exp_rtn", "exp_vol", 
      "sharpe"
    )
  ]
  
  mp
}
