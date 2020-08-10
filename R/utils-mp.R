
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

mp_pocket <- function(portfolio, rtn, vol, cov){
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
