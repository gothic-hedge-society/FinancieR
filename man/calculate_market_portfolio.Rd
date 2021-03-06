% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_market_portfolio.R
\name{calculate_market_portfolio}
\alias{calculate_market_portfolio}
\title{Calculate Market Portfolio}
\usage{
calculate_market_portfolio(exp_rtn, exp_vol, exp_cor, rfr = 2.7397e-05)
}
\arguments{
\item{exp_rtn}{Named numeric vector for which each element is the return
expected for the next period for the asset specified by the element's name.}

\item{exp_vol}{Named numeric vector for which each element is the volatility
expected for the asset specified by the element's name.}

\item{exp_cor}{Named numeric matrix specifying the expected covariance of
returns for each asset pair.}

\item{rfr}{The risk-free rate (in decimal form; i.e., to specify a rate of
"3\%" use "0.03", \emph{not} "3") that \strong{YOU} can earn on cash with
reasonable liquidity constraints that \strong{YOU} can tolerate. For big
banks and in economic textbooks, this rate is usually the current rate on
3-month T-bills (or even higher). That might be just fine for you if you
plan on rebalancing once a year. For a trader running a strategy that can't
tolerate cash being tied up for 3 months at a time, the \emph{rfr} should
be set to whatever interest rate your brokerage is giving you on cash in
your trading account. Used in calculation of the
\href{https://www.investopedia.com/articles/07/sharpe_ratio.asp}{Sharpe
Ratio}. Defaults to \strong{0.0027397}\% daily return (about 1\% annually).
If you specify a different value for \emph{rfr}, \strong{make sure its time
basis matches the one used for you other inputs}, (i.e., if \emph{exp_rtn}
contains monthly returns, use monthly risk-free rate)!}
}
\value{
A list describing the market portfolio (MP) having four elements:

\describe{
\item{\code{sharpe}, numeric:}{Sharpe Ratio of MP}
\item{\code{weights}, named numeric vector:}{The values of
\code{weights} range from 0 to 1 and denote the fraction of the total
MP value allocated to the asset whose identifier is that value's name.}
\item{\code{ex_return}, numeric:}{The return that is expected (i.e.,
predicted/forecast) for the MP over the next time interval.}
\item{\code{ex_volatility}:}{The volatility that is expected (i.e.,
predicted/forecast) for the MP over the next time interval.}
}
}
\description{
Calculate the Sharpe-optimal market portfolio available for a set of assets
given the expected returns, volatilities, and correlations of returns for
each asset.
}
\details{
All arguments which are percentages (\emph{exp_rtn}, \emph{exp_vol}, and
\emph{rfr}) must be supplied in decimal form; i.e., to specify "12\%", use
0.12, not 12.

It should go without saying, but make sure that every asset is represented
in the three inputs: names of \emph{exp_rtn}, \emph{exp_vol}, and the row &
colnames of \emph{exp_cor}.

This function works by finding the Sharpe-optimum equal-weighted portfolio
that can be created using the assets passed in. Using that portfolio as a
starting point, the function finds the assets A and B such that
reallocating ("selling") a small amount (\emph{step}) of A into asset B
("buying") results in a portfolio whose Sharpe is greater than all other
possible assets A and B. The portfolio variable is updated.

This process is repeated with smaller and smaller \emph{step} sizes. When
it is not possible to reallocate \emph{step} amount of any asset A into any
other asset B so as to create a portfolio having a better Sharpe, the
Market Portfolio has been reached and the function returns the value.

This function answers the question: "What is the the optimum fractional,
real-valued weight of each asset in the Sharpe-optimal MP?" Because the
Sharpe ratio and the optimum weights are real numbers, this operation can
be computed with arbitrary precision, to infinte decimal places.
\emph{calculate_market_portfolio}() was written to be clearly understood
and run reasonably fast, and testing indicates that the weights are
reliable to within +/- 1\% (i.e., "0.18" to "0.20" for a reported weight of
"0.19") of the optimal value. As such, weights reported by \emph{weights
mode} should be considered somewhat approximate.
}
\section{The Cost of Shorting}{

You would only short an asset if the return you expect for buying that asset
is negative. You may think that the expected return for shorting an asset is
simply the return you expect for longing the asset times -1. In reality, the
return you'll get from a short sale will be a fraction of the asset times -1
because your broker charges fees on short position in exchange for providing
the shorting service. In addition, the capital gains from short selling may
taxed at a different rate than those realized on your long positions.

The key additional costs of shorting are:

\describe{
\item{Dividends}{
You don't earn dividends on a short position. In fact, it's the
opposite: if you're short a stock on a dividend's ex-date, then you must
actually \emph{pay} the dividend to the owner of the stock.
\emph{\link{calculate_historical_returns}}() takes this into account
itself when shorting is included.
}
\item{Short Fees}{
Your brokerage will charge a fee on a short position for every day the
position is open. This fee is based on the availability of assets for
shorting, and varies from asset to asset and through time. Usually the
fee is calculated on each trading day and debited from the trading
account on at the beginning of each month.
}
\item{Taxes}{
Capital gains taxes on your short sales can often be taxed at a higher
rate than what you might expect for long sales. If \strong{uncovered},
the short position will always be taxed as a short-term capital gain
because the holding period is considered by the IRS to begin on the day
when the short position was closed out (bought to cover). If
\strong{covered}, then the holding period is considered to be the
holding period of the \emph{substantially different securities} that can
be converted to the stock itself. Taxes must be taken into account in
the \emph{exp_rtn} parameter as passed to
\emph{calculate_market_portfolio}.\preformatted{ Taxes will depend on your situation: whether you expect a short-term or
 long-term investment, your income bracket, whether or not you're an
 institution, etc. You must figure that out for yourself, and you might
 find that, once everything is taken into account, it's just not worth it
 to short assets in a lot of situations. Stay smart!
}

}
}
}

\examples{
# Let's pick an example date for which to create an MP:
mp_date <- "2018-01-09"
# Feel free to change this date to any other date for which you have data.

# Use calculate_historical_returns() to get the daily returns observed for the 365 days
# ending on mp_date:
historical_rtn <- calculate_historical_returns(
  assets         = stock_data,
  date_range_xts = paste0(
    as.Date(mp_date) - 365,
    "/",
    mp_date
  )
)

# Note that we get an error message about LIN: that's because PX merged Linde
# Plc during the time period we selected, and the system is alerting you that
# data for the newly merged company, which trades under "LIN", is not available.

# We'll assume that the return we expect over the next year is the annualized 
# GMMR of the daily rates of return in historical_rtn:
exp_rtn <- gmrr(historical_rtn)

# Assume that the volatilities we expect for the next year are the annualized
# daily vols we observed during the previous year:
exp_vol <- dplyr::summarize(
  tibble::as_tibble(historical_rtn),
  dplyr::across(dplyr::everything(), sd, na.rm = TRUE)
) \%>\%
  purrr::as_vector()

# Assume that the correlations of returns of each asset pair that we expect
# for the next year will be the same as the previous year:
exp_cor <- stats::cor(historical_rtn, use = "pairwise.complete.obs")

# Calculate the market portfolio:
mp_by_wt <- calculate_market_portfolio(exp_rtn, exp_vol, exp_cor)
mp_by_wt
}
