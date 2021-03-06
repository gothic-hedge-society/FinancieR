% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blotter.R
\name{blotter}
\alias{blotter}
\alias{is_blotter}
\title{Trade Blotter}
\usage{
is_blotter(obj)
}
\arguments{
\item{obj}{An R object}
}
\value{
The function \code{is_blotter}() returns TRUE if passed an R object that
meets# the criteria to be treated as a blotter.
}
\description{
A \strong{blotter} is an xts time-series object that contains a complete record of
all trades made during a given time interval. To be a blotter, an xts object
must have four columns: \emph{symbol}, \emph{action}, \emph{price}, and \emph{fees}, and must
meet the following criteria:
\itemize{
\item{\strong{rows}}: Each row represents a completed (filled) trade
order for a single asset.
\item{\strong{index}}: The datetime index gives the timestamp (or date)
on which the trade was filled.
\item{\strong{symbol}}: Identifier for the asset traded. Can be a ticker,
CUSIP, ISIN, a unique ID assigned by a brokerage, etc.
\item{\strong{price}}: Always positive. The asset's price for the trade.
\item{\strong{fees}}: Sum of all fees paid for the trade. Includes
commision, transaction fees, etc. Does not include taxes.
}

Use the function \code{is_blotter}() to determine if an object is a blotter.
}
\details{
\strong{Options exercises} do not appear explicitly on the blotter. However, the
purchase/sale of the option, and the purchase/sale of the underlying asset
due to an exercise, do.

\strong{Shorting} also does not appear explicitly on the blotter -- it appears
only as a sale.

\strong{Dividends, coupon payments, and other cash} do not appear on the blotter.
Trades only.
}
