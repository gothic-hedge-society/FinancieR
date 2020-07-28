# Hurricane Sandy hit the Northeastern United States at the end of October in
# 2012, shutting down the stock exchange and most of New York on 29 October.
# Texas Instruments (TXN) had scheduled a dividend to go ex-div that day, but
# since the stock exchange wasn't open, there are no historical prices for
# TXN on 29 Oct 2012, when a seller of TXN would have received the dividend.
#
# xts_merge_align_next() handles this situation by correctly applying the
#' # dividend to the next trading day, which happened to be the 31st:
xts_merge_align_next(
  stock_data$TXN$prices$Close["2012-10-26/2012-11-02", drop = FALSE],
  stock_data$TXN$dividends$DividendAmount,
  base::sum,
  0
)
