# Calculate daily natural log returns of Apple (AAPL) and AT&T (T) for 2014, a
# year in which Apple had a major stock split.
aapl_att_returns <- calculate_historical_returns(
  assets         = stock_data[c("AAPL", "T")],
  date_range_xts = "2014"
)

# Print the first 10 rows:
head(aapl_att_returns, 10)

# Let's check this result in a few key places. 

# 1) Normal trading day
#      Because we didn't specify otherwise, calculate_historical_returns() will
#      use its default behavior and will calculate period-over-period
#      natural-log returns using Close prices. Let's pick Monday, 21 July 2014,
#      which was a normal trading day (no dividend, no split) for AT&T. The
#      previous trading day was Friday, 18 Jul 2014:
stock_data$T$prices["2014-07-18/2014-07-21"]

#      The natural log return using Close prices (calculate_historical_returns()
#      default) as calculated by hand should be:
log(
  as.numeric(stock_data$T$prices$Close["2014-07-21"]) /
    as.numeric(stock_data$T$prices$Close["2014-07-18"])
)

#      This return was realized on 2014-07-21, so it should appear at that date
#      index in the results returned by calculate_historical_returns():
aapl_att_returns["2014-07-21", "T"]

# 2) AT&T's dividend
#      AT&T paid several dividends in 2014:
stock_data$T$dividends["2014"]

#     If you had bought a share of AT&T stock on 07 July 2014 and sold it at the
#     Close price the next day after the dividend went ex-div, you would have
#     collected the closing price from the sale plus the dividend amount, 
#     meaning that your total earned return would have been:
log(
  (
    as.numeric(stock_data$T$prices$Close["2014-07-08"]) + 
      as.numeric(stock_data$T$dividends$DividendAmount["2014-07-08"])
  ) / as.numeric(stock_data$T$prices$Close["2014-07-07"])
)

#      This return was realized on 2014-07-08, so it should appear at that date
#      index in the results returned by calculate_historical_returns():
aapl_att_returns["2014-07-08", "T"]
# --> In this case, the stock closed at the same price as the previous day once
#     the dividend was taken into account, so return = 0.

# 3) AAPL's split
#      Apple underwent a major split on a Monday in June of 2014:
stock_data$AAPL$splits

#     If you had bought a share of AAPL at Close price on Friday, you would have
#     woken up on Monday morning with 7 shares... each one of which worth about
#     1/7th what they were worth previously. If you sold those shares at Close
#     price on Monday, your total return would be:
log(
  (
    as.numeric(stock_data$AAPL$prices$Close["2014-06-09"]) *
      as.numeric(stock_data$AAPL$splits$Denominator["2014-06-09"])
  ) / as.numeric(stock_data$AAPL$prices$Close["2014-06-06"])
)

#      This return was realized on 2014-06-09, so it should appear at that date
#      index in the results returned by calculate_historical_returns():
aapl_att_returns["2014-06-09", "AAPL"]
