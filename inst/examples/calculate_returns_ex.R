# Calculate the daily natural log returns of Apple (AAPL) and AT&T (T) for 2014,
# a year in which Apple had a major stock split. 
aapl_att_returns <- calculate_returns(
  assets         = stock_data[c("AAPL", "T")],
  date_range_xts = "2014"
)

# Print the first 10 rows:
head(aapl_att_returns, 10)

# Let's check this result in a few key places. 

# 1) Normal trading day
#      Because we didn't specify otherwise, calculate_returns() will use its
#      default behavior and will calculate period-over-period natural-log
#      returns using Close prices. Let's pick Monday, 21 July 2014, which was a
#      normal trading day (no dividend, no split) for AT&T. The previous trading
#      day was Friday, 18 Jul 2014:
stock_data$T$prices["2014-07-18/2014-07-21"]

#      The natural log return using Close prices (calculate_returns() default)
#      as calculated by hand should be:
log(
  as.numeric(stock_data$T$prices$Close["2014-07-21"]) /
    as.numeric(stock_data$T$prices$Close["2014-07-18"])
)

#      This return was realized on 2014-07-21, so it should appear at that date
#      index in the results returned by calculate_returns():
aapl_att_returns["2014-07-21", "T"]
