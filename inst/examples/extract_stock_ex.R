
# If no columns are specified, all available columns are returned.
# Single date:
stock_data$IVV["2019-06-11"]

# Dividends are correctly applied, even when issued on a non-trading day. In
# 2012, for example, Texas Instruments (TXN) had scheduled a dividend for the
# 29th of October but the exchange was closed due to Hurricane Sandy. The
# dividend, then, should be applied to the next trading day (31 October). Here,
# we'll select just the closing price for a date range around that time.
# Remember, any dividends that occur within the date range are always included.
stock_data$TXN["2012-10-25/2012-11-05", "Close"]

# Splits are handled as follows for Apple's 7-to-1 split of 2014:
stock_data$AAPL["2014-06-05/2014-06-11", c("Open", "Close")]

# Not specifying which columns you want can quickly lead to a lot of data, as in
# the below subset of IVV for the month of June 2019:
stock_data$IVV["2019-06"]
