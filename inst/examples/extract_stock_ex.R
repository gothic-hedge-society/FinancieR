
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

# Mergers are handled as illustrated below for the PX-LIN merger in 2018.
# Shareholders of PX received 1 share of the new company, LIN, for every 1
# share of PX.
stock_data$PX["2018-10-29/2018-11-05", "Close"]

# Splits are handled as follows for Apple's 7-to-1 split of 2014:
stock_data$AAPL["2014-06-05/2014-06-11", c("Open", "Close")]

# Not specifying which columns you want can quickly lead to a lot of data, as in
# the below subset of IVV for the month of June 2019:
stock_data$IVV["2019-06"]

###### M&A Events: Mergers, Acquisitions, Spinoffs, Breakups, etc.
# Consider the M&A event that took place on 31 Oct 2018 in which Praxair (PX)
# merged with Linde AG (not included in dataset) to form Linde PLC, a new
# company traded under the symbol LIN.

# Extracting the newly stock will treat the stock as a brand new company whose
# "born on" date is the ex-date of the M&A event. Therefore, extracting LIN over
# a date range that spans the merger's ex-Date will return data for LIN only:
stock_data$LIN["2018-10-26/2018-11-05", "Close"]

# Extracting a stock that is no longer traded but was merged into or acquired by
# another company will output data for BOTH the acquired / merged stock and the
# acquiring stock / newly merged company:
stock_data$PX["2018-10-26/2018-11-05", c("Close", "Close")]
