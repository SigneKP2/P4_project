# Read tickers and market caps for the 466 assets from a CSV-file
symbols <- read.csv("C:...tickers.csv")[, "symbol"]
market_caps <- read.csv("C:...tickers.csv")[, "market_cap"]

# Make a vector with a length equal to 466
sharpe_ratios <- vector("numeric", length(symbols))

#Name the entries in the sharpe_ratios vector by the tickers
names(sharpe_ratios) <- symbols

#Define the risk_free_rate
risk_free_rate <- 0.000052

#Calculate the Sharp ratio for each asset
for (i in 1:length(symbols)) {
  #Find the return of each ticker
  symbol_returns <- returns_data[, i]
  
  # Calculate the excess returns (returns above the risk-free rate)
  excess_returns <- symbol_returns - risk_free_rate
  
  # Calculate rge standard deviation of excess returns
  stdev_excess_returns <- sqrt(var(excess_returns))
  
  # Calculate the Sharpe ratio
  sharpe_ratio <- mean(excess_returns) / stdev_excess_returns
  
  # Store the Sharpe ratio in the sharpe_ratios vector
  sharpe_ratios[i] <- sharpe_ratio
}

#Write a csv-file containing all the sharpe ratios
write.csv(sharpe_ratios, "C:\\Users\\signe\\Documents\\Uni\\4. semester\\P4\\Kode\\sharpe_ratios.csv")