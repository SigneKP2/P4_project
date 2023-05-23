library(quantmod)
library(ggplot2)

# Symbols and portfolio weights from CSVs
symbols <- read.csv("C....tickers (3).csv")[, "symbol"]
weights_10 <- read.csv("C....tickers (3).csv")[, "mean_variance_weights_test.0.1"]
weights_25 <- read.csv("C....tickers (3).csv")[, "mean_variance_weights_test.0.25"]
weights_50 <- read.csv("C....tickers (3).csv")[, "mean_variance_weights_test.0.50"]

# Set start and end dates
start_date <- as.Date("2021-01-04")
end_date <- as.Date("2023-01-04") 

all_data <- list() # create an empty list to store the data

# Get historical data for all companies
for (symbol in symbols) {
  # Get historical data
  data <- try(getSymbols(paste0(symbol), src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)) # use auto.assign = FALSE to prevent overwriting the data frame

  # Check if the download was successful
  if (inherits(data, "try-error")) {
    # If the download failed, skip to the next symbol
    cat("Download for", symbol, "failed. Skipping to next symbol.\n")
    next
  } else {
    # If the download was successful, print the symbol to indicate progress
    cat("Downloaded data for", symbol, "\n")
    
    # Extract the adjusted closing prices from the data
    prices <- Ad(data)
    
    # Calculate daily returns of closing prices
    returns <- dailyReturn(prices)

    expected <- colMeans(returns, na.rm = TRUE)
    
    var <- var(returns, na.rm = TRUE)

    # Add the returns data to the all_data list
    all_data[[symbol]] <- cbind (data, returns, expected, var)

      names(all_data[[symbol]])[ncol(data) + 1] <- paste(symbol, "returns")
      names(all_data[[symbol]])[ncol(data) + 2] <- paste(symbol, "expected_return")
      names(all_data[[symbol]])[ncol(data) + 3] <- paste(symbol, "variance_of_return")


    # Add a delay to avoid overloading Yahoo Finance
    # Sys.sleep(0.1)
  }
}

# Combine all the returns data into a single data frame
all_returns_2021_2023 <- do.call(cbind, all_data)

write.csv(all_returns_2021_2023, "C....all_returns_2021_2023_test.csv", row.names = index(all_returns_2021_2023))


return_cols <- grep("returns", names(all_returns_2021_2023), value = TRUE)
returns_data <- all_returns_2021_2023[, return_cols]

price_cols <- grep("Adjusted", names(all_returns_2021_2023), value = TRUE)
price_data <- all_returns_2021_2023[, price_cols]

write.csv(returns_data, "C....returns_data_test.csv", row.names = index(all_returns_2021_2023))

risk_free_rate <- (1+0.0145)^(1/252) - 1 #0.000052
risk_free_rate_yearly <- 0.0145
risk_free_rate_yearly_entire_period <- (1 + risk_free_rate_yearly) ^ 2 - 1

risk_free_rate_yearly_entire_period

# Calculate portfolio variance and mean for portfolio 1

weighted_returns_10 <- t(apply(returns_data, 1, function(row) row * weights_10))



start_prices <- as.vector(price_data[1, ])
end_prices <- as.vector(price_data[504, ])

percent_returns <- as.data.frame((end_prices - start_prices) / start_prices)

weighted_percent_returns_10 <- percent_returns * weights_10
weighted_percent_return_10 <- sum(weighted_percent_returns_10)
weighted_percent_return_10_with_risk_free <- weighted_percent_return_10 + risk_free_rate_yearly_entire_period * weight_on_risk_free_10

weighted_percent_return_10_with_risk_free

write.csv(weighted_returns_10, "C....weighted_returns_10.csv", row.names = index(all_returns_2021_2023))

assets_daily_return_10 <- rowSums(weighted_returns_10)

weight_on_risk_free_10 <- 1 - sum(weights_10)



portfolio_daily_return_10 <- assets_daily_return_10 + risk_free_rate * weight_on_risk_free_10
portfolio_daily_return_10_var <- var(portfolio_daily_return_10)
portfolio_yearly_return_10_var <- portfolio_daily_return_10_var * 252 
portfolio_yearly_return_10_std <- sqrt(portfolio_yearly_return_10_var)



assets_variance_10 <- var(assets_daily_return_10)
assets_mean_10 <- mean(assets_daily_return_10)

portfolio_mean_10 <- assets_mean_10 + risk_free_rate * weight_on_risk_free_10

yearly_return_10 <- (1 + portfolio_mean_10)^252 - 1


# Calculate portfolio variance and mean for portfolio 2

weight_on_risk_free_25 <- 1 - sum(weights_25)

weighted_returns_25 <- t(apply(returns_data, 1, function(row) row * weights_25))

assets_daily_return_25 <- rowSums(weighted_returns_25)

portfolio_daily_return_25 <- assets_daily_return_25 + risk_free_rate * weight_on_risk_free_25
portfolio_daily_return_25_var <- var(portfolio_daily_return_25)
portfolio_yearly_return_25_var <- portfolio_daily_return_25_var * 252 
portfolio_yearly_return_25_std <- sqrt(portfolio_yearly_return_25_var)


weighted_percent_returns_25 <- percent_returns * weights_25
weighted_percent_return_25 <- sum(weighted_percent_returns_25)
weighted_percent_return_25_with_risk_free <- weighted_percent_return_25 + risk_free_rate_yearly_entire_period * weight_on_risk_free_25

weighted_percent_return_25_with_risk_free


assets_variance_25 <- var(assets_daily_return_25)
assets_mean_25 <- mean(assets_daily_return_25)

portfolio_mean_25 <- assets_mean_25 + risk_free_rate * weight_on_risk_free_25

yearly_return_25 <- (1 + portfolio_mean_25)^252 - 1

# Calculate portfolio variance and mean for portfolio 3

weight_on_risk_free_50 <- 1 - sum(weights_50)

weighted_returns_50 <- t(apply(returns_data, 1, function(row) row * weights_50))

assets_daily_return_50 <- rowSums(weighted_returns_50)

weighted_percent_returns_50 <- percent_returns * weights_50
weighted_percent_return_50 <- sum(weighted_percent_returns_50)
weighted_percent_return_50_with_risk_free <- weighted_percent_return_50 + risk_free_rate_yearly_entire_period * weight_on_risk_free_50

weighted_percent_return_50_with_risk_free

portfolio_daily_return_50 <- assets_daily_return_50 + risk_free_rate * weight_on_risk_free_50
portfolio_daily_return_50_var <- var(portfolio_daily_return_50)
portfolio_yearly_return_50_var <- portfolio_daily_return_50_var * 252 
portfolio_yearly_return_50_std <- sqrt(portfolio_yearly_return_50_var)

assets_variance_50 <- var(assets_daily_return_50)
assets_mean_50 <- mean(assets_daily_return_50)

portfolio_mean_50 <- assets_mean_50 + risk_free_rate * weight_on_risk_free_50

yearly_return_50 <- (1 + portfolio_mean_50)^252 - 1

yearly_return_50

# Calculate Sharpe ratio for each symbol
sharpe_ratios <- vector("numeric", length(symbols))
names(sharpe_ratios) <- symbols

for (i in 1:length(symbols)) {
  symbol_returns <- returns_data[, i]
  
  # Calculate excess returns (returns above the risk-free rate)
  excess_returns <- symbol_returns - risk_free_rate
  
  # Calculate standard deviation of excess returns
  stdev_excess_returns <- sqrt(var(excess_returns))
  
  # Calculate Sharpe ratio using the formula
  sharpe_ratio <- mean(excess_returns) / stdev_excess_returns
  
  # Store Sharpe ratio in the sharpe_ratios vector
  sharpe_ratios[i] <- sharpe_ratio
}

write.csv(sharpe_ratios, "C....new_sharpe_ratios.csv")

# Calculate Sharpe ratio for each portfolio

# Calculate excess returns (returns above the risk-free rate)
excess_returns_10 <- portfolio_mean_10 - risk_free_rate
excess_returns_25 <- portfolio_mean_25 - risk_free_rate
excess_returns_50 <- portfolio_mean_50 - risk_free_rate

# Calculate standard deviation of excess returns
stdev_excess_returns_10 <- sqrt(var(excess_returns_10))
stdev_excess_returns_25 <- sqrt(var(excess_returns_25))
stdev_excess_returns_50 <- sqrt(var(excess_returns_50))

# Calculate Sharpe ratio using the formula
sharpe_ratio_10 <- mean(excess_returns) / stdev_excess_returns
sharpe_ratio_25 <- mean(excess_returns) / stdev_excess_returns
sharpe_ratio_50 <- mean(excess_returns) / stdev_excess_returns

portfolio_yearly_return_10_std
portfolio_yearly_return_25_std
portfolio_yearly_return_50_std
