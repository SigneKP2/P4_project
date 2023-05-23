library(quantmod)
library(ggplot2)

options(scipen = 999)


# Symbols and market caps from CSV
symbols <- read.csv("C:....tickers.csv")[, "symbol"]

symbols_in_portfolio <- read.csv("C:....tickers (3).csv")[, "symbol"]
weights_10 <- read.csv("C:....tickers (3).csv")[, "mean_variance_weights_test.0.1"]
weights_25 <- read.csv("C:....tickers (3).csv")[, "mean_variance_weights_test.0.25"]
weights_50 <- read.csv("C:....tickers (3).csv.csv")[, "mean_variance_weights_test.0.50"]
weights_tangent_portfolio <- read.csv("C:....weights_tangent (3).csv")[, "weights"]

market_caps <- read.csv("C:....tickers (2).csv")[, "market_cap"]

all_returns <- read.csv("C:....sp500_new_tickers.csv")


symbols_in_portfolio <- as.vector(symbols_in_portfolio)
symbols_in_portfolio [1]

portfolio_return_cols <- character(length(symbols_in_portfolio))

for (i in 1:length(symbols_in_portfolio)) {
portfolio_return_cols[i] <- grep(paste0(symbols_in_portfolio[i],".returns"), names(all_returns), value = TRUE)
}

portfolio_returns_data <- all_returns[, portfolio_return_cols]

return_cols <- grep("returns", names(all_returns), value = TRUE)
returns_data <- all_returns[, return_cols]

expected_returns <- colMeans(returns_data)

market_cap <- setNames(market_caps, symbols[seq_along(market_caps)])
market_weights <- market_cap / (sum(market_caps))
# market_weights <- read.csv("C:\\Users\\MikkelNørgaard\\Downloads\\weights_tangent (3).csv")[, "weights"] #if weight for tangent portfolio should be used for beta and alpha calculations

weighted_returns <- t(apply(returns_data, 1, function(row) row * market_weights))
market_daily_return <- rowSums(weighted_returns)

mean(market_daily_return)

(1+mean(market_daily_return))^252 - 1

weighted_returns_tangent_portfolio <- t(apply(returns_data, 1, function(row) row * weights_tangent_portfolio))
tangent_portfolio_daily_returns <- rowSums(weighted_returns_tangent_portfolio)

# Calculate market variance and mean
market_variance <- var(market_daily_return)
market_mean <- mean(market_daily_return)

# Calculate beta for each symbol
betas <- vector("numeric", length(symbols))
names(betas) <- symbols

for (i in 1:length(symbols)) {
  symbol_returns <- returns_data[, i]
  
  # Calculate covariance between symbol and market returns
  covariance <- cov(symbol_returns, market_daily_return)
  
  # Calculate beta using the formula
  beta <- covariance / market_variance
  
  # Store beta in the betas vector
  betas[i] <- beta
}
write.csv(betas, "C:....betas.csv")

risk_free_rate <- (1+0.0145)^(1/252) - 1 #0.000052

# Calculate portfolio variance and mean for portfolio 1

weighted_returns_10 <- t(apply(portfolio_returns_data, 1, function(row) row * weights_10))
assets_daily_return_10 <- rowSums(weighted_returns_10)
weight_on_risk_free_10 <- 1 - sum(weights_10)
portfolio_daily_return_10 <- assets_daily_return_10 + risk_free_rate * weight_on_risk_free_10

covariance <- cov(portfolio_daily_return_10, market_daily_return)
beta_10 <- covariance / market_variance

alpha_10 <- mean(portfolio_daily_return_10) - risk_free_rate - ( beta_10 * ( market_mean - risk_free_rate ) )

excess_returns <- portfolio_daily_return_10 - risk_free_rate
sharpe_ratio_10 <- mean(excess_returns) / sqrt(var(excess_returns))


beta_10
alpha_10
sharpe_ratio_10

# Calculate portfolio variance and mean for portfolio 2

weight_on_risk_free_25 <- 1 - sum(weights_25)
weighted_returns_25 <- t(apply(portfolio_returns_data, 1, function(row) row * weights_25))
assets_daily_return_25 <- rowSums(weighted_returns_25)
portfolio_daily_return_25 <- assets_daily_return_25 + risk_free_rate * weight_on_risk_free_25
portfolio_mean_25 <- mean(portfolio_daily_return_25)

covariance <- cov(portfolio_daily_return_25, market_daily_return)
beta_25 <- covariance / market_variance

alpha_25 <- mean(portfolio_daily_return_25) - risk_free_rate - ( beta_25 * ( market_mean - risk_free_rate ) )

excess_returns <- portfolio_daily_return_25 - risk_free_rate
sharpe_ratio_25 <- mean(excess_returns) / sqrt(var(excess_returns))

mean(excess_returns)
sqrt(var(excess_returns))

beta_25
alpha_25
sharpe_ratio_25

# Calculate portfolio variance and mean for portfolio 3

weight_on_risk_free_50 <- 1 - sum(weights_50)
weighted_returns_50 <- t(apply(portfolio_returns_data, 1, function(row) row * weights_50))
assets_daily_return_50 <- rowSums(weighted_returns_50)
portfolio_daily_return_50 <- assets_daily_return_50 + risk_free_rate * weight_on_risk_free_50
portfolio_mean_50 <- mean(portfolio_daily_return_50)

covariance <- cov(portfolio_daily_return_50, market_daily_return)
beta_50 <- covariance / market_variance
alpha_50 <- mean(portfolio_daily_return_50) - risk_free_rate - ( beta_50 * ( market_mean - risk_free_rate ) )

excess_returns <- portfolio_daily_return_50 - risk_free_rate
sharpe_ratio_50 <- mean(excess_returns) / sqrt(var(excess_returns))

mean(excess_returns)
sqrt(var(excess_returns))

beta_50
alpha_50
sharpe_ratio_50

market_daily_return <- rowSums(weighted_returns)

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

write.csv(sharpe_ratios, "C:....sharpe_ratios.csv")

# Calculate alpha for each symbol
alphas <- vector("numeric", length(symbols))
names(alphas) <- symbols

for (i in 1:length(symbols)) {
  symbol_returns <- returns_data[, i]
  
  # Calculate alpha using the formula
  alpha <- mean(symbol_returns) - risk_free_rate - ( betas[i] * ( market_mean - risk_free_rate ) )

  # Store alpha in the alphas vector
  alphas[i] <- alpha
}

write.csv(alphas, "C:....alphas.csv")

hist_for_market <- ggplot(as.data.frame(market_daily_return), aes(x = market_daily_return)) +
  geom_histogram(aes(y=..density..), bins=100, color="#10cea89c", fill="#2ac9cc") +
  stat_function(fun=dnorm, args=list(mean=market_mean, sd=sqrt(market_variance)), color="#000000", size=1) +
  labs(title="Histogram of Daily Returns") +
  theme_bw()

hist_for_market

ggsave(file = "C:....market_hist_plot_new_tickers.png", plot = hist_for_market, dpi = 1000)


excess_returns <- market_daily_return - risk_free_rate
sharpe_ratio_market <- mean(excess_returns) / sqrt(var(excess_returns))

alpha_10
alpha_25
alpha_50

sharpe_ratio_10
sharpe_ratio_25
sharpe_ratio_50
sharpe_ratio_market

mean(market_daily_return)
mean(assets_daily_return_50)

excess_returns <- tangent_portfolio_daily_returns - risk_free_rate
sharpe_ratio_tangent_portfolio <- mean(excess_returns) / sqrt(var(excess_returns))

sharpe_ratio_tangent_portfolio

all_returns <- read.csv("C....sp500_yearly_all_data_2021_2023.csv")

dim(all_returns)

price_cols <- grep("Adjusted", names(all_returns), value = TRUE)
price_data <- all_returns[, price_cols]

dim(price_data)

dim(price_data[1, ])
dim(price_data[504, ])

start_prices <- as.vector(price_data[1, ])
end_prices <- as.vector(price_data[504, ])
start_prices <- as.numeric(start_prices)
end_prices <- as.numeric(end_prices)

percent_returns <- as.data.frame((end_prices - start_prices) / start_prices)

market_caps <- read.csv("C:....tickers (2).csv")[, "market_cap"]
market_cap <- setNames(market_caps, symbols[seq_along(market_caps)])
market_weights <- market_cap / (sum(market_caps))

weighted_percent_returns_market <- percent_returns * market_weights
weighted_percent_return_market <- sum(weighted_percent_returns_market)

weighted_percent_return_market

write.csv(price_data, "C....price_data.csv", row.names = index(all_returns))
