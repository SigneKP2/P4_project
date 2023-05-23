library(quantmod)
library(ggplot2)
library(zoo)
library(tseries)

# Symbols and market caps from CSV
symbols <- read.csv("C:....tickers.csv")[, "symbol"]
market_caps <- read.csv("C:....tickers.csv")[, "market_cap"]

# Start and end date of period
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2021-01-01")

# all_data <- list()

# # Historical data
# for (symbol in symbols) {
#     # Get historical data
#     data <- try(getSymbols(paste0(symbol), src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)) # use auto.assign = FALSE to prevent overwriting the data frame

#     # Check if the download was successful
#     if (inherits(data, "try-error")) {
#         # If the download failed, skip to the next symbol
#         cat("Download for", symbol, "failed. Skipping to next symbol.\n")
#         next
#     } else {
#         # If the download was successful, print the symbol to indicate progress
#         cat("Downloaded data for", symbol, "\n")

#         # Extract the adjusted closing prices from the data
#         prices <- Ad(data)

#         # Calculate daily returns of closing prices
#         returns <- dailyReturn(prices)

#         # Add the returns data to the all_data list
#         all_data[[symbol]] <- cbind(data, returns)

#         names(all_data[[symbol]])[ncol(data) + 1] <- paste(symbol, "returns")

#         # Add a delay to avoid overloading Yahoo Finance
#         # Sys.sleep(0.1)
#     }
# }

# Combine all the returns data into a single data frame
all_returns <- read.csv("C:....returns_2021.csv")

return_cols <- grep("returns", names(all_returns), value = TRUE)
returns_data <- all_returns[, return_cols]

# write.csv(returns_data, "C:....returns_new.csv")

expected_returns <- colMeans(returns_data)
cov_matrix <- cov(returns_data, use = "na.or.complete")
cov_matrix_inverse <- solve(cov_matrix)

write.csv(cov_matrix, "C:....cov_matrix_new.csv")

market_cap <- setNames(market_caps, symbols[seq_along(market_caps)])
market_weights <- market_cap / (sum(market_caps))
one <- rep(1, 466)
risk_free_return <- (1 + 0.0131)^(1 / 252) - 1

expected_return <- function(weights) {
    return(unlist(t(weights) %*% expected_returns + (1 - t(weights) %*% one) * risk_free_return))
}

variance <- function(weights) {
    return(unlist(t(weights) %*% cov_matrix %*% weights)[1])
}

# Market portfolio
market_expected_return <- expected_return(market_weights)
market_variance <- variance(market_weights)

# Mean-variance analysis
# Parameters
A <- (t(one) %*% cov_matrix_inverse %*% expected_returns)[1]
B <- (t(expected_returns) %*% cov_matrix_inverse %*% expected_returns)[1]
C <- (t(one) %*% cov_matrix_inverse %*% one)[1]
D <- B * C - A^2
g <- (B * cov_matrix_inverse %*% one - A * cov_matrix_inverse %*% expected_returns) / D
h <- (C * cov_matrix_inverse %*% expected_returns - A * cov_matrix_inverse %*% one) / D
K <- B - 2 * A * risk_free_return + C * risk_free_return^2

# Optimal weights given mu
mean_variance_weights <- function(mu) {
    g + h * ((1 + mu)^(1 / 252) - 1)
}

mean_variance_weights_risk_free <- function(mu) {
    ((1 + mu)^(1 / 252) - 1 - risk_free_return) / K *
        cov_matrix_inverse %*% (expected_returns - risk_free_return * one)
}

# Portfolio frontier
mus_PF <- sapply(seq(-0.5, 2, by = 0.001), function(mu) (1 + mu)^(1 / 252) - 1)

variance_PF <- sapply(mus_PF, function(mu) C / D * (mu - A / C)^2 + 1 / C)
variance_PF_star <- sapply(mus_PF, function(mu) (mu - risk_free_return)^2 / K)

ggplot(NULL, aes(x = sqrt(variance_PF), y = mus_PF)) +
    geom_point() +
    xlab("Portfolio Standard Deviation") +
    ylab("Expected Return") +
    ggtitle("Portfolio Frontiers")

ggplot() +
    geom_point(aes(x = sqrt(variance_PF), y = mus_PF)) +
    geom_point(aes(x = sqrt(variance_PF_star), y = mus_PF)) +
    xlab("Standard Deviation") +
    ylab("Expected Return") +
    ggtitle("Portfolio Frontiers") +
    coord_fixed(ratio = 1)
# scale_x_continuous(limits = c(0, 0.015)) +
# scale_y_continuous(limits = c(-0.005, 0.005))

sharpe_ratios <- vector("numeric", length(symbols))
names(sharpe_ratios) <- symbols

for (i in 1:length(symbols)) {
    symbol_returns <- returns_data[, i]
    excess_returns <- symbol_returns - risk_free_return
    stdev_excess_returns <- sqrt(var(excess_returns))
    sharpe_ratio <- mean(excess_returns) / stdev_excess_returns
    sharpe_ratios[i] <- sharpe_ratio
}

write.csv(sharpe_ratios, "C:....sharpe_ratios.csv")

# Daily expected return market portfolio
expected_return(market_weights)

# Yearly expected return market portfolio
(1 + expected_return(market_weights))^252 - 1

# Daily variance market portfolio
variance(market_weights)

# Yearly variance market portfolio = 0.03982562
variance(market_weights) * 252

# Daily expected return MVP = 0.000001385878
expected_return(mean_variance_weights(A / C))

# Yearly expected return MVP = 0.0003493
(1 + expected_return(mean_variance_weights(A / C)))^252 - 1

# Daily variance MVP = 0.00001599047
variance(mean_variance_weights(A / C))

# Yearly variance MVP = 0.0040296
sqrt(variance(mean_variance_weights(A / C)) * 252)

# Daily return tangent portfolio
expected_return(mean_variance_weights_risk_free(18.454))

# Yearly return tangent portfolio
(1 + expected_return(mean_variance_weights_risk_free(18.454)))^252 - 1

# Daily variance tangent portfolio
variance(mean_variance_weights_risk_free(18.454))

# Yearly variance tangent portfolio
variance(mean_variance_weights_risk_free(18.454)) * 252

# Yearly variance with mu = 0.05
variance(mean_variance_weights_risk_free(0.1)) * 252

# Yearly variance with mu = 0.1
variance(mean_variance_weights_risk_free(0.2)) * 252

# Yearly variance with mu = 0.15
variance(mean_variance_weights_risk_free(0.3)) * 252

ggplot() +
    geom_point(aes(x = sqrt(variance_PF_star) * sqrt(252), y = mus_PF)) +
    xlab("Portfolio Standard Deviation") +
    ylab("Expected Return") +
    ggtitle("Portfolio Frontiers") +
    theme_bw()

variance(mean_variance_weights_risk_free(0.1)) * 252

(1 + 0.0025)^252 - 1
sqrt(variance(mean_variance_weights_risk_free(0.876135)) * 252)

(1 + expected_return(market_weights))^252 - 1
sqrt(variance(market_weights) * 252)
variance(market_weights) * 252

expected_return(market_weights)
sqrt(variance(market_weights))
variance(market_weights)

sum(mean_variance_weights_risk_free(15))
sqrt(variance(mean_variance_weights_risk_free(15)) * 252)

(1 + (A * risk_free_return - B) / (C * risk_free_return - A))^252 - 1
sqrt(variance(mean_variance_weights_risk_free((A * risk_free_return - B) / (C * risk_free_return - A))) * 252)

sqrt(variance(mean_variance_weights_risk_free((1 + (A * risk_free_return - B) / (C * risk_free_return - A))^252 - 1)) * 252)

returns_data[1, 398]
mean_variance_weights_risk_free((1 + (A * risk_free_return - B) / (C * risk_free_return - A))^252 - 1)[399]

(1 + expected_return(market_weights))^252 - 1

sum(mean_variance_weights_risk_free((A * risk_free_return - B) / (C * risk_free_return - A)))


histo_m <- ggplot(data = data.frame(all_returns_vector), aes(x = all_returns_vector)) +
    geom_histogram(aes(y = ..count..), binwidth = 0.000395, color = "#10cea89c", fill = "#2ac9cc") +
    labs(title = "Daily Returns for all assets in the market portfolio", x = "Daily Returns in %", y = "Frequency", ) +
    stat_function(fun = dnorm, args = list(mean = mean(all_returns_vector), sd = sd(all_returns_vector)), color = "#000000", size = 1) +
    scale_x_continuous(limits = c(-0.1, 0.1)) +
    theme_bw()
histo_m

write.csv(mean_variance_weights_risk_free(18.454), "C:....weights_tanget_portfolio.csv")


# Histograms of the assets with corresponding normal distributions
h_AAPL <- ggplot(data = data.frame(returns_data_AAPL), aes(x = returns_data_AAPL)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.000395, color = "#10cea89c", fill = "#2ac9cc") +
    labs(title = "AAPL Daily Returns", x = "Daily Returns in %", y = "Density", ) +
    stat_function(fun = dnorm, args = list(mean = mean(returns_data_AAPL), sd = sd(returns_data_AAPL)), color = "#000000", size = 1) +
    # scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()
h_NFLX <- ggplot(data = data.frame(returns_data_NFLX), aes(x = returns_data_NFLX)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.000395, color = "#10cea89c", fill = "#2ac9cc") +
    labs(title = "NFLX Daily Returns", x = "Daily Returns in %", y = "Density", ) +
    stat_function(fun = dnorm, args = list(mean = mean(returns_data_NFLX), sd = sd(returns_data_NFLX)), color = "#000000", size = 1) +
    # scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()
h_PFG <- ggplot(data = data.frame(returns_data_PFG), aes(x = returns_data_PFG)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.000395, color = "#10cea89c", fill = "#2ac9cc") +
    labs(title = "PFG Daily Returns", x = "Daily Returns in %", y = "Density", ) +
    # scale_x_continuous(limits = c(-0.1,0.1)) +
    stat_function(fun = dnorm, args = list(mean = mean(returns_data_PFG), sd = sd(returns_data_PFG)), color = "#000000", size = 1) +
    theme_bw()
h_UNP <- ggplot(data = data.frame(returns_data_UNP), aes(x = returns_data_UNP)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.000395, color = "#10cea89c", fill = "#2ac9cc") +
    labs(title = "UNP Daily Returns", x = "Daily Returns in %", y = "Density", ) +
    stat_function(fun = dnorm, args = list(mean = mean(returns_data_UNP), sd = sd(returns_data_UNP)), color = "#000000", size = 1) +
    # scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()
h_PXD <- ggplot(data = data.frame(returns_data_PXD), aes(x = returns_data_PXD)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.000395, color = "#10cea89c", fill = "#2ac9cc") +
    labs(title = "PXD Daily Returns", x = "Daily Returns in %", y = "Density", ) +
    stat_function(fun = dnorm, args = list(mean = mean(returns_data_PXD), sd = sd(returns_data_PXD)), color = "#000000", size = 1) +
    # scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()
h_MU <- ggplot(data = data.frame(returns_data_MU), aes(x = returns_data_MU)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.000395, color = "#10cea89c", fill = "#2ac9cc") +
    labs(title = "MU Daily Returns", x = "Daily Returns in %", y = "Density", ) +
    stat_function(fun = dnorm, args = list(mean = mean(returns_data_PFG), sd = sd(returns_data_PFG)), color = "#000000", size = 1) +
    # scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()

sum(mean_variance_weights_risk_free((A * risk_free_return - B) / (C * risk_free_return - A)))
(A * risk_free_return - B) / (C * risk_free_return - A)

(1 + expected_return(mean_variance_weights_risk_free((1 + (A * risk_free_return - B) / (C * risk_free_return - A))^252 - 1)))^252 - 1

write.csv(mean_variance_weights_risk_free((1 + (A * risk_free_return - B) / (C * risk_free_return - A))^252 - 1), "C:....weights_tangent.csv")

sqrt(K)

variance(market_weights)
min(expected_returns)

(expected_return(market_weights) - risk_free_return) / sqrt(variance(market_weights))

daily_market_returns <- as.matrix(returns_data) %*% as.vector(market_weights)

jarque.bera.test(daily_market_returns)

(1 + expected_return(mean_variance_weights_risk_free((1 + (A * risk_free_return - B) / (C * risk_free_return - A))^252 - 1)))^252 - 1



# Calculate beta for each symbol
betas <- vector("numeric", length(symbols))
names(betas) <- symbols

for (i in 1:length(symbols)) {
    symbol_returns <- returns_data[, i]

    # Calculate covariance between symbol and market returns
    covariance <- cov(symbol_returns, daily_market_returns)

    # Calculate beta using the formula
    beta <- covariance / variance(market_weights)

    # Store beta in the betas vector
    betas[i] <- beta
}
(A * risk_free_return - B) / (C * risk_free_return - A)
