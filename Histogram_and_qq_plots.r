library(quantmod)
library(moments)
library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries)
library(scales)
library(tidyverse)



###############################################################################################################################################################################################################
#THE MARKET PORTFOLIO

# Read tickers and market caps for the 466 assets from a CSV-file
symbols <- read.csv("C:....tickers.csv")[, "symbol"]
market_caps <- read.csv("C:....tickers.csv")[, "market_cap"]

#Assosiate the different market caps with the right tickers
market_cap <- setNames(market_caps, symbols[seq_along(market_caps)])

#Define the market weights
market_weights <- market_cap / (sum(market_caps))


#Find the daily weigted returns wrt the weights and take the sum of each row to find the total daily return for the market portfolio 
weighted_returns <- t(apply(returns_data, 1, function(row) row * market_weights))
market_daily_return <- rowSums(weighted_returns)

# Calculate market variance and mean
market_variance <- var(market_daily_return)
market_mean <- mean(market_daily_return)

#Histogram for the market portfolio
hist_for_market <- ggplot(as.data.frame(market_daily_return), aes(x = market_daily_return)) +
  geom_histogram(aes(y=..density..), bins=100, color="#10cea89c", fill="#2ac9cc") +
  stat_function(fun=dnorm, args=list(mean=market_mean, sd=sqrt(market_variance)), color="#000000", size=1) +
  labs(title="Market Portfolio Daily Returns") +
  theme_bw()
hist_for_market

#Save the plot hist_for_market
#ggsave(file = "C:...market_hist_plot", plot = hist_for_market, dpi = 1000)

#qq-plot for the market portfolio
qq_market <- ggplot(data = data.frame(market_daily_return), aes(sample = market_daily_return)) +
  stat_qq(color = "#2ac9cc") +
  stat_qq_line( size=1) +
  labs(title = "QQ-Plot for Market Portfolio Daily Returns", x = "Theoretical Quantiles", y = "Quantiles") +
  theme_bw()

#Save the plot qq_market
ggsave(file = "C:...QQ-plot_market.png", plot = qq_market, dpi = 1000)



###############################################################################################################################################################################################################
#THE SIX DIFFERENT ASSETS


#Read csv-file containing returns for the 466 assets
all_returns <- read.csv("C:...sp500_all_data.csv")


#Extract daily returns for six different assets
returns_AAPL <- grep("AAPL.returns", names(all_returns), value = TRUE)
returns_NFLX <- grep("NFLX.returns", names(all_returns), value = TRUE)
returns_PFG <- grep("PFG.returns", names(all_returns), value = TRUE)
returns_UNP <- grep("UNP.returns", names(all_returns), value = TRUE)
returns_PXD <- grep("PXD.returns", names(all_returns), value = TRUE)
returns_MU <- grep("MU.returns", names(all_returns), value = TRUE)
returns_data_AAPL <- all_returns[, returns_AAPL]
returns_data_NFLX <- all_returns[, returns_NFLX]
returns_data_PFG <- all_returns[, returns_PFG]
returns_data_UNP <- all_returns[, returns_UNP]
returns_data_PXD <- all_returns[, returns_PXD]
returns_data_MU <- all_returns[, returns_MU]

#Remove zeros from the returns_data
returns_data_AAPL <- returns_data_AAPL[returns_data_AAPL[,1] != 0,]
returns_data_NFLX <- returns_data_NFLX[returns_data_NFLX[,1] != 0,]
returns_data_PFG <- returns_data_PFG[returns_data_PFG[,1] != 0,]
returns_data_UNP <- returns_data_UNP[returns_data_UNP[,1] != 0,]
returns_data_PXD <- returns_data_PXD[returns_data_PXD[,1] != 0,]
returns_data_MU <- returns_data_MU[returns_data_MU[,1] != 0,]

#Histograms of the six different assets with a corresponding normal distributions
h_AAPL <- ggplot(data = data.frame(returns_data_AAPL), aes(x = returns_data_AAPL)) +
    geom_histogram(aes(y=..density..), binwidth = 0.000395, color = "#10cea89c", fill ="#2ac9cc") +
    labs(title = "AAPL Daily Returns", x = "Daily Returns in %",  y = "Density",) +
    stat_function(fun=dnorm, args=list(mean=mean(returns_data_AAPL), sd=sd(returns_data_AAPL)), color="#000000", size=1) +
    theme_bw()
h_NFLX <- ggplot(data = data.frame(returns_data_NFLX), aes(x = returns_data_NFLX)) +
    geom_histogram(aes(y=..density..), binwidth = 0.000395, color = "#10cea89c", fill ="#2ac9cc") +
    labs(title = "NFLX Daily Returns", x = "Daily Returns in %",  y = "Density",) +
    stat_function(fun=dnorm, args=list(mean=mean(returns_data_NFLX), sd=sd(returns_data_NFLX)), color="#000000", size=1) +
    theme_bw()
h_PFG <- ggplot(data = data.frame(returns_data_PFG), aes(x = returns_data_PFG)) +
    geom_histogram(aes(y=..density..), binwidth = 0.000395, color = "#10cea89c", fill ="#2ac9cc") +
    labs(title = "PFG Daily Returns", x = "Daily Returns in %",  y = "Density",) +
    #scale_x_continuous(limits = c(-0.1,0.1)) +
    stat_function(fun=dnorm, args=list(mean=mean(returns_data_PFG), sd=sd(returns_data_PFG)), color="#000000", size=1) +
    theme_bw()
h_UNP <- ggplot(data = data.frame(returns_data_UNP), aes(x = returns_data_UNP)) +
    geom_histogram(aes(y=..density..), binwidth = 0.000395, color = "#10cea89c", fill ="#2ac9cc") +
    labs(title = "UNP Daily Returns", x = "Daily Returns in %",  y = "Density",) +
    stat_function(fun=dnorm, args=list(mean=mean(returns_data_UNP), sd=sd(returns_data_UNP)), color="#000000", size=1) +
    #scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()
h_PXD <- ggplot(data = data.frame(returns_data_PXD), aes(x = returns_data_PXD)) +
    geom_histogram(aes(y=..density..), binwidth = 0.000395, color = "#10cea89c", fill ="#2ac9cc") +
    labs(title = "PXD Daily Returns", x = "Daily Returns in %",  y = "Density",) +
    stat_function(fun=dnorm, args=list(mean=mean(returns_data_PXD), sd=sd(returns_data_PXD)), color="#000000", size=1) +
    #scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()
h_MU <- ggplot(data = data.frame(returns_data_MU), aes(x = returns_data_MU)) +
    geom_histogram(aes(y=..density..), binwidth = 0.000395, color = "#10cea89c", fill ="#2ac9cc") +
    labs(title = "MU Daily Returns", x = "Daily Returns in %",  y = "Density",) +
    stat_function(fun=dnorm, args=list(mean=mean(returns_data_PFG), sd=sd(returns_data_PFG)), color="#000000", size=1) +
    #scale_x_continuous(limits = c(-0.1,0.1)) +
    theme_bw()

#Save the plot h_AAPL/h_NFLX/h_PFG/h_UNP/h_PXD/h_MU
ggsave(file = "C:...Histogram_of_daily_returns_for_6_asset_in_the_market_portfolio_density.png", plot = h_AAPL/h_NFLX/h_PFG/h_UNP/h_PXD/h_MU, dpi = 1000)


# Create QQ-plots for each return vector
qq_AAPL <- ggplot(data = data.frame(returns_data_AAPL), aes(sample = returns_data_AAPL)) +
  stat_qq(color = "#2ac9cc") +
  stat_qq_line(size=1) +
  labs(title = "QQ-Plot for AAPL Daily Returns", x = "Theoretical Quantiles", y = "Quantiles") +
  theme_bw()

qq_NFLX <- ggplot(data = data.frame(returns_data_NFLX), aes(sample = returns_data_NFLX)) +
  stat_qq(color = "#2ac9cc") +
  stat_qq_line(size=1) +
  labs(title = "QQ-Plot for NFLX Daily Returns", x = "Theoretical Quantiles", y = "Quantiles") +
  theme_bw()

qq_PFG <- ggplot(data = data.frame(returns_data_PFG), aes(sample = returns_data_PFG)) +
  stat_qq(color = "#2ac9cc") +
  stat_qq_line(size=1) +
  labs(title = "QQ-Plot for PFG Daily Returns", x = "Theoretical Quantiles", y = "Quantiles") +
  theme_bw()

qq_UNP <- ggplot(data = data.frame(returns_data_UNP), aes(sample = returns_data_UNP)) +
  stat_qq(color = "#2ac9cc") +
  stat_qq_line(size=1) +
  labs(title = "QQ-Plot for UNP Daily Returns", x = "Theoretical Quantiles", y = "Quantiles") +
  theme_bw()

qq_PXD <- ggplot(data = data.frame(returns_data_PXD), aes(sample = returns_data_PXD)) +
  stat_qq(color = "#2ac9cc") +
  stat_qq_line(size=1) +
  labs(title = "QQ-Plot for PXD Daily Returns", x = "Theoretical Quantiles", y = "Quantiles") +
  theme_bw()

qq_MU <- ggplot(data = data.frame(returns_data_MU), aes(sample = returns_data_MU)) +
  stat_qq(color = "#2ac9cc") +
  stat_qq_line( size=1) +
  labs(title = "QQ-Plot for MU Daily Returns", x = "Theoretical Quantiles", y = "Quantiles") +
  theme_bw()

#Save the plot qq_AAPL+qq_NFLX+qq_PFG+qq_UNP+qq_PXD+qq_MU
ggsave(file = "C:...QQ-plots_of_daily_returns_for_6_asset_in_the_market_portfolio.png", plot = qq_AAPL+qq_NFLX+qq_PFG+qq_UNP+qq_PXD+qq_MU, dpi = 1000)

###############################################################################################################################################################################################################