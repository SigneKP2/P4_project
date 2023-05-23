
  library(quantmod)

  # Get the symbols for each company in the S&P 500
  # symbols <- read.csv("Downloads\\tickers.csv")[,"Symbol"]
  symbols <- read.csv("C:....tickers.csv")[,"symbol"]

  # Set start and end dates
  start_date <- as.Date("2016-01-04")
  end_date <- as.Date("2021-01-04") 

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
      returns <- yearlyReturn(prices)

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

  # head(all_data)

  # Combine all the returns data into a single data frame
  all_returns <- do.call(cbind, all_data)

  # View the resulting data frame
  #head(all_returns)

  # Write the all_data list and all_returns data frame to CSV files
  write.csv(all_returns, "C:...sp500_all_data.csv", row.names = index(all_returns))