# Load required libraries
library(tidyverse)
library(tidyquant)
library(h2o)
library(lubridate)
library(tm)
library(TTR)  # For technical indicators
library(zoo)  # For rolling functions

# Initialize H2O
h2o.init()
h2o.no_progress()  # Disable progress bars

# Function to safely read and preprocess data
safe_read_data <- function(file_path) {
  tryCatch({
    # Read the CSV file
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    # Ensure required columns exist
    required_columns <- c("date.filed", "cik", "company.name", "event.info")
    if (!all(required_columns %in% names(data))) {
      stop("Missing required columns in the dataset")
    }
    # Convert dates from any format to proper Date format
    data <- data %>%
      mutate(
        date.filed = parse_date_time(date.filed, orders = c("dmy", "ymd", "mdy")),
        date.filed = as.Date(date.filed),
        cik = as.character(cik)
      )
    print("Date range in data:")
    print(range(data$date.filed))
    return(data)
  }, error = function(e) {
    message("Error reading data: ", e$message)
    return(NULL)
  })
}

# Symbol mapping dataframe
symbol_mapping <- data.frame(
  company.name = c(
    "PFIZER INC", "MICROSOFT CORP", "APPLE INC", "AMAZON COM INC", 
    "ALPHABET INC", "META PLATFORMS INC", "JOHNSON & JOHNSON", 
    "Merck & Co Inc", "AbbVie Inc", "Workday Inc", "Palantir Technologies Inc", 
    "Uber Technologies Inc", "DROPBOX INC", "Atlassian Corp", "Square Inc", 
    "Block Inc", "ZoomInfo Technologies Inc", "Zscaler Inc", "Snowflake Inc", 
    "Okta Inc", "INTERNATIONAL BUSINESS MACHINES CORP", "QUALCOMM INC DE", 
    "ADOBE SYSTEMS INC", "ADOBE INC", "PayPal Holdings Inc", "Zoom Video Communications Inc", 
    "MongoDB Inc", "TESLA MOTORS INC", "Tesla Inc", "NVIDIA CORP", "ORACLE CORP", 
    "INTEL CORP", "ServiceNow Inc", "Google Inc", "Broadcom Inc", 
    "Wright Express CORP", "A10 Networks Inc", "NETFLIX INC", "Synchrony Financial", 
    "LyondellBasell Industries N V", "Mellanox Technologies Ltd", 
    "TWITTER INC", "BROADRIDGE FINANCIAL SOLUTIONS INC", "Groupon Inc", 
    "J P MORGAN CHASE & CO", "CITIGROUP INC", "WELLS FARGO & CO MN", 
    "GOLDMAN SACHS GROUP INC", "MORGAN STANLEY", "AMERICAN EXPRESS CO", 
    "SOUTHWEST AIRLINES CO", "BlackRock Inc", "BlackRock Finance Inc", 
    "CENTERPOINT ENERGY INC", "Zoetis Inc"
  ), 
  symbol = c(
    "PFE", "MSFT", "AAPL", "AMZN", "GOOGL", "META", "JNJ", "MRK", 
    "ABBV", "WDAY", "PLTR", "UBER", "DBX", "TEAM", "SQ", "SQ", 
    "ZI", "ZS", "SNOW", "OKTA", "IBM", "QCOM", "ADBE", "ADBE", 
    "PYPL", "ZM", "MDB", "TSLA", "TSLA", "NVDA", "ORCL", "INTC", 
    "NOW", "GOOGL", "AVGO", "WEX", "ATEN", "NFLX", "SYF", 
    "LYB", "MLNX", "TWTR", "BR", "GRPN", "JPM", "C", "WFC", 
    "GS", "MS", "AXP", "LUV", "BLK", "BGSF", "CNP", "ZTS"
  )
)



# Function to preprocess text
preprocess_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  return(text)
}

# Optimized function to fetch stock data with retries
fetch_stock_data <- function(symbols, start_date, end_date) {
  stock_data <- tq_get(symbols, 
                       from = start_date, 
                       to = end_date,
                       get = "stock.prices")
  if (nrow(stock_data) == 0) {
    warning("No stock data retrieved")
    return(NULL)
  }
  stock_data <- stock_data %>%
    mutate(date = as.Date(date))
  return(stock_data)
}

# Function to calculate technical indicators
calculate_technical_indicators <- function(data) {
  data %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(
      # Price-based features
      price_range = high - low,
      price_change = close - open,
      price_range_pct = (high - low) / open,
      price_change_pct = (close - open) / open,
      # Volume-based features
      volume_price_ratio = volume / close,
      # Rolling metrics (5-day window)
      roll_mean_close = rollmean(close, k = 5, fill = NA, align = "right"),
      roll_std_close = rollapply(close, width = 5, FUN = sd, fill = NA),
      roll_mean_volume = rollmean(volume, k = 5, fill = NA, align = "right"),
      # Technical indicators
      rsi_14 = RSI(close, n = 14),
      sma_20 = SMA(close, n = 20),
      ema_9 = EMA(close, n = 9),
      macd = MACD(close, nFast = 12, nSlow = 26, nSig = 9)[,'macd'],
      macd_signal = MACD(close, nFast = 12, nSlow = 26, nSig = 9)[,'signal'],
      bb_up = BBands(close, n = 20)[,'up'],
      bb_down = BBands(close, n = 20)[,'dn']
    ) %>%
    ungroup()
}

# Main processing function
process_stock_data <- function(file_path) {
  # Read data
  data <- safe_read_data(file_path)
  if (is.null(data)) {
    stop("Failed to load data")
  }
  # Join with symbol mapping
  data <- data %>%
    left_join(symbol_mapping, by = "company.name") %>%
    filter(!is.na(symbol)) %>%
    arrange(date.filed)
  # Calculate date range
  start_date <- min(data$date.filed, na.rm = TRUE) - days(30)  # Extra 30 days for indicators
  end_date <- max(data$date.filed, na.rm = TRUE)
  print(paste("Processing data from", start_date, "to", end_date))
  # Fetch stock data for all symbols
  stock_data <- fetch_stock_data(unique(data$symbol), start_date, end_date)
  if (is.null(stock_data)) {
    stop("No stock data was retrieved")
  }
  # Calculate technical indicators
  stock_data <- calculate_technical_indicators(stock_data)
  # Join stock data with filing data
  joined_data <- data %>%
    select(cik, company.name, symbol, date.filed, event.info) %>%
    inner_join(
      stock_data,
      by = c("symbol" = "symbol", "date.filed" = "date")
    )
  # Preprocess text data
  text_data <- sapply(joined_data$event.info, preprocess_text)
  doc_matrix <- DocumentTermMatrix(Corpus(VectorSource(text_data)))
  text_features <- as.matrix(doc_matrix)
  # Remove rare terms and scale text features
  text_features <- text_features[, colSums(text_features) > 5]
  text_features <- scale(text_features)
  # Prepare market features
  market_features <- joined_data %>%
    select(-cik, -company.name, -symbol, -date.filed, -event.info) %>%
    mutate(across(everything(), scale)) %>%
    as.matrix()
  # Handle NA and Inf values
  market_features[is.na(market_features)] <- 0
  market_features[is.infinite(market_features)] <- 0
  text_features[is.na(text_features)] <- 0
  # Combine features
  features <- cbind(market_features, text_features)
  # Create target variable with multiple classes
  target_data <- joined_data %>%
    group_by(symbol) %>%
    arrange(date.filed) %>%
    mutate(
      next_day_return = lead(close) / close - 1,
      target = case_when(
        next_day_return > 0.02 ~ 2,  # Strong positive
        next_day_return > 0 ~ 1,     # Positive
        next_day_return < -0.02 ~ 0, # Strong negative
        TRUE ~ 0                     # Negative
      )
    ) %>%
    ungroup()
  # Remove rows with NA in target
  valid_indices <- !is.na(target_data$target)
  features <- features[valid_indices, ]
  target <- target_data$target[valid_indices]
  # Convert to H2O frames
  h2o_features <- as.h2o(features)
  h2o_target <- as.h2o(as.factor(target))
  h2o_data <- h2o.cbind(h2o_features, h2o_target)
  # Split the data
  splits <- h2o.splitFrame(h2o_data, ratios = c(0.7, 0.15), seed = 1234)
  train <- splits[[1]]
  valid <- splits[[2]]
  test <- splits[[3]]
  model <- h2o.deeplearning(
    x = 1:(ncol(features)),
    y = ncol(h2o_data),
    training_frame = train,
    validation_frame = valid,
    hidden = c(200, 100, 50),
    epochs = 200,
    activation = "RectifierWithDropout",  # Changed to RectifierWithDropout
    l1 = 1e-5,
    l2 = 1e-5,
    input_dropout_ratio = 0.2,
    hidden_dropout_ratios = c(0.3, 0.3, 0.3),
    balance_classes = TRUE,
    standardize = TRUE,
    momentum_start = 0.5,
    momentum_ramp = 1e6,
    momentum_stable = 0.99,
    nesterov_accelerated_gradient = TRUE,
    stopping_metric = "logloss",
    stopping_rounds = 5,
    stopping_tolerance = 1e-3,
    seed = 1234,
    model_id = "Deep_Learning_Model_8k_Filings.h2o"
  )
  model_path <- h2o.saveModel(model, path = getwd(), force = TRUE)
  cat("Model saved to:",model_path)
  # Return results
  list(
    model = model,
    performance = h2o.performance(model, test),
    predictions = h2o.predict(model, test),
    test_data = as.data.frame(test),
    feature_importance = h2o.varimp(model)
  )
 }

# Run the entire process with error handling
tryCatch({
  print("Starting processing...")
  results <- process_stock_data("final_combined_8K_filings_all.csv")
  # Print model performance
  print("Model Performance:")
  print(results$performance)
  # Get predictions and actual values
  predictions <- as.vector(results$predictions$predict)
  actuals <- as.vector(results$test_data[, ncol(results$test_data)])
  # Ensure they have the same length
  if(length(predictions) == length(actuals)) {
    # Create confusion matrix
    conf_matrix <- table(
      Actual = actuals,
      Predicted = predictions
    )
    print("Confusion Matrix:")
    print(conf_matrix)
    # Calculate accuracy
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
  } else {
    print("Error: Mismatch in predictions and actuals dimensions")
    print(paste("Predictions length:", length(predictions)))
    print(paste("Actuals length:", length(actuals)))
  }
}, error = function(e) {
}, finally = {
  # Ensure H2O is shut down even if there's an error
  try(h2o.shutdown(prompt = FALSE), silent = TRUE)
})