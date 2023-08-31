> data <- read.csv("diabetes.csv", header = TRUE, sep = ",")
install.packages("forecast")
# Load necessary libraries
install.packages(caret)
install.packages(caret)

library(dplyr)
library(caret)
library(forecast)
# Load your dataset
#data <- read.csv("your_dataset.csv")  # Replace with your actual dataset file name

# Exploratory Data Analysis (EDA)
summary(data)
str(data)
head(data)

# Data Splitting
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
remaining_data <- data[-train_indices, ]

validation_indices <- sample(1:nrow(remaining_data), 0.5 * nrow(remaining_data))
validation_data <- remaining_data[validation_indices, ]
test_data <- remaining_data[-validation_indices, ]

# Model Building and Validation

# Binary Classification Model (Logistic Regression)
formula <- Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Age + Education + Income
classification_model <- train(formula, data = train_data, method = "glm", family = "binomial")

# Validate on the validation set
validation_predictions <- predict(classification_model, data = validation_data)
confusion_matrix <- confusionMatrix(validation_predictions, validation_data$Diabetes_binary)
print(confusion_matrix)

# Time-Series Forecasting Model (ARIMA)
time_series_data <- ts(train_data$Diabetes_binary, start = c(2010, 1), frequency = 12)
time_series_model <- auto.arima(time_series_data)

# Validate on the validation set
validation_forecast <- forecast(time_series_model, h = length(validation_data))
plot(validation_forecast)

# Model Evaluation on Test Data

# For classification model
test_predictions <- predict(classification_model, newdata = test_data)
confusion_matrix_test <- confusionMatrix(test_predictions, test_data$Diabetes_binary)
print(confusion_matrix_test)

# For time-series forecasting model
time_series_test <- ts(test_data$Diabetes_binary, start = c(2010, 1), frequency = 12)
test_forecast <- forecast(time_series_model, h = length(time_series_test))
plot(test_forecast)
install.packages(pROC)
library(pROC)

# Get predicted probabilities on validation data
validation_probs <- predict(classification_model, data = validation_data, type = "prob")[, "1"]

# Create an ROC curve
roc_curve <- roc(validation_data$Diabetes_binary, validation_probs)

# Find the threshold that maximizes the Youden's J statistic
best_threshold <- roc_curve$thresholds[which.max(roc_curve$youden)]

# Get predicted probabilities on test data
test_probs <- predict(classification_model, newdata = test_data, type = "prob")[, "1"]

# Make predictions using the tuned threshold
test_predictions <- ifelse(test_probs >= best_threshold, 1, 0)

# Create a confusion matrix
conf_matrix <- table(Actual = test_data$Diabetes_binary, Predicted = test_predictions)
conf_matrix
# ROC Curve
plot(roc_curve, print.thres = "best")

# Confusion Matrix Heatmap
library(ggplot2)
conf_matrix_df <- as.data.frame.matrix(conf_matrix)
ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 0.5) +
  theme_minimal()

# Calibration Plot
calibration <- calibration(test_data$Diabetes_binary, test_probs, method = "boot")
plot(calibration, las = 1)





# Load required libraries
library(forecast)

# Load your diabetes dataset (replace 'data' with your actual dataset)

# Prepare time-series data
time_series_data <- ts(data$Diabetes_binary, start = c(2010, 1), frequency = 12)

# Visualize time-series data
plot(time_series_data, main = "Diabetes Time Series Data", xlab = "Year", ylab = "Diabetes Likelihood")

# Decompose time-series data
decomposed <- decompose(time_series_data)
plot(decomposed)

# Check for stationarity (Dickey-Fuller test)
adf_result <- adf.test(time_series_data)
print(adf_result)

# Perform differencing if needed to make the series stationary
differenced_data <- diff(time_series_data)

# Fit ARIMA model
auto_arima_model <- auto.arima(time_series_data)
summary(auto_arima_model)

# Forecasting
forecast_horizon <- 12  # Example: Forecasting for 12 months ahead
forecasted_values <- forecast(auto_arima_model, h = forecast_horizon)

# Plot the forecasted values
plot(forecasted_values, main = "Diabetes Forecast", xlab = "Year", ylab = "Forecasted Diabetes Likelihood")
lines(time_series_data, col = "blue")

# Evaluate forecast
actual_values <- window(time_series_data, start = c(2017, 1))
forecast_values <- forecasted_values$mean

mae <- mean(abs(actual_values - forecast_values))
print(paste("Mean Absolute Error:", mae))

