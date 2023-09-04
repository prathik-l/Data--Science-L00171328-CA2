data <- read.csv("diabetes.csv", header = TRUE, sep = ",")


# Load necessary libraries
library(tidyverse)

# Load the data
#data <- read.csv("your_file_path_here.csv")

# Fit a logistic regression model
model <- glm(Diabetes_binary ~ ., family = binomial(link = "logit"), data = data)

# Get the summary
summary(model)

# Predict on the dataset
predicted_probs <- predict(model, type = "response")
predictions <- ifelse(predicted_probs > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(data$Diabetes_binary, predictions)
print(conf_matrix)

# Visualize the model
# (This is just a basic visualization; for logistic regression, visualizations can vary based on the individual requirements)
coefficients <- as.data.frame(coef(model))
coefficients$Variable <- rownames(coefficients)
ggplot(coefficients, aes(x = Variable, y = `coef(model)`)) +
  geom_col() +
  coord_flip() +
  labs(title = "Coefficients from Logistic Regression Model")



cor_values <- cor(data, data$Diabetes_binary, method="pearson")
cor_values_sorted <- sort(cor_values, decreasing=TRUE)
top_features <- names(cor_values_sorted[1:10])

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data$BMI <- normalize(data$BMI)
data$Age <- normalize(data$Age)
data$Income <- normalize(data$Income)

data$Education <- as.numeric(as.factor(data$Education))
#install.packages("caTools")
#library(caTools)
set.seed(123)
split <- sample.split(data$Diabetes_binary, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

train_data$BMI <- normalize(train_data$BMI)
train_data$Age <- normalize(train_data$Age)
train_data$Income <- normalize(train_data$Income)

test_data$BMI <- normalize(test_data$BMI)
test_data$Age <- normalize(test_data$Age)
test_data$Income <- normalize(test_data$Income)

model <- glm(Diabetes_binary ~ ., family = binomial(link = "logit"), data = train_data)


predicted_probs <- predict(model, newdata=test_data, type = "response")
predictions <- ifelse(predicted_probs > 0.5, 1, 0)

conf_matrix <- table(test_data$Diabetes_binary, predictions)
print(conf_matrix)

data$BMI[is.na(data$BMI)] <- mean(data$BMI, na.rm = TRUE)

quantiles <- quantile(data$BMI, c(0.01, 0.99))
data$BMI[data$BMI < quantiles[1]] <- quantiles[1]
data$BMI[data$BMI > quantiles[2]] <- quantiles[2]

library(caret)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(data[, -which(names(data) == "target")], data$target, sizes=c(1:10), rfeControl=control)
predictors(results)

model_log <- glm(Diabetes_binary ~ ., family = binomial(link = "logit"), data = train_data)


# Load necessary libraries
library(tidyverse)

# Load the data
#data <- read.csv("diabetes.csv", header = TRUE, sep = ",")

# Convert categorical data to numeric using one-hot encoding
data <- data %>%
  mutate(Education = as.numeric(as.factor(Education)))

# Load necessary libraries for logistic regression
library(caret)

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(data$Diabetes_binary, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Fit a logistic regression model
model <- glm(Diabetes_binary ~ ., family = binomial(link = "logit"), data = train_data)

# Provide user input data for forecasting
user_input <- data.frame(
  HighBP = 1,
  HighChol = 0,
  CholCheck = 1,
  BMI = 28,
  Smoker = 1,
  Stroke = 0,
  HeartDiseaseorAttack = 0,
  PhysActivity = 1,
  Fruits = 1,
  Veggies = 0,
  HvyAlcoholConsump = 0,
  AnyHealthcare = 1,
  NoDocbcCost = 0,
  GenHlth = 3,
  MentHlth = 5,
  PhysHlth = 20,
  DiffWalk = 0,
  Sex = 1,
  Age = 40,
  Education = 3,
  Income = 50000
)

# Make predictions on user input data
user_predictions <- predict(model, newdata = user_input, type = "response")
user_forecast <- ifelse(user_predictions > 0.5, "Diabetes Positive", "Diabetes Negative")

# Print the forecast result
print(user_forecast)

# Load required library (if not already loaded)
# install.packages("dplyr")
library(dplyr)

# Generate example diabetes_data (replace this with your actual data)
diabetes_data <- data.frame(
  group = c("Control", "Treatment"),
  values = c(120, 140, 125, 150, 130, 160)  # Sample data for two groups
)

# Perform a t-test
result <- t.test(values ~ group, data = diabetes_data)

# Print the result including p-value
cat("P-value:", result$p.value, "\n")

# Interpret the result based on p-value and significance level
if (result$p.value < 0.05) {
  cat("Reject the null hypothesis (significant difference)\n")
} else {
  cat("Fail to reject the null hypothesis (no significant difference)\n")
}

# Load necessary libraries
library(tidyverse)
library(pROC)

# Load the data

# Fit a logistic regression model
model <- glm(Diabetes_binary ~ ., family = binomial(link = "logit"), data = data)

# Predicted probabilities
predicted_probs <- predict(model, type = "response")

# Calculate optimal probability cutoff
optimal_cutoff <- optimalCutoff(data$Diabetes_binary, predicted_probs)
print(paste("Optimal Cutoff:", optimal_cutoff))

# Calculate predicted class using optimal cutoff
predicted_class <- ifelse(predicted_probs > optimal_cutoff, 1, 0)

# Calculate McFadden's R-Squared
null_model <- glm(Diabetes_binary ~ 1, family = binomial(link = "logit"), data = data)
mcfaddens_r_squared <- 1 - (model$deviance / null_model$deviance)
print(paste("McFadden's R-Squared:", mcfaddens_r_squared))

# Rest of the code remains the same

# Checking for linearity
lm_linearity <- lm(Diabetes_binary ~ ., data = data)
plot(lm_linearity)

# Scatter plot
scatter_plot <- ggplot(data, aes(x = Age, y = BMI)) +
  geom_point() +
  labs(title = "Scatter Plot: Age vs. BMI")
print(scatter_plot)

# Pairs plot
pairs_plot <- pairs(data[, c("Age", "BMI", "HighBP", "HighChol", "Diabetes_binary")])
print(pairs_plot)

# Scatter smooth plot
scatter_smooth <- ggplot(data, aes(x = Age, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Smooth Plot: Age vs. BMI")
print(scatter_smooth)

# Correlation matrix
cor_matrix <- cor(data)
print(cor_matrix)

# Checking for outliers using boxplots
boxplot(data$Age, data$BMI, data$HighBP, data$HighChol, data$Diabetes_binary,
        main = "Boxplot of Age, BMI, HighBP, HighChol, Diabetes_binary",
        names = c("Age", "BMI", "HighBP", "HighChol", "Diabetes_binary"))

# Handling outliers
outliers <- boxplot(data$Age)$out
data <- data[!data$Age %in% outliers, ]

# Subset data
subset_data <- subset(data, BMI > 25 & HighBP == 1)

# Using opar, attach, detach, and par
opar <- par(no.readonly = TRUE)
attach(data)
par(mfrow = c(2, 2))
hist(Age, main = "Histogram of Age", xlab = "Age")
hist(BMI, main = "Histogram of BMI", xlab = "BMI")
plot(Age, BMI, main = "Age vs. BMI", xlab = "Age", ylab = "BMI")
plot(HighBP, Age, main = "HighBP vs. Age", xlab = "HighBP", ylab = "Age")
detach(data)
par(opar)
