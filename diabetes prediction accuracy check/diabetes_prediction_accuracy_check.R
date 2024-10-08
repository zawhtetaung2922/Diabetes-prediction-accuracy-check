# Read the CSV file into a data frame
Diabetes <- read.csv("Diabetes.csv")
View(Diabetes)
# aaaaaaaaaaaaaaaaaaaaaaaaaa
# Display the structure of the data frame
str(Diabetes)

# Display summary statistics
summary(Diabetes)

# Show the first few rows of the dataset
head(Diabetes)

# Show the last few rows of the dataset
tail(Diabetes)
# bbbbbbbbbbbbbbbbbbbbbbbbb
# Check for missing values
missing_values <- colSums(is.na(Diabetes))

# If there are missing values, handle them accordingly
# For example, you can impute missing values with mean or median
# For demonstration, let's impute missing values with mean for numeric columns
Diabetes_imputed <- Diabetes
Diabetes_imputed[, sapply(Diabetes_imputed, is.numeric)] <- lapply(Diabetes_imputed[, sapply(Diabetes_imputed, is.numeric)], function(x) {ifelse(is.na(x), mean(x, na.rm = TRUE), x)})

# Remove duplicate rows
Diabetes_unique <- unique(Diabetes)

# Columns to normalize
cols_to_normalize <- c('Pregnancies', 'Glucose', 'BloodPressure', 'SkinThickness', 'Insulin', 'BMI', 'DiabetesPedigreeFunction', 'Age')

# Z-score normalization
Diabetes[cols_to_normalize] <- scale(Diabetes[cols_to_normalize])
Diabetes_standardized <- Diabetes[cols_to_normalize]

# head the normalized dataset
head(Diabetes_standardized)

# # Perform one-hot encoding for the 'Outcome' variable
# Diabetes$Outcome <- factor(Diabetes$Outcome)
# Diabetes <- cbind(Diabetes, model.matrix(~Outcome - 1, data = Diabetes))
# Encode the "Outcome" attribute as numeric (0 or 1)
Diabetes$Outcome <- as.numeric(Diabetes$Outcome)

# Rejoin with Diabetes_standardized
Diabetes_combined <- cbind(Diabetes_standardized, Outcome = Diabetes$Outcome)
Diabetes_standardized <- Diabetes_combined
# Display the head of the combined dataset
head(Diabetes_standardized)


# cccccccccccccccccccccccccccccccccccc
# "What are the key factors contributing to the likelihood of developing diabetes in individuals?"

# ddddddddddddddddd
# Load the required library
library(ggplot2)
Diabetes_no_id <- Diabetes_standardized[, !names(Diabetes_standardized) %in% c("Id")]
# Melt the data frame into long format
library(reshape2)
Diabetes_melted <- melt(Diabetes_no_id)

# Create histograms for all variables in one plot
ggplot(data = Diabetes_melted, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Variables", x = "Value", y = "Frequency") +
  theme_minimal()

# Load the required library
library(GGally)

# Create a pair plot
pairs(Diabetes_no_id)
# Load the required library
library(ggplot2)

# Exclude the "Id" column from the dataset
Diabetes_no_id <- Diabetes_standardized[, !names(Diabetes_standardized) %in% c("Id")]

# Create scatter plots for "Outcome" against all other attributes
plots <- lapply(names(Diabetes_no_id)[-9], function(var_name) {
  # Create a scatter plot for each attribute against "Outcome"
  ggplot(data = Diabetes_no_id, aes_string(x = var_name, y = "Outcome")) +
    geom_point() +
    labs(x = var_name, y = "Outcome") +
    theme_minimal()
})

# Arrange the plots in a grid
library(gridExtra)
grid.arrange(grobs = plots, ncol = 3)  # Adjust the number of columns as needed
Diabetes_standardized
# Compute the correlation matrix
correlation_matrix <- cor(Diabetes_standardized)

install.packages("gplots")
# Load the required library for heatmap
library(gplots)

# Create a heatmap of the correlation matrix
heatmap.2(correlation_matrix, 
          trace = "none",         # don't show the grid lines
          col = colorRampPalette(c("blue", "white", "red"))(100),  # color scheme
          margins = c(10, 10),    # increase margins for row and column labels
          main = "Correlation Heatmap",  # title
          xlab = "Variables",     # x-axis label
          ylab = "Variables",     # y-axis label
          symm = TRUE)            # symmetrical plot

# Calculate Z-scores for all variables except "Id"
z_scores <- scale(Diabetes_standardized[, !names(Diabetes_standardized) %in% c("Id")])

# Set Z-score threshold
threshold <- 3

# Identify outliers based on Z-score threshold
outliers <- apply(abs(z_scores) > threshold, 2, any)

# Print variable names with outliers
variable_names <- names(Diabetes_standardized)[!names(Diabetes_standardized) %in% c("Id")]
outlier_variables <- variable_names[outliers]
print(outlier_variables)

# Print indices and values of outliers for each variable
for (var in outlier_variables) {
  outlier_indices <- which(abs(z_scores[, var]) > threshold)
  outlier_values <- Diabetes_standardized[outlier_indices, var]
  print(paste("Variable:", var))
  print(paste("Outlier indices:", outlier_indices))
  print(paste("Outlier values:", outlier_values))
}

# Load the required library
library(ggplot2)

# Exclude the "Id" column from the dataset
Diabetes_no_id <- Diabetes_standardized[, !names(Diabetes_standardized) %in% c("Id")]

# Calculate Z-scores for all variables except "Outcome"
z_scores <- scale(Diabetes_no_id)

# Set Z-score threshold
threshold <- 3

# Identify outliers based on Z-score threshold
outliers <- abs(z_scores) > threshold

# Create boxplots with outliers marked
boxplot_outliers <- lapply(1:ncol(Diabetes_no_id), function(i) {
  # Get variable name
  var_name <- names(Diabetes_no_id)[i]
  
  # Get Z-scores and outliers for the variable
  z_scores_var <- z_scores[, i]
  outliers_var <- outliers[, i]
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Value = Diabetes_no_id[, i],
    Outlier = outliers_var
  )
  
  # Create a boxplot with outliers marked
  p <- ggplot(plot_data, aes(x = 1, y = Value)) +
    geom_boxplot() +
    geom_point(data = subset(plot_data, Outlier), aes(color = "red"), size = 3) +
    labs(x = "", y = var_name) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Add plot title
  if (i == 1) {
    p <- p + ggtitle("Boxplots with Outliers Marked")
  }
  
  return(p)
})

# Arrange the plots in a grid
library(gridExtra)
grid.arrange(grobs = boxplot_outliers, ncol = 3)  # Adjust the number of columns as needed

# # eeeeeeeeeeeeeeeeeeeeeeeeee
# Dependent variable: "Outcome" (presence or absence of diabetes)
# Independent variables: "Pregnancies," "Glucose," "BloodPressure," "SkinThickness," "Insulin," "BMI," "DiabetesPedigreeFunction," and "Age"

# ffffffffffffffffffff
# Load the required library
library(glmnet)
# Prepare the data
# Exclude the "Id" column and separate predictors (X) and outcome (y)
X <- Diabetes_standardized[, !names(Diabetes_standardized) %in% c("Id", "Outcome")]
y <- Diabetes_standardized$Outcome

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(Diabetes_standardized), 0.8 * nrow(Diabetes_standardized))
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Combine predictors (X_train) and outcome (y_train) into a data frame
train_data <- cbind(y_train, X_train)
colnames(train_data)[1] <- "Outcome"  # Rename the first column to "Outcome"
# gggggggggggggg
# Build a logistic regression model
model <- glm(Outcome ~ ., data = train_data, family = binomial)

# Make predictions on the test set
predictions <- predict(model, newdata = data.frame(X_test), type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predicted_classes - y_test)^2))
print(paste("RMSE:", rmse))

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(predicted_classes - y_test))
print(paste("MAE:", mae))

# Calculate Standard Deviation (SD) of the errors
sd_error <- sd(predicted_classes - y_test)
print(paste("SD of errors:", sd_error))
# Display summary of the regression model
summary(model)


# # Load the required library
# library(glmnet)
# library(caret)
# 
# # Load the diabetes dataset (assuming it's already preprocessed and split into X_train, X_test, y_train, y_test)
# 
# # Define a grid of hyperparameters to search over
# hyperparameters <- expand.grid(
#   alpha = 1,             # L1 regularization (Lasso)
#   lambda = 10^seq(-4, 2, by = 0.1)  # Sequence of regularization strengths
# )
# 
# # Perform grid search with cross-validation
# set.seed(123)  # For reproducibility
# cv <- train(
#   x = as.matrix(X_train), 
#   y = y_train, 
#   method = "glmnet", 
#   trControl = trainControl(method = "cv", number = 5),  # 5-fold cross-validation
#   tuneGrid = hyperparameters
# )
# 
# # Get the best model
# best_model <- cv$finalModel
# 
# # Make predictions on the test set
# predictions <- predict(best_model, newx = as.matrix(X_test), type = "response")
# 
# # Convert predicted probabilities to binary predictions
# predicted_classes <- ifelse(predictions > 0.5, 1, 0)
# 
# # Evaluate the model
# accuracy <- mean(predicted_classes == y_test)
# print(paste("Accuracy:", accuracy))
# 
# # Get the optimal hyperparameters
# optimal_alpha <- best_model$alpha
# optimal_lambda <- best_model$lambda
# 
# # Print the optimal hyperparameters
# print(paste("Optimal Alpha:", optimal_alpha))
# print(paste("Optimal Lambda:", optimal_lambda))
# iiiiiiiiiiiiiiiii
# Load the required library for calculating ROC AUC
library(pROC)

# Calculate accuracy
accuracy <- mean(predicted_classes == y_test)

# Calculate precision
precision <- sum(predicted_classes == 1 & y_test == 1) / sum(predicted_classes == 1)

# Calculate recall (sensitivity)
recall <- sum(predicted_classes == 1 & y_test == 1) / sum(y_test == 1)

# Calculate F1 score
f1_score <- 2 * precision * recall / (precision + recall)

# Create confusion matrix
conf_matrix <- table(Actual = y_test, Predicted = predicted_classes)

# Print the evaluation metrics
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall (Sensitivity):", recall))
print(paste("F1 Score:", f1_score))

# jjjjjjjjjjjjjjj
# Ensure that your model predicts outcomes correctly for instances in X_test
# For example, if 'model' is your trained model:
predictions <- predict(model, newdata = X_test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create the confusion matrix
conf_matrix <- table(Actual = y_test, Predicted = predicted_classes)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)







# Load the necessary library
library(MASS)
# Fit a logistic regression model
model <- glm(Outcome ~ ., data = Diabetes, family = binomial)

# Perform backward stepwise selection
step_model <- stepAIC(model, direction = "backward", trace = FALSE)

# Get the selected features
selected_features <- names(step_model$coefficients)[-1]  # Exclude intercept

# Subset the data with selected features
Diabetes_selected <- Diabetes[, c("Outcome", selected_features)]

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(Diabetes_selected), 0.8 * nrow(Diabetes_selected))
X_train <- Diabetes_selected[train_indices, selected_features] # Exclude "Outcome"
y_train <- Diabetes_selected$Outcome[train_indices]
X_test <- Diabetes_selected[-train_indices, selected_features] # Exclude "Outcome"
y_test <- Diabetes_selected$Outcome[-train_indices]

# Fit the logistic regression model with selected features
# Combine predictors and outcome into a data frame
train_data <- cbind(y_train, X_train)
colnames(train_data)[1] <- "Outcome"  # Rename the first column to "Outcome"

# Fit the logistic regression model with selected features
model_selected <- glm(Outcome ~ ., data = train_data, family = binomial)


# Make predictions on the test set
predictions <- predict(model_selected, newdata = data.frame(X_test), type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
conf_matrix <- table(Actual = y_test, Predicted = predicted_classes)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
rmse <- sqrt(mean((predicted_classes - y_test)^2))
mae <- mean(abs(predicted_classes - y_test))
sd_error <- sd(predicted_classes - y_test)

# Print evaluation metrics
print(paste("Accuracy:", accuracy))
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("Standard deviation of error:", sd_error))
print("Confusion matrix")
print(conf_matrix)
