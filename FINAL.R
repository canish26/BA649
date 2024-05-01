if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("sportsdataverse/hoopR", dependencies = TRUE, update = TRUE)

pacman::p_load(dplyr, zoo, ggimage, gt)


progressr::with_progress({
  nba_team_box <- hoopR::load_nba_team_box(2021:hoopR::most_recent_nba_season())
})


glue::glue("{nrow(nba_team_box)} rows of NBA team boxscore data from {length(unique(nba_team_box$game_id))} games.")
dplyr::glimpse(nba_team_box)

summary(nba_team_box)
new_data <- subset(nba_team_box, select = c(season, team_slug, team_winner,assists,blocks,field_goal_pct,fouls,free_throw_pct,steals,total_rebounds,team_score,opponent_team_slug))
new_data$binary_variable <- ifelse(new_data$team_winner==TRUE,1,0)
new_data <- new_data [c(4969:7598),]
mean(new_data$assists)
mean(new_data$blocks)
mean(new_data$field_goal_pct)
mean(new_data$fouls)
mean(new_data$free_throw_pct)
mean(new_data$steals)
mean(new_data$total_rebounds)
mean(new_data$team_score)
sd(new_data$assists)
sd(new_data$blocks)
sd(new_data$field_goal_pct)
sd(new_data$fouls)
sd(new_data$free_throw_pct)
sd(new_data$steals)
sd(new_data$total_rebounds)
sd(new_data$team_score)
range(new_data$assists)
range(new_data$blocks)
range(new_data$field_goal_pct)
range(new_data$fouls)
range(new_data$free_throw_pct)
range(new_data$steals)
range(new_data$total_rebounds)
range(new_data$team_score)
plot(new_data$assists,new_data$binary_variable)
plot(new_data$blocks,new_data$binary_variable)
plot(new_data$field_goal_pct,new_data$binary_variable)
plot(new_data$fouls,new_data$binary_variable)
plot(new_data$free_throw_pct,new_data$binary_variable)
plot(new_data$steals,new_data$binary_variable)
plot(new_data$total_rebounds,new_data$binary_variable)
plot(new_data$team_score,new_data$binary_variable)
plot(new_data$assists,new_data$team_score)
plot(new_data$blocks,new_data$team_score)
plot(new_data$field_goal_pct,new_data$team_score)
plot(new_data$fouls,new_data$team_score)
plot(new_data$free_throw_pct,new_data$team_score)
plot(new_data$total_rebounds,new_data$team_score)
plot(new_data$team_score,new_data$binary_variable)
hist(new_data$assists,new_data$binary_variable)
hist(new_data$blocks)
hist(new_data$field_goal_pct)
hist(new_data$fouls)
hist(new_data$free_throw_pct)
hist(new_data$steals)
hist(new_data$total_rebounds)
hist(new_data$team_score)

boxplot(assists ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "assists",main = "Boxplot of Continuous Variable by Binary Variable")
boxplot(blocks ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "blocks",main = "Boxplot of Continuous Variable by Binary Variable")
boxplot(field_goal_pct ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "field_goal_pct",main = "Boxplot of Continuous Variable by Binary Variable")
boxplot(fouls ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "fouls",main = "Boxplot of Continuous Variable by Binary Variable")
boxplot(free_throw_pct ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "free_throw_pct",main = "Boxplot of Continuous Variable by Binary Variable")
boxplot(steals ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "steals",main = "Boxplot of Continuous Variable by Binary Variable")
boxplot(total_rebounds ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "total_rebounds",main = "Boxplot of Continuous Variable by Binary Variable")
boxplot(team_score ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "team_score",main = "Boxplot of Continuous Variable by Binary Variable")

boxplot(assists ~ team_score, data = new_data,xlab = "team_score", ylab = "assists",main = "Boxplot of Continuous Variable by Team Score")
boxplot(blocks ~ team_score, data = new_data,xlab = "team_score", ylab = "blocks",main = "Boxplot of Continuous Variable by Team Score")
boxplot(field_goal_pct ~ team_score, data = new_data,xlab = "team_score", ylab = "field_goal_pct ",main = "Boxplot of Continuous Variable by Team Score")
boxplot(fouls ~ team_score, data = new_data,xlab = "team_score", ylab = "fouls ",main = "Boxplot of Continuous Variable by Team Score")
boxplot(free_throw_pct ~ team_score, data = new_data,xlab = "team_score", ylab = "free_throw_pct ",main = "Boxplot of Continuous Variable by Team Score")
boxplot(steals ~ team_score, data = new_data,xlab = "team_score", ylab = "steals ",main = "Boxplot of Continuous Variable by Team Score")
boxplot(total_rebounds ~ team_score, data = new_data,xlab = "team_score", ylab = "total_rebounds ",main = "Boxplot of Continuous Variable by Team Score")
any_missing <-any(is.na(new_data))
any_missing <-any(is.na(new_data$season))
any_missing <-any(is.na(new_data$team_slug))
any_missing <-any(is.na(new_data$team_winner))
any_missing <-any(is.na(new_data$assists))
any_missing <-any(is.na(new_data$blocks))
any_missing <-any(is.na(new_data$field_goal_pct))
any_missing <-any(is.na(new_data$fouls))
any_missing <-any(is.na(new_data$free_throw_pct))
any_missing <-any(is.na(new_data$steals))
any_missing <-any(is.na(new_data$total_rebounds))
any_missing <-any(is.na(new_data$team_score))
any_missing <-any(is.na(new_data$opponent_team_slug))
any_missing <-any(is.na(new_data$binary_variable))






pie(table(new_data$binary_variable),main = "Pie Chart of True/False Variables",labels = c("False","True"),col = c("red","blue"))

# Install necessary packages
install.packages("caret", dependencies = TRUE)

# Load necessary libraries
library(caret, lib.loc = .libPaths(), verbose = TRUE)


# Load necessary libraries
library(caret)
library(MASS)
library(glmnet)
library(leaps)

# Subset the data for regression analysis
regression_data <- new_data

# Define predictor terms
predictor_terms <- c("assists", "blocks", "field_goal_pct", "fouls", "free_throw_pct", "steals", "total_rebounds")

# Define outcome variable
outcome_variable <- "team_score"
str(X)
str(Y)

# Convert Y to a vector
Y <- as.vector(Y$team_score)

Y <- as.vector(Y)

# Check the dimensions of X and Y
dim(X)
length(Y)


# Subset X to match the length of Y
X <- X[1:length(Y), ]
breaks = sequence (0, 2630, by = 1) 

# Load necessary libraries
install.packages(c("caret", "MASS", "glmnet", "leaps"))
library(caret)
library(MASS)
library(glmnet)
library(leaps)

# Subset the data for regression analysis
regression_data <- subset(new_data, select = c("assists", "blocks", "field_goal_pct", "fouls", "free_throw_pct", "steals", "total_rebounds", "team_score"))

# Define predictor terms
predictor_terms <- c("assists", "blocks", "field_goal_pct", "fouls", "free_throw_pct", "steals", "total_rebounds")

# Define outcome variable
outcome_variable <- "team_score"

### Regression Problem using Generalized Regression Models ###

# Check the structure of the regression_data dataframe
str(regression_data)

# Verify the names of columns in regression_data
names(regression_data)

# Ensure that 'team_score' is correctly named in the dataframe
# If not, replace 'team_score' with the correct name

# Run regsubsets() with error handling
tryCatch({
  subset_model <- regsubsets(
    team_score ~ .,
    data = regression_data,
    nvmax = length(predictor_terms),
    method = "forward"
  )
}, error = function(e) {
  message("Error: ", e)
})

# Run regsubsets() for subset selection
subset_model <- regsubsets(
  team_score ~ .,
  data = regression_data,
  nvmax = length(predictor_terms),
  method = "forward"
)

# Summary of subset selection
subset_summary <- summary(subset_model)

# Check the summary
print(subset_summary)


# Subset Selection
subset_model <- regsubsets(team_score ~ ., data = regression_data[, predictor_terms], nvmax = length(predictor_terms), method = "forward")
subset_summary <- summary(subset_model)
subset_selected_terms <- names(coef(subset_model, id = which.min(subset_summary$cp)))
subset_coefficients <- coef(subset_model, id = which.min(subset_summary$cp))
subset_cv_errors <- subset_summary$cp[which.min(subset_summary$cp)]

# Check the data type of team_score
class(regression_data$team_score)
# Convert the outcome variable to a matrix
y_matrix <- as.matrix(regression_data[, outcome_variable])

# Run glmnet again
ridge_model <- glmnet(
  x = as.matrix(regression_data[, predictor_terms]),
  y = y_matrix,
  alpha = 0
)

# Convert the outcome variable to a matrix
y_matrix <- as.matrix(regression_data[, outcome_variable])

# Run cv.glmnet again
ridge_cv <- cv.glmnet(
  x = as.matrix(regression_data[, predictor_terms]),
  y = y_matrix,
  alpha = 0
)


# Extract selected terms
ridge_selected_terms <- predict(ridge_cv, type = "nonzero")$s0

# Extract coefficients
ridge_coefficients <- coef(ridge_cv)

# Extract cross-validated errors
ridge_cv_errors <- min(ridge_cv$cvm)

# Check the structure of the outcome variable
str(regression_data[, outcome_variable])


# Compare Results and Determine Optimal Model
if (subset_cv_errors < ridge_cv_errors) {
  optimal_model <- "Subset Selection"
  selected_terms <- subset_selected_terms
  coefficients <- subset_coefficients
  cv_error <- subset_cv_errors
} else {
  optimal_model <- "Ridge Regression"
  selected_terms <- rownames(ridge_coefficients)[ridge_selected_terms]
  coefficients <- coef(ridge_cv)[, ridge_selected_terms]
  cv_error <- ridge_cv_errors
}


# Compare Results and Determine Optimal Model
if (subset_cv_errors < ridge_cv_errors) {
  optimal_model <- "Subset Selection"
  selected_terms <- subset_selected_terms
  coefficients <- subset_coefficients
  cv_error <- subset_cv_errors
} else {
  optimal_model <- "Ridge Regression"
  selected_terms <- rownames(ridge_coefficients)[ridge_selected_terms]
  coefficients <- coef(ridge_cv)[, ridge_selected_terms]
  cv_error <- ridge_cv_errors
}

# Display the model outputs
cat("Optimal Model:", optimal_model, "\n")
cat("Selected Terms:", selected_terms, "\n")
cat("Coefficients:\n")
print(coefficients)
cat("Cross-Validated Error:", cv_error, "\n")

# Comment on Findings
# Based on the outputs of the optimal model, analyze coefficients and discuss findings related to business/research questions.


### Classification Problem using Logistic Regression, Linear Discriminant Analysis, KNN ###

# Define outcome variable for classification
classification_data <- new_data
classification_data$binary_variable <- ifelse(classification_data$team_winner == TRUE, 1, 0)

# Define predictor terms for classification
classification_predictors <- c("assists", "blocks", "field_goal_pct", "fouls", "free_throw_pct", "steals", "total_rebounds")

# Logistic Regression
logistic_model <- glm(binary_variable ~ ., data = classification_data[, c(classification_predictors, "binary_variable")], family = binomial)
logistic_cv_error <- 1 - sum(predict(logistic_model, type = "response") > 0.5 == classification_data$binary_variable) / nrow(classification_data)
logistic_selected_terms <- names(coef(logistic_model))

# Logistic Regression
logistic_model <- glm(binary_variable ~ ., 
                      data = classification_data[, c(classification_predictors, "binary_variable")], 
                      family = binomial)

# Predict probabilities and calculate error
logistic_probabilities <- predict(logistic_model, type = "response")
logistic_cv_error <- 1 - sum(ifelse(logistic_probabilities > 0.5, 1, 0) == classification_data$binary_variable) / nrow(classification_data)


# Linear Discriminant Analysis (LDA)
lda_model <- lda(binary_variable ~ ., data = classification_data[, c(classification_predictors, "binary_variable")])
lda_cv_error <- mean(lda_model$posterior[, 1] > 0.5 != classification_data$binary_variable)
lda_selected_terms <- names(lda_model$scaling)

# Linear Discriminant Analysis (LDA)
lda_model <- lda(binary_variable ~ ., 
                 data = classification_data[, c(classification_predictors, "binary_variable")])

# Predict probabilities and calculate error
lda_probabilities <- predict(lda_model)$posterior[, 1]
lda_cv_error <- mean(ifelse(lda_probabilities > 0.5, 1, 0) != classification_data$binary_variable)

# Print Model Accuracies
cat("Logistic Regression CV Error:", logistic_cv_error, "\n")
cat("LDA CV Error:", lda_cv_error, "\n")

# KNN
knn_model <- train(binary_variable ~ ., data = classification_data[, c(classification_predictors, "binary_variable")], method = "knn")
knn_cv_error <- 1 - min(knn_model$results$Accuracy)
knn_selected_terms <- knn_model$bestTune$k

# Correcting the outcome variable to a factor
classification_data$binary_variable <- as.factor(classification_data$binary_variable)

# KNN model training
knn_model <- train(binary_variable ~ ., 
                   data = classification_data[, c(classification_predictors, "binary_variable")], 
                   method = "knn")

# Check for missing values in results
if (is.null(knn_model$results$Accuracy)) {
  cat("Error: No results available for KNN model.\n")
} else {
  # Calculate CV error
  knn_cv_error <- 1 - min(knn_model$results$Accuracy)
  
  # Extract selected terms (k value)
  knn_selected_k <- knn_model$bestTune$k
  
  # Print the outputs
  cat("KNN CV Error:", knn_cv_error, "\n")
  cat("Selected k:", knn_selected_k, "\n")
}


# Compare Results and Determine Optimal Model
optimal_classification_model <- ifelse(logistic_cv_error < lda_cv_error & logistic_cv_error < knn_cv_error, "Logistic Regression",
                                       ifelse(lda_cv_error < knn_cv_error, "LDA", "KNN"))


# Check the coefficients of the logistic regression model
logistic_coefficients <- coef(logistic_model)

# Check if any of the coefficients contain interaction terms
interaction_terms <- grep(":|\\*", names(logistic_coefficients))

if (length(interaction_terms) > 0) {
  cat("Interaction terms were used in the logistic regression model.\n")
  cat("Coefficients involving interaction terms:\n")
  print(logistic_coefficients[interaction_terms])
} else {
  cat("No interaction terms were used in the logistic regression model.\n")
}

