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
new_data <- subset(nba_team_box, select = c(season, team_slug, team_winner,assists,blocks,field_goal_pct,fouls,free_throw_pct,steals,total_rebounds,team_score))
new_data <- new_data [c(4969:7598),]
new_data$binary_variable <- ifelse(new_data$team_winner==TRUE,1,0)
pie(table(new_data$binary_variable),main = "Pie Chart of True/False Variables",labels = c("False","True"),col = c("red","blue"))
boxplot(field_goal_pct ~ binary_variable, data = new_data,xlab = "Binary Variable", ylab = "field_goal_pct",main = "Boxplot of Continuous Variable by Binary Variable")


write.csv(nba_team_box, file = "nba_team_box_data.csv", row.names = FALSE)

#bivariate code

lmmodel <- lm(binary_variable ~ assists + blocks, data = new_data)
summary(lmmodel)
lmmodel1 <- lm(binary_variable ~ assists + field_goal_pct, data = new_data)
summary(lmmodel1)
lmmodel2 <- lm(binary_variable ~ fouls + field_goal_pct, data = new_data)
summary(lmmodel2)
lmmodel3 <- lm(binary_variable ~ fouls + total_rebounds, data = new_data)
summary(lmmodel3)
lmmodel4 <- lm(binary_variable ~ free_throw_pct + total_rebounds, data = new_data)
summary(lmmodel4)


#data cleaning
model <- lm(team_score ~ assists + fouls + free_throw_pct + steals + total_rebounds, data = new_data)
summary(model)

model1 <- lm(team_score  ~ assists + free_throw_pct + field_goal_pct, data = new_data)
summary(model1)

model2 <- lm(team_score ~ fouls + free_throw_pct + total_rebounds + binary_variable, data = new_data)
summary(model2)


# log transformation and Linear Regression

log_assists <- log(new_data$assists) 
log_blocks <- log(new_data$blocks)
log_steals <- log(new_data$steals)
log_total_rebounds <- log(new_data$total_rebounds)
log_field_goal_pct <- log(new_data$field_goal_pct)
log_fouls <- log(new_data$fouls)
log_free_throw_pct <- log(new_data$free_throw_pct)

model3 <- lm(new_data$team_score ~ log_assists + log_free_throw_pct+ log_field_goal_pct + log_fouls, data = new_data)
summary(model3)


#sqrt transformation and linear regression

sqrt_assists <- sqrt(new_data$assists)
sqrt_blocks <- sqrt(new_data$blocks)
sqrt_steals <- sqrt(new_data$steals)
sqrt_total_rebounds <- sqrt(new_data$total_rebounds)
sqrt_field_goal_pct <- sqrt(new_data$field_goal_pct)
sqrt_fouls <- sqrt(new_data$fouls)
sqrt_free_throw_pct <- sqrt(new_data$free_throw_pct)

model4 <- lm(new_data$team_score ~ sqrt_free_throw_pct + sqrt_steals + sqrt_field_goal_pct + sqrt_fouls, data = new_data)
summary(model4)

#sq transformation and linear regression

sq_assists <- new_data$assists^2
sq_blocks <- new_data$blocks^2
sq_steals <- new_data$steals^2
sq_total_rebounds <- new_data$total_rebounds^2
sq_field_goal_pct <- new_data$field_goal_pct^2
sq_fouls <- new_data$fouls^2
sq_free_throw_pct <- new_data$free_throw_pct^2

model5 <- lm(new_data$team_score ~ sq_steals + sq_total_rebounds + sq_fouls + sq_assists, data = new_data)
summary(model5)

# Load the leaps package for subset selection
library(leaps)

# Check dimensions of X and Y
nrow(X)
length(Y)

# Convert X to a data frame
X <- as.data.frame(X)

# Convert Y to a vector
Y <- as.vector(Y)

# Assuming there are multiple predictor variables in the original dataset
# Select the predictor variables and assign them to X
X <- regression_data[, predictor_terms]

# Check the structure of X
str(X)

# Perform subset selection
subset_model <- regsubsets(X, Y, method = "forward", nvmax = length(predictor_terms))
subset_summary <- summary(subset_model)


# Check the class of subset_model
print(class(subset_model))

# Check if coef() returns any output without using 'print' or 'length'
coef(subset_model, id = "best")

# Try to extract coefficients without using 'length'
model_coefs <- coef(subset_model, id = "best")
print(model_coefs)

# Check the structure of subset_model
str(subset_model)

# Check the help page for coef.regsubsets
?coef.regsubsets

# Try accessing coefficients without specifying 'id'
coef(subset_model)


coef(subset_model, id = "best")



# Install and load the glmnet package
install.packages("glmnet")
library(glmnet)

# Now you can use the glmnet function
shrinkage_model <- glmnet(X, Y)

print(dim(X))
print(length(coef(subset_model, id = "best")))

# Subset Selection Results
subset_terms <- colnames(X)[which(coef(subset_model, id = "best") != 0)]
subset_coef <- coef(subset_model, id = "best")
cv_error_subset <- min(subset_summary$cp)

# Shrinkage Results
cv_error_shrinkage <- min(shrinkage_model$cvm)

# Residual Diagnosis
plot(subset_model, scale = "Cp")
plot(shrinkage_model)

# Check the structure of subset_model
str(subset_model)

# Review the documentation for plot() and regsubsets()
?plot
?regsubsets
clean_data <- na.omit(new_data)
nrow(clean_data)

# Compare the results and determine the optimal regression model
if (cv_error_subset < cv_error_shrinkage) {
  optimal_model <- subset_model
} else {
  optimal_model <- shrinkage_model
}

logistic_model <- glm(binary_variable ~ ., data = new_data, family = "binomial")

lda_model <- lda(binary_variable ~ ., data = new_data)

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

