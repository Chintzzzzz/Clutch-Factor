library(glmnet)
library(dplyr)
library(tibble)

ridge_train <- temp2_75 %>%
  select(player.id, clutch_factor_scaled, shot.statsbomb_xg, shot.statsbomb_xga, is_penalty_won,
         is_winning_goal, is_equalizer, is_goal_when_losing, is_winning_assist,
         is_equalizer_assist, is_assist_when_losing, is_substitution, opponent_difficulty,
         is_away, sub_boost, red_card_boost, away_boost, opponent_boost) %>%
  na.omit()

X_train <- ridge_train %>% select(-player.id, -clutch_factor_scaled) %>% as.matrix()
Y_train <- ridge_train$clutch_factor_scaled

# Train ridge regression with cross-validation
set.seed(123)
ridge_model <- cv.glmnet(X_train, Y_train, alpha = 0, standardize = TRUE)

best_lambda <- ridge_model$lambda.min
print(paste("Best lambda:", best_lambda))

final_model <- glmnet(X_train, Y_train, alpha = 0, lambda = best_lambda, standardize = TRUE)

ridge_test <- temp_75 %>%
  select(player.id, clutch_factor_scaled, shot.statsbomb_xg, shot.statsbomb_xga, is_penalty_won,
         is_winning_goal, is_equalizer, is_goal_when_losing, is_winning_assist,
         is_equalizer_assist, is_assist_when_losing, is_substitution, opponent_difficulty,
         is_away, sub_boost, red_card_boost, away_boost, opponent_boost) %>%
  na.omit()

# Match players present in both datasets (optional, if you want to predict only for players in both seasons)
common_players <- intersect(ridge_train$player.id, ridge_test$player.id)
ridge_test <- ridge_test %>% filter(player.id %in% common_players)

X_test <- ridge_test %>% select(-player.id, -clutch_factor_scaled) %>% as.matrix()
Y_test <- ridge_test$clutch_factor_scaled

# Predict on test data
predictions <- predict(final_model, newx = X_test)

# Calculate R² on test data
sst <- sum((Y_test - mean(Y_test))^2)
sse <- sum((Y_test - predictions)^2)
r2 <- 1 - (sse / sst)

print(paste("Test R^2:", r2))

# Extract coefficients at the best lambda
coef_matrix <- coef(final_model, s = best_lambda)

# Convert sparse matrix to a tidy data frame
coef_df <- as.data.frame(as.matrix(coef_matrix)) %>%
  rownames_to_column(var = "predictor") %>%
  rename(coefficient = s1) %>%           # Use the actual column name here
  filter(predictor != "(Intercept)") %>%
  arrange(desc(abs(coefficient)))

print(coef_df)



