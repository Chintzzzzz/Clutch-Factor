 # temp2 <- temp2 %>%
 #   select(-MinutesPlayed.y)
 # temp2 <- temp2 %>%
 #   rename(MinutesPlayed = MinutesPlayed.x)

# Get player minutes and join with main data
player_minutes2 = get.minutesplayed(temp2)
minutes_data2 <- player_minutes2 %>%
  select(player.id, match_id, MinutesPlayed)
total_player_minutes2 = player_minutes2 %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))
temp2 = left_join(temp2, minutes_data2, by = c("player.id", "match_id"))

# Filter dataset to keep only minutes >= 75 and process assists
temp2_75 <- temp2 %>% filter(minute >= 75)

# Create game state modifiers
temp2_75 <- temp2_75 %>%
  group_by(match_id) %>%
  mutate(
    is_substitution = ifelse(player.name %in% substitution.replacement.name, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(
    shot.statsbomb_xga = ifelse(is.na(xGA), 0, xGA),
    is_away = ifelse(home_away == "A", 0.05, 0),
    opponent_difficulty = case_when(
      opposition_team_name == "Real Madrid" ~ 0.05,
      opposition_team_name == "Atlético Madrid" ~ 0.03,
      opposition_team_name %in% c("Athletic Bilbao", "Valencia CF", "Sevilla FC") ~ 0.015,
      opposition_team_name == "Espanyol" ~ 0.005,
      TRUE ~ 0
    ),
    is_winning_goal = ifelse(goal_scored == 1 & score_difference == 1, 1, 0),
    is_equalizer = ifelse(goal_scored == 1 & score_difference == 0, 1, 0),
    is_goal_when_losing = ifelse(goal_scored == 1 & score_difference < 0 & score_difference > -3, 1, 0),
    is_assist = ifelse(pass.goal_assist == TRUE, 1, 0)
  ) %>%
  left_join(penalty_winners, by = c("match_id", "minute", "player.name")) %>%
  mutate(
    is_penalty_won = ifelse(is.na(foul_won.penalty), 0, as.numeric(foul_won.penalty))
  )


temp2_75 <- temp2_75 %>%
  arrange(match_id, minute, second) %>%
  group_by(match_id) %>%
  mutate(
    time_seconds = minute * 60 + second,
    is_winning_assist = 0,
    is_equalizer_assist = 0,
    is_assist_when_losing = 0
  ) %>%
  group_modify(~ {
    current_match <- .x
    total_rows <- nrow(current_match)
    
    for (i in 1:total_rows) {
      # Ensure we only proceed if is_assist is TRUE and not NA
      if (!is.na(current_match$is_assist[i]) && current_match$is_assist[i] == 1) {
        look_ahead <- min(i + 5, total_rows)
        
        for (j in (i + 1):look_ahead) {
          if (j > total_rows) break
          time_diff <- current_match$time_seconds[j] - current_match$time_seconds[i]
          
          # Ensure goal_scored and score_difference are not NA
          if (!is.na(current_match$goal_scored[j]) && current_match$goal_scored[j] == 1 && time_diff <= 30) {
            goal_diff <- current_match$score_difference[j]
            
            # Ensure score_difference is not NA before assigning assist types
            if (!is.na(goal_diff)) {
              current_match$is_winning_assist[i] <- as.numeric(goal_diff == 1)
              current_match$is_equalizer_assist[i] <- as.numeric(goal_diff == 0)
              current_match$is_assist_when_losing[i] <- as.numeric(goal_diff < 0 && goal_diff > -3)
            }
            break
          }
        }
      }
    }
    current_match
  }) %>%
  ungroup() %>%
  select(-time_seconds)


# Process penalty winners
penalty_winners2 <- temp2_75 %>%
  filter(foul_won.penalty == TRUE) %>%
  select(match_id, minute, player.name) %>%
  distinct()


# Calculate boosts and clutch factor
temp2_75 <- temp2_75 %>%
  mutate(
    sub_boost = ifelse(is_substitution == 1, pmin(1 + (pmax(90 - MinutesPlayed, 0)) / 180, 1.5), 1),
    red_card_boost = ifelse(
      player.name %in% barcelona_players &
        foul_committed.card.name %in% c("Second Yellow", "Red Card") &
        (shot.statsbomb_xg > 0 | is_penalty_won == 1 |
           is_winning_goal == 1 | is_equalizer == 1 | is_goal_when_losing == 1 |
           is_equalizer_assist == 1 | is_winning_assist == 1 | is_assist_when_losing == 1),
      0.1, 0
    ),
    away_boost = ifelse(
      home_away == "A" &
        (shot.statsbomb_xg > 0 | is_penalty_won == 1 |
           is_winning_goal == 1 | is_equalizer == 1 | is_goal_when_losing == 1 |
           is_equalizer_assist == 1 | is_winning_assist == 1 | is_assist_when_losing == 1),
      0.05, 0
    ),
    opponent_boost = ifelse(
      (shot.statsbomb_xg > 0 | is_penalty_won == 1 |
         is_winning_goal == 1 | is_equalizer == 1 | is_goal_when_losing == 1 |
         is_equalizer_assist == 1 | is_winning_assist == 1 | is_assist_when_losing == 1) &
        opponent_difficulty > 0,
      opponent_difficulty, 0
    ),
    clutch_factor = sub_boost * (
      (0.1 * coalesce(shot.statsbomb_xg, 0)) +
        (0.05 * coalesce(shot.statsbomb_xga, 0)) +
        is_away +
        opponent_boost +
        (0.4 * is_penalty_won) +
        (0.9 * is_winning_goal) +
        (0.75 * is_equalizer) +
        (0.5 * is_goal_when_losing) +
        (0.45 * is_winning_assist) +
        (0.375 * is_equalizer_assist) +
        (0.3 * is_assist_when_losing) +
        red_card_boost
    ),
    clutch_factor_scaled = (clutch_factor - min(clutch_factor, na.rm = TRUE)) / 
      (max(clutch_factor, na.rm = TRUE) - min(clutch_factor, na.rm = TRUE))
  )


#######Checking players
top_barca_players2 <- temp2_75 %>%
  group_by(player.id,player.name, team.name, match_id) %>%  # Ensure match-wise grouping
  summarise(total_clutch = sum(clutch_factor_scaled, na.rm = TRUE), .groups = "drop") %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(
    total_clutch = sum(total_clutch, na.rm = TRUE),
    matches_played = n_distinct(match_id),  # Count unique matches played
    .groups = "drop"
  ) %>%
  filter(team.name == "Barcelona") %>%
  arrange(desc(total_clutch))


top_barca_players2 = left_join(top_barca_players2, total_player_minutes2)

top_barca_players2 = top_barca_players2 %>% mutate(nineties = minutes/90)

top_barca_players2 = top_barca_players2 %>% mutate(clutch_per90 = total_clutch/nineties)

head(top_barca_players2, 10) # Top 10 clutch barca players
View(top_barca_players2)



# Load necessary libraries
library(glmnet)
library(tidyverse)

# Select relevant predictor variables
ridge_data2 <- temp2_75 %>%
  select(
    clutch_factor_scaled, shot.statsbomb_xg, shot.statsbomb_xga, is_penalty_won,
    is_winning_goal, is_equalizer, is_goal_when_losing, is_winning_assist,
    is_equalizer_assist, is_assist_when_losing, is_substitution, opponent_difficulty,
    is_away, sub_boost, red_card_boost, away_boost, opponent_boost
  ) %>%
  na.omit()  # Remove NA values

# Separate predictors (X) and target (Y)
X2 <- as.matrix(ridge_data2 %>% select(-clutch_factor_scaled))  # Predictor variables
Y2 <- ridge_data2$clutch_factor_scaled  # Target variable

# Define a sequence of lambda values for ridge regression
lambda_seq <- 10^seq(3, -3, by = -0.1)  # From 1000 to 0.001

# Fit ridge regression model using cross-validation
set.seed(123)
ridge_model2 <- cv.glmnet(X2, Y2, alpha = 0, lambda = lambda_seq, standardize = TRUE)

# Get best lambda (optimal penalty)
best_lambda2 <- ridge_model2$lambda.min
print(paste("Best lambda:", best_lambda2))

# Fit final ridge model with best lambda
final_ridge2 <- glmnet(X2, Y2, alpha = 0, lambda = best_lambda2, standardize = TRUE)

# Predictions on the same dataset
predictions2 <- predict(final_ridge2, X2)

# Calculate R² (coefficient of determination)
sst2 <- sum((Y2 - mean(Y2))^2)  # Total sum of squares
sse2 <- sum((Y2 - predictions2)^2)  # Sum of squared errors
r2_2 <- 1 - (sse2 / sst2)  # R² formula

print(paste("R² Value:", r2_2))

#######Clustering##########
# Find common columns across all three datasets
common_cols <- Reduce(intersect, list(names(temp_75), names(temp1_75), names(temp2_75)))

# Reorder columns alphabetically
common_cols <- sort(common_cols)

# Select only the common columns in all datasets and reorder them
temp_75_common <- temp_75[, common_cols]
temp1_75_common <- temp1_75[, common_cols]
temp2_75_common <- temp2_75[, common_cols]

# Combine the datasets
temp_75_all <- rbind(temp_75_common, temp1_75_common, temp2_75_common)

# Check the structure of the combined dataset
str(combined_data)
#sum(temp_75$is_winning_assist)

library(mclust)
library(ggplot2)


# Select relevant features for clustering
clustering_data <- temp_75_all %>%
  select(clutch_factor_scaled, shot.statsbomb_xg, shot.statsbomb_xga, 
         away_boost, opponent_boost, is_penalty_won, is_winning_goal, 
         is_equalizer, is_goal_when_losing, is_winning_assist, 
         is_equalizer_assist, is_assist_when_losing, 
         sub_boost, red_card_boost) %>%
  na.omit()

# Remove constant columns
clustering_data <- clustering_data %>%
  select(-where(~ var(., na.rm = TRUE) < 1e-4))

robust_scaler <- function(x) {
  (x - median(x)) / IQR(x)
}
clustering_data_scaled <- clustering_data %>% mutate(across(where(is.numeric), robust_scaler))

# Remove problematic rows (if any)
clustering_data_scaled <- clustering_data_scaled[!rowSums(is.nan(clustering_data_scaled) | is.infinite(clustering_data_scaled)), ]

# Fit GMM model and let it choose the optimal number of clusters
gmm_model <- Mclust(clustering_data_scaled)

# Print summary of the model
summary(gmm_model)

# Extract cluster assignments
clustering_data$cluster <- gmm_model$classification

# Visualize the clustering results
library(ggplot2)
library(factoextra)

fviz_cluster(list(data = clustering_data_scaled, cluster = gmm_model$classification), 
             ellipse.type = "norm", geom = "point", 
             ggtheme = theme_minimal())

# Save clustering results for further analysis
head(clustering_data)

# Perform PCA on numeric_data2
pca_result <- prcomp(numeric_data2, scale. = TRUE)

# Extract PC1 scores
pc1_scores <- pca_result$x[, 1]  # First column is PC1

# Ensure matching rows for clutch_factor_scaled
valid_rows <- which(complete.cases(numeric_data2))
filtered_clutch_factor <- combined_data$clutch_factor_scaled[valid_rows]

# Check correlation
correlation <- cor(pc1_scores, filtered_clutch_factor)
print(paste("Correlation between PC1 and clutch_factor_scaled:", correlation))

# Regression: PC1 predicting clutch_factor_scaled
pc1_model <- lm(filtered_clutch_factor ~ pc1_scores)
summary(pc1_model)
