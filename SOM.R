library(tidyverse)
library(kohonen)

# --- Step 1: Aggregate event data to player-level statistics ---
predictors <- c("xG", "xGA", "is_away", "opponent_boost", 
                "is_penalty_won", "is_winning_goal", "is_equalizer", 
                "is_goal_when_losing", "is_winning_assist", 
                "is_equalizer_assist", "is_assist_when_losing", 
                "sub_boost")

player_stats <- temp_75_all %>%
  filter(team.name == "Barcelona") %>%
  group_by(player.id, player.name) %>%
  summarise(
    across(all_of(predictors), ~ sum(., na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  na.omit()



# --- Step 2: SOM Clustering ---
som_data <- player_stats %>% 
  select(-player.id, -player.name)

# Normalize variables (z-score standardization)
som_data_scaled <- scale(som_data)

# Create SOM grid
som_grid <- somgrid(xdim = 5, ydim = 4, topo = "hexagonal")

# Train SOM
set.seed(123)
som_model <- som(som_data_scaled, 
                 grid = som_grid, 
                 rlen = 500,
                 keep.data = TRUE)

# --- Step 3: Feature Importance Analysis ---
feature_importance <- apply(som_model$codes[[1]], 2, var)
normalized_importance <- feature_importance / sum(feature_importance)

sorted_importance <- sort(normalized_importance, decreasing = TRUE)
barplot(sorted_importance,
        main = "Player Clustering Feature Importance",
        ylab = "Relative Importance",
        col = "black",
        las = 2,
        cex.names = 0.7,
        names.arg = names(sorted_importance),
        border = NA)

bp <- barplot(sorted_importance, plot = FALSE)
barplot(sorted_importance,
        main = "Player Clustering Feature Importance",
        ylab = "Relative Importance",
        col = "black",
        names.arg = rep("", length(sorted_importance)))
text(x = bp, y = par("usr")[3] - 0.01,
     labels = names(sorted_importance), srt = 45, adj = 1, xpd = TRUE, cex = 0.7)

player_stats <- player_stats %>%
  mutate(
    clutch_factor = 
      0.4  * xG +
      0.3  * xGA +
      0.5  * is_penalty_won +
      0.8  * is_winning_goal +
      0.8  * is_equalizer +
      0.9  * is_goal_when_losing +
      0.6  * is_winning_assist +
      0.85 * is_equalizer_assist +
      0.9  * is_assist_when_losing 
  )

# --- Step 4: Cluster Analysis ---
player_stats$som_node <- som_model$unit.classif

node_clutch_factors <- player_stats %>%
  group_by(som_node) %>%
  summarise(
    total_clutch_factor = sum(clutch_factor, na.rm = TRUE),
    n_players = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_clutch_factor))

hot_threshold <- quantile(node_clutch_factors$total_clutch_factor, 0.75)
hot_nodes <- node_clutch_factors %>% 
  filter(total_clutch_factor >= hot_threshold)

hot_players <- player_stats %>%
  select(player.name,clutch_factor) %>%
  arrange(desc(clutch_factor))

hot_players

# --- Step 5: Visualization ---
plot(som_model, type = "mapping", 
     labels = player_stats$player.name,
     main = "Player Clusters")

plot(som_model, type = "property",
     property = player_stats$clutch_factor,
     main = "Clutch Factor Distribution Across SOM")


 player_cluster_table <- player_stats %>%
  select(player.name, som_node) %>%
  arrange(som_node, player.name)

print(player_cluster_table)
