# Get player minutes and join with main data
player_minutes1 = get.minutesplayed(temp1)
minutes_data1 <- player_minutes1 %>%
  select(player.id, match_id, MinutesPlayed)
total_player_minutes1 = player_minutes1 %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))
temp1 = left_join(temp1, minutes_data1, by = c("player.id", "match_id"))

# temp1 <- temp1 %>%
#  select(-MinutesPlayed.y)
# temp1 <- temp1 %>%
#  rename(MinutesPlayed = MinutesPlayed.x)

# Filter dataset to keep only minutes >= 75 and process assists
temp1_75 <- temp1 %>% filter(minute >= 75)

# Create game state modifiers
temp1_75 <- temp1_75 %>%
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


temp1_75 <- temp1_75 %>%
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
penalty_winners1 <- temp1_75 %>%
  filter(foul_won.penalty == TRUE) %>%
  select(match_id, minute, player.name) %>%
  distinct()


# Calculate boosts and clutch factor
temp1_75 <- temp1_75 %>%
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
top_barca_players1 <- temp1_75 %>%
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


top_barca_players1 = left_join(top_barca_players1, total_player_minutes1)

top_barca_players1 = top_barca_players1 %>% mutate(nineties = minutes/90)

top_barca_players1 = top_barca_players1 %>% mutate(clutch_per90 = total_clutch/nineties)

head(top_barca_players1, 10) # Top 10 clutch barca players
View(top_barca_players1)

