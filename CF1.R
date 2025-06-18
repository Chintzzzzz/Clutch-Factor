library(StatsBombR)
library(tidyverse)
library(ggplot2)
#################INITIALIZATION################
Comp <- FreeCompetitions()
View(Comp)

##########LA LIGA 20-21
Comp1 <- FreeCompetitions() %>%
  filter(competition_id==11 & season_id==90)
Matches <- FreeMatches(Comp1)
Liga20_21 <- free_allevents(MatchesDF = Matches, Parallel = T)
Liga20_21 = allclean(Liga20_21)
Liga20_21 = cleanlocations(Liga20_21)
colnames(Liga20_21)
View(Liga20_21)



##########LA LIGA 19-20
Comp2 <- FreeCompetitions() %>%
  filter(competition_id==11 & season_id==42)
Matches <- FreeMatches(Comp2)
Liga19_20 <- free_allevents(MatchesDF = Matches, Parallel = T)
Liga19_20 = allclean(Liga19_20)
Liga19_20 = cleanlocations(Liga19_20)
View(Liga19_20)



##########LA LIGA 18-19
Comp3 <- FreeCompetitions() %>%
  filter(competition_id==11 & season_id==4)
Matches <- FreeMatches(Comp3)
Liga18_19 <- free_allevents(MatchesDF = Matches, Parallel = T)
Liga18_19 = allclean(Liga18_19)
Liga18_19 = cleanlocations(Liga18_19)
View(Liga18_19)



#################PLAYER POSITION###################################
temp = Liga20_21
AllPasses <- temp %>%
  ungroup() %>%
  mutate(position.category = ifelse(position.name %in% c("Right Center Forward", 
                                                         "Left Center Forward", 
                                                         "Striker",
                                                         "Secondary Striker", 
                                                         "Center Forward"), "Forwards",
                                    ifelse(position.name %in% c("Center Attacking Midfield",
                                                                "Left Attacking Midfield", "Left Midfield",
                                                                "Right Attacking Midfield", "Right Midfield",
                                                                "Left Wing", "Right Wing"), "Attacking Midfielders",
                                           ifelse(position.name %in% c("Center Defensive Midfield", 
                                                                       "Center Midfield",
                                                                       "Left Center Midfield", 
                                                                       "Left Defensive Midfield",
                                                                       "Right Center Midfield", 
                                                                       "Right Defensive Midfield"),
                                                  "Central Midfielders",
                                                  ifelse(position.name %in% c("Left Back", "Left Wing Back", 
                                                                              "Right Back", "Right Wing Back"), "Full Backs",
                                                         ifelse(position.name %in% c("Center Back", "Left Center Back", 
                                                                                     "Right Center Back"), "Center Backs",
                                                                ifelse(grepl("Goalkeeper", position.name), "Goalkeepers", "Missing")))))))

AllPasses$position.category <- factor(AllPasses$position.category, 
                                      levels = c("Forwards", "Attacking Midfielders",
                                                 "Central Midfielders", "Full Backs", 
                                                 "Center Backs", "Goalkeepers", "Missing"))

playersusualposition <- AllPasses %>%
  group_by(position.category, 
           player.name, 
           team.name) %>%
  count() %>%
  arrange(player.name, desc(n)) %>%
  group_by(player.name) %>%
  dplyr::slice(1) %>%
  rename(TypicalPosition = position.category) %>%
  dplyr::select(-n)

playersusualposition



##########LA LIGA 19-20
temp1 = Liga19_20
AllPasses1 <- temp1 %>%
  ungroup() %>%
  mutate(position.category = ifelse(position.name %in% c("Right Center Forward", 
                                                         "Left Center Forward", 
                                                         "Striker",
                                                         "Secondary Striker", 
                                                         "Center Forward"), "Forwards",
                                    ifelse(position.name %in% c("Center Attacking Midfield",
                                                                "Left Attacking Midfield", "Left Midfield",
                                                                "Right Attacking Midfield", "Right Midfield",
                                                                "Left Wing", "Right Wing"), "Attacking Midfielders",
                                           ifelse(position.name %in% c("Center Defensive Midfield", 
                                                                       "Center Midfield",
                                                                       "Left Center Midfield", 
                                                                       "Left Defensive Midfield",
                                                                       "Right Center Midfield", 
                                                                       "Right Defensive Midfield"),
                                                  "Central Midfielders",
                                                  ifelse(position.name %in% c("Left Back", "Left Wing Back", 
                                                                              "Right Back", "Right Wing Back"), "Full Backs",
                                                         ifelse(position.name %in% c("Center Back", "Left Center Back", 
                                                                                     "Right Center Back"), "Center Backs",
                                                                ifelse(grepl("Goalkeeper", position.name), "Goalkeepers", "Missing")))))))

AllPasses1$position.category <- factor(AllPasses1$position.category, 
                                      levels = c("Forwards", "Attacking Midfielders",
                                                 "Central Midfielders", "Full Backs", 
                                                 "Center Backs", "Goalkeepers", "Missing"))

playersusualposition1 <- AllPasses1 %>%
  group_by(position.category, 
           player.name, 
           team.name) %>%
  count() %>%
  arrange(player.name, desc(n)) %>%
  group_by(player.name) %>%
  dplyr::slice(1) %>%
  rename(TypicalPosition = position.category) %>%
  dplyr::select(-n)

playersusualposition1



##########LA LIGA 18-19
temp2 = Liga18_19
AllPasses2 <- temp2 %>%
  ungroup() %>%
  mutate(position.category = ifelse(position.name %in% c("Right Center Forward", 
                                                         "Left Center Forward", 
                                                         "Striker",
                                                         "Secondary Striker", 
                                                         "Center Forward"), "Forwards",
                                    ifelse(position.name %in% c("Center Attacking Midfield",
                                                                "Left Attacking Midfield", "Left Midfield",
                                                                "Right Attacking Midfield", "Right Midfield",
                                                                "Left Wing", "Right Wing"), "Attacking Midfielders",
                                           ifelse(position.name %in% c("Center Defensive Midfield", 
                                                                       "Center Midfield",
                                                                       "Left Center Midfield", 
                                                                       "Left Defensive Midfield",
                                                                       "Right Center Midfield", 
                                                                       "Right Defensive Midfield"),
                                                  "Central Midfielders",
                                                  ifelse(position.name %in% c("Left Back", "Left Wing Back", 
                                                                              "Right Back", "Right Wing Back"), "Full Backs",
                                                         ifelse(position.name %in% c("Center Back", "Left Center Back", 
                                                                                     "Right Center Back"), "Center Backs",
                                                                ifelse(grepl("Goalkeeper", position.name), "Goalkeepers", "Missing")))))))

AllPasses2$position.category <- factor(AllPasses2$position.category, 
                                       levels = c("Forwards", "Attacking Midfielders",
                                                  "Central Midfielders", "Full Backs", 
                                                  "Center Backs", "Goalkeepers", "Missing"))

playersusualposition2 <- AllPasses2 %>%
  group_by(position.category, 
           player.name, 
           team.name) %>%
  count() %>%
  arrange(player.name, desc(n)) %>%
  group_by(player.name) %>%
  dplyr::slice(1) %>%
  rename(TypicalPosition = position.category) %>%
  dplyr::select(-n)

playersusualposition2

#############MATCH SCORES#####################
# Set team name as Barcelona
temp$team_name <- "Barcelona"

# Create a column to store opposition team name, initialized as NA
temp$opposition_team_name <- NA

# Identify the opposition team at the start of each match (minute == 0)
temp <- temp %>%
  group_by(match_id) %>% 
  mutate(opposition_team_name = first(team.name[minute == 0 & team.name != "Barcelona"])) %>%
  ungroup()

# Fill down the opposition team name for the entire match
temp <- temp %>%
  group_by(match_id) %>%
  fill(opposition_team_name, .direction = "down") %>%
  ungroup()



# Step 1: Identify goal-scored events
temp <- temp %>%
  mutate(
    goal_scored = case_when(
      (type.name == "Shot" & shot.outcome.name == "Goal") ~ 1,
      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal") ~ 1,
      type.name == "Own Goal For" ~ 1,
      type.name == "Own Goal Against" ~ -1,
      TRUE ~ 0
    )
  )

# Step 2: Assign scoring_team
temp <- temp %>%
  mutate(
    scoring_team = case_when(
      type.name == "Own Goal For" ~ opposition_team_name,
      type.name == "Own Goal Against" ~ team.name,
      goal_scored == 1 ~ team.name,
      goal_scored == -1 ~ opposition_team_name,
      TRUE ~ NA_character_
    )
  )

# Step 3: Calculate live scores for Barca and opposition (fix for own goals)
temp <- temp %>%
  group_by(match_id) %>%
  mutate(
    team_score = cumsum(
      ifelse(team.name == "Barcelona",
             ifelse((type.name == "Shot" & shot.outcome.name == "Goal") |
                      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal") | 
                      type.name == "Own Goal For",  
                    1, 0),
             0)
    ),
    opposition_team_score = cumsum(
      ifelse(team.name != "Barcelona",
             ifelse((type.name == "Shot" & shot.outcome.name == "Goal") |
                      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal"),
                    1, 0),
             0)
    )
  ) %>%
  ungroup() %>%
  # Ensure Own Goal Against for Barcelona properly accumulates
  group_by(match_id) %>%
  mutate(
    opposition_team_score = cumsum(
      ifelse(type.name == "Own Goal Against" & team.name == "Barcelona", 1, 0)
    ) + opposition_team_score # Add the own goal contribution correctly
  ) %>%
  ungroup()




# Step 4: Determine game state for both teams
temp <- temp %>%
  mutate(
    team_game_state = case_when(
      team_score > opposition_team_score ~ "Winning",
      team_score < opposition_team_score ~ "Losing",
      TRUE ~ "Drawing"
    ),
    opp_team_game_state = case_when(
      opposition_team_score > team_score ~ "Winning",
      opposition_team_score < team_score ~ "Losing",
      TRUE ~ "Drawing"
    )
  )

# Step 5: Create a summary dataframe of total scores for each match
match_summary <- temp %>%
  group_by(match_id, team_name, opposition_team_name) %>%
  summarise(
    final_team_score = max(team_score, na.rm = TRUE),             # Final score for Barcelona
    final_opposition_score = max(opposition_team_score, na.rm = TRUE), # Final score for opposition
    .groups = 'drop'                                              # Ungroup after summarizing
  ) %>%
  distinct(match_id, .keep_all = TRUE)

# Check live scores for a specific match
# trial = temp %>%
#   filter(match_id == 3773661) %>%
#   select(match_id, minute, team.name, type.name, shot.outcome.name, team_score, opposition_team_score) %>%
#   arrange(minute)


# Step 6: Check goal scorers for a specific match ID (e.g., match_id = 3773593)
goal_scorers <- temp %>%
  filter(
    match_id == 3773593 & 
      (shot.outcome.name == "Goal" | type.name %in% c("Own Goal For", "Own Goal Against"))
  ) %>%
  mutate(
    player.name = case_when(
      type.name == "Own Goal For" ~ paste("Own Goal by", opposition_team_name), # Own goal benefits Barca
      type.name == "Own Goal Against" ~ paste("Own Goal by", team.name),       # Own goal against Barca benefits opposition
      TRUE ~ player.name                                                   # Regular goal scorer
    )
  ) %>%
  select(player.name, minute, team.name, type.name) %>%
  arrange(minute)

print(goal_scorers)



##########LA LIGA 19-20
# Set team name as Barcelona
temp1$team_name <- "Barcelona"

# Create a column to store opposition team name, initialized as NA
temp1$opposition_team_name <- NA

# Identify the opposition team at the start of each match (minute == 0)
temp1 <- temp1 %>%
  group_by(match_id) %>% 
  mutate(opposition_team_name = first(team.name[minute == 0 & team.name != "Barcelona"])) %>%
  ungroup()

# Fill down the opposition team name for the entire match
temp1 <- temp1 %>%
  group_by(match_id) %>%
  fill(opposition_team_name, .direction = "down") %>%
  ungroup()


# Step 1: Identify goal-scored events
temp1 <- temp1 %>%
  mutate(
    goal_scored = case_when(
      (type.name == "Shot" & shot.outcome.name == "Goal") ~ 1,
      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal") ~ 1,
      type.name == "Own Goal For" ~ 1,
      type.name == "Own Goal Against" ~ -1,
      TRUE ~ 0
    )
  )

# Step 2: Assign scoring_team
temp1 <- temp1 %>%
  mutate(
    scoring_team = case_when(
      type.name == "Own Goal For" ~ opposition_team_name,
      type.name == "Own Goal Against" ~ team.name,
      goal_scored == 1 ~ team.name,
      goal_scored == -1 ~ opposition_team_name,
      TRUE ~ NA_character_
    )
  )

# Step 3: Calculate live scores for Barca and opposition (fix for own goals)
temp1 <- temp1 %>%
  group_by(match_id) %>%
  mutate(
    team_score = cumsum(
      ifelse(team.name == "Barcelona",
             ifelse((type.name == "Shot" & shot.outcome.name == "Goal") |
                      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal") | 
                      type.name == "Own Goal For",  
                    1, 0),
             0)
    ),
    opposition_team_score = cumsum(
      ifelse(team.name != "Barcelona",
             ifelse((type.name == "Shot" & shot.outcome.name == "Goal") |
                      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal"),
                    1, 0),
             0)
    )
  ) %>%
  ungroup() %>%
  # Ensure Own Goal Against for Barcelona properly accumulates
  group_by(match_id) %>%
  mutate(
    opposition_team_score = cumsum(
      ifelse(type.name == "Own Goal Against" & team.name == "Barcelona", 1, 0)
    ) + opposition_team_score # Add the own goal contribution correctly
  ) %>%
  ungroup()




# Step 4: Determine game state for both teams
temp1 <- temp1 %>%
  mutate(
    team_game_state = case_when(
      team_score > opposition_team_score ~ "Winning",
      team_score < opposition_team_score ~ "Losing",
      TRUE ~ "Drawing"
    ),
    opp_team_game_state = case_when(
      opposition_team_score > team_score ~ "Winning",
      opposition_team_score < team_score ~ "Losing",
      TRUE ~ "Drawing"
    )
  )

# Step 5: Create a summary dataframe of total scores for each match
match_summary1 <- temp1 %>%
  group_by(match_id, team_name, opposition_team_name) %>%
  summarise(
    final_team_score = max(team_score, na.rm = TRUE),             # Final score for Barcelona
    final_opposition_score = max(opposition_team_score, na.rm = TRUE), # Final score for opposition
    .groups = 'drop'                                              # Ungroup after summarizing
  ) %>%
  distinct(match_id, .keep_all = TRUE)

# Check live scores for a specific match
# trial1 = temp1 %>%
#   filter(match_id == 303451) %>%
#   select(match_id, minute, team.name, type.name, shot.outcome.name, team_score, opposition_team_score) %>%
#   arrange(minute)


# Step 6: Check goal scorers for a specific match ID (e.g., match_id = 303451)
goal_scorers1 <- temp1 %>%
  filter(
    match_id == 303451 & 
      (shot.outcome.name == "Goal" | type.name %in% c("Own Goal For", "Own Goal Against"))
  ) %>%
  mutate(
    player.name = case_when(
      type.name == "Own Goal For" ~ paste("Own Goal by", opposition_team_name), # Own goal benefits Barca
      type.name == "Own Goal Against" ~ paste("Own Goal by", team.name),       # Own goal against Barca benefits opposition
      TRUE ~ player.name                                                   # Regular goal scorer
    )
  ) %>%
  select(player.name, minute, team.name, type.name) %>%
  arrange(minute)

print(goal_scorers1)





##########LA LIGA 18-19
# Set team name as Barcelona
temp2$team_name <- "Barcelona"

# Create a column to store opposition team name, initialized as NA
temp2$opposition_team_name <- NA

# Identify the opposition team at the start of each match (minute == 0)
temp2 <- temp2 %>%
  group_by(match_id) %>% 
  mutate(opposition_team_name = first(team.name[minute == 0 & team.name != "Barcelona"])) %>%
  ungroup()

# Fill down the opposition team name for the entire match
temp2 <- temp2 %>%
  group_by(match_id) %>%
  fill(opposition_team_name, .direction = "down") %>%
  ungroup()


# Step 1: Identify goal-scored events
temp2 <- temp2 %>%
  mutate(
    goal_scored = case_when(
      (type.name == "Shot" & shot.outcome.name == "Goal") ~ 1,
      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal") ~ 1,
      type.name == "Own Goal For" ~ 1,
      type.name == "Own Goal Against" ~ -1,
      TRUE ~ 0
    )
  )

# Step 2: Assign scoring_team
temp2 <- temp2 %>%
  mutate(
    scoring_team = case_when(
      type.name == "Own Goal For" ~ opposition_team_name,
      type.name == "Own Goal Against" ~ team.name,
      goal_scored == 1 ~ team.name,
      goal_scored == -1 ~ opposition_team_name,
      TRUE ~ NA_character_
    )
  )

# Step 3: Calculate live scores for Barca and opposition (fix for own goals)
temp2 <- temp2 %>%
  group_by(match_id) %>%
  mutate(
    team_score = cumsum(
      ifelse(team.name == "Barcelona",
             ifelse((type.name == "Shot" & shot.outcome.name == "Goal") |
                      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal") | 
                      type.name == "Own Goal For",  
                    1, 0),
             0)
    ),
    opposition_team_score = cumsum(
      ifelse(team.name != "Barcelona",
             ifelse((type.name == "Shot" & shot.outcome.name == "Goal") |
                      (type.name == "Shot" & shot.type.name == "Penalty" & shot.outcome.name == "Goal"),
                    1, 0),
             0)
    )
  ) %>%
  ungroup() %>%
  # Ensure Own Goal Against for Barcelona properly accumulates
  group_by(match_id) %>%
  mutate(
    opposition_team_score = cumsum(
      ifelse(type.name == "Own Goal Against" & team.name == "Barcelona", 1, 0)
    ) + opposition_team_score # Add the own goal contribution correctly
  ) %>%
  ungroup()




# Step 4: Determine game state for both teams
temp2 <- temp2 %>%
  mutate(
    team_game_state = case_when(
      team_score > opposition_team_score ~ "Winning",
      team_score < opposition_team_score ~ "Losing",
      TRUE ~ "Drawing"
    ),
    opp_team_game_state = case_when(
      opposition_team_score > team_score ~ "Winning",
      opposition_team_score < team_score ~ "Losing",
      TRUE ~ "Drawing"
    )
  )

# Step 5: Create a summary dataframe of total scores for each match
match_summary2 <- temp2 %>%
  group_by(match_id, team_name, opposition_team_name) %>%
  summarise(
    final_team_score = max(team_score, na.rm = TRUE),             # Final score for Barcelona
    final_opposition_score = max(opposition_team_score, na.rm = TRUE), # Final score for opposition
    .groups = 'drop'                                              # Ungroup after summarizing
  ) %>%
  distinct(match_id, .keep_all = TRUE)

# Check live scores for a specific match
# trial2 = temp2 %>%
#   filter(match_id == 16056) %>%
#   select(match_id, minute, team.name, type.name, shot.outcome.name, team_score, opposition_team_score) %>%
#   arrange(minute)


# Step 6: Check goal scorers for a specific match ID (e.g., match_id = 16056)
goal_scorers2 <- temp2 %>%
  filter(
    match_id == 16056 & 
      (shot.outcome.name == "Goal" | type.name %in% c("Own Goal For", "Own Goal Against"))
  ) %>%
  mutate(
    player.name = case_when(
      type.name == "Own Goal For" ~ paste("Own Goal by", opposition_team_name), # Own goal benefits Barca
      type.name == "Own Goal Against" ~ paste("Own Goal by", team.name),       # Own goal against Barca benefits opposition
      TRUE ~ player.name                                                   # Regular goal scorer
    )
  ) %>%
  select(player.name, minute, team.name, type.name) %>%
  arrange(minute)

print(goal_scorers2)

#########SCORE DIFFERENCE##########################
temp <- temp %>%
  mutate(
    score_difference = team_score - opposition_team_score  # Calculate the difference
  )


##########LA LIGA 19-20
temp1 <- temp1 %>%
  mutate(
    score_difference = team_score - opposition_team_score  # Calculate the difference
  )

##########LA LIGA 18-19
temp2 <- temp2 %>%
  mutate(
    score_difference = team_score - opposition_team_score  # Calculate the difference
  )

###########HOME OR AWAY###################
# Step 1: Create a data frame for match_id and home/away mapping
match_ids <- c(3764440, 3764661, 3773369, 3773372, 3773377, 3773386, 3773387, 3773403, 
               3773415, 3773428, 3773457, 3773466, 3773474, 3773477, 3773497, 3773523,
               3773526, 3773547, 3773552, 3773565, 3773571, 3773585, 3773586, 3773587,
               3773593, 3773597, 3773625, 3773631, 3773656, 3773660, 3773661, 3773665,
               3773672, 3773689, 3773695)

home_away_vector <- c("H", "A", "H", "H", "H", "A", "A", "H", "H", "A",
                      "H", "A", "H", "H", "A", "H", "H", "H", "A", "A",
                      "A", "H", "H", "A", "H", "A", "A", "A", "A", "H",
                      "H", "A", "H", "A", "A")

# Combine match IDs and home/away vector into a data frame
home_away_mapping <- data.frame(
  match_id = match_ids,
  home_away = home_away_vector
)

# Step 2: Add the home/away information to the `temp` dataset
temp <- temp %>%
  left_join(home_away_mapping, by = "match_id") # Join on match_id

match_summary <- temp %>%
  group_by(match_id, team_name, opposition_team_name, home_away) %>%  # Include home_away in grouping
  summarise(
    final_team_score = max(team_score, na.rm = TRUE),             # Final score for Barcelona
    final_opposition_score = max(opposition_team_score, na.rm = TRUE), # Final score for opposition
    .groups = 'drop'                                              # Ungroup after summarizing
  ) %>%
  distinct(match_id, .keep_all = TRUE)                            # Ensure no duplicate match IDs

# View the updated match summary
print(match_summary)



##########LA LIGA 19-20

# Match IDs
match_ids1 <- c(303377, 303400, 303421, 303430, 303451, 303470, 303473, 303479, 
                 303487, 303493, 303504, 303516, 303517, 303524, 303532, 303548, 
                 303596, 303600, 303610, 303615, 303634, 303652, 303664, 303666, 
                 303674, 303680, 303682, 303696, 303700, 303707, 303715, 303725, 
                 303731)

# Home/Away Vector
home_away_vector1 <- c("H", "A", "A", "H", "H", "A", "H", "H", "H", "H", 
                        "A", "A", "H", "A", "H", "A", "H", "H", "H", "H", 
                        "H", "A", "A", "A", "A", "A", "H", "H", "A", "A", 
                        "A", "A", "H")

# Combine match IDs and home/away vector into a data frame
home_away_mapping1 <- data.frame(
  match_id = match_ids1,
  home_away = home_away_vector1
)

# Step 2: Add the home/away information to the `temp` dataset
temp1 <- temp1 %>%
  left_join(home_away_mapping1, by = "match_id") # Join on match_id

match_summary1 <- temp1 %>%
  group_by(match_id, team_name, opposition_team_name, home_away) %>%  # Include home_away in grouping
  summarise(
    final_team_score = max(team_score, na.rm = TRUE),             # Final score for Barcelona
    final_opposition_score = max(opposition_team_score, na.rm = TRUE), # Final score for opposition
    .groups = 'drop'                                              # Ungroup after summarizing
  ) %>%
  distinct(match_id, .keep_all = TRUE)                            # Ensure no duplicate match IDs

# View the updated match summary
View(match_summary1)




##########LA LIGA 18-19

# Match IDs
match_ids2 <- c(15946, 15956, 15973, 15978, 15986, 15998, 16010, 16023, 16029, 16056, 
                16073, 16079, 16086, 16095, 16109, 16120, 16131, 16136, 16149, 16157, 
                16173, 16182, 16190, 16196, 16205, 16215, 16231, 16240, 16248, 16265, 
                16275, 16289, 16306, 16317)

# Home/Away Vector
home_away_vector2 <- c("H", "A", "H", "A", "H", "A", "H", "A", "H", "H", 
                       "A", "H", "A", "A", "H", "A", "H", "H", "A", "H", 
                       "A", "H", "A", "A", "H", "A", "H", "A", "H", "H", 
                       "A", "H", "H", "A")

# Step 1: Create mapping of match_id to home/away status
home_away_mapping2 <- data.frame(
  match_id = match_ids2,
  home_away = home_away_vector2
)

# Step 2: Merge home/away information into temp2 dataset
temp2 <- temp2 %>%
  left_join(home_away_mapping2, by = "match_id")  # Ensure match_id exists in temp2

# Step 3: Summarize match details
match_summary2 <- temp2 %>%
  group_by(match_id, team_name, opposition_team_name, home_away) %>%
  summarise(
    final_team_score = max(team_score, na.rm = TRUE),             
    final_opposition_score = max(opposition_team_score, na.rm = TRUE), 
    .groups = 'drop'  # Ungrouping for clarity
  ) %>%
  distinct(match_id, .keep_all = TRUE)  # Ensure unique match IDs

# View the updated match summary
View(match_summary2)



########BARCA PLAYERS##################

barcelona_players <- unique(temp[temp$team.name == "Barcelona", "player.name"])
print(barcelona_players)

##########LA LIGA 19-20
barcelona_players1 <- unique(temp1[temp1$team.name == "Barcelona", "player.name"])
print(barcelona_players1)

##########LA LIGA 18-19
barcelona_players2 <- unique(temp2[temp2$team.name == "Barcelona", "player.name"])
print(barcelona_players2)


# Combine all Barcelona players from different seasons without duplicates
all_barcelona_players <- union(barcelona_players, union(barcelona_players1, barcelona_players2))

# Print the unique list of players
print(all_barcelona_players)

###############XG##############################
#temp <- temp %>% select(-xG)
temp$xG = round(ifelse(is.na(temp$shot.statsbomb_xg), 0, temp$shot.statsbomb_xg), 2)

##########LA LIGA 19-20
temp1$xG = round(ifelse(is.na(temp1$shot.statsbomb_xg), 0, temp1$shot.statsbomb_xg), 2)

##########LA LIGA 19-20
temp2$xG = round(ifelse(is.na(temp2$shot.statsbomb_xg), 0, temp2$shot.statsbomb_xg), 2)

##############xGA#####################################

# Extract key pass ID and xG values for shots
xGA_data <- temp %>%
  filter(type.name == "Shot", !is.na(shot.key_pass_id)) %>%  # Keep only shots with an assist
  select(shot.key_pass_id, xGA = shot.statsbomb_xg)  # Rename xG column to xGA

# Ensure IDs are the same type before joining
temp <- temp %>%
  mutate(id = as.character(id),  # Convert ID to character if necessary
         shot.key_pass_id = as.character(shot.key_pass_id)) %>%
  left_join(xGA_data, by = c("id" = "shot.key_pass_id")) %>%  # Match key pass ID with event ID
  mutate(xGA = round(ifelse(is.na(xGA), 0, xGA), 2))  # Replace NA with 0 and round
colnames(temp)
##########LA LIGA 19-20
# Extract xG values for shots and link them to the key pass (assist)
xGA_data1 <- temp1 %>%
  filter(type.name == "Shot") %>%  # Keep only shot events
  select(shot.key_pass_id, xGA = shot.statsbomb_xg)  # Select key pass ID and xG of the shot

# Join this xGA data back to the main dataset
temp1 <- temp1 %>%
  left_join(xGA_data1, by = c("id" = "shot.key_pass_id")) %>%  # Match key pass ID with event ID
  mutate(xGA = round(ifelse(is.na(xGA), 0, xGA), 2))



##########LA LIGA 18-19
# Extract xG values for shots and link them to the key pass (assist)
xGA_data2 <- temp2 %>%
  filter(type.name == "Shot") %>%  # Keep only shot events
  select(shot.key_pass_id, xGA = shot.statsbomb_xg)  # Select key pass ID and xG of the shot

# Join this xGA data back to the main dataset
temp2 <- temp2 %>%
  left_join(xGA_data2, by = c("id" = "shot.key_pass_id")) %>%  # Match key pass ID with event ID
  mutate(xGA = round(ifelse(is.na(xGA), 0, xGA), 2))



#############CLUTCH FACTOR(MANUAL)###################
# Get player minutes and join with main data
player_minutes = get.minutesplayed(temp)
minutes_data <- player_minutes %>%
  select(player.id, match_id, MinutesPlayed)
total_player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))
temp = left_join(temp, minutes_data, by = c("player.id", "match_id"))

# Filter dataset to keep only minutes >= 75 and process assists
temp_75 <- temp %>% filter(minute >= 75)

# Create game state modifiers
temp_75 <- temp_75 %>%
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


temp_75 <- temp_75 %>%
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
penalty_winners <- temp_75 %>%
  filter(foul_won.penalty == TRUE) %>%
  select(match_id, minute, player.name) %>%
  distinct()


# Calculate boosts and clutch factor
temp_75 <- temp_75 %>%
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






#################################################
# # Count total number of equalizers
# equalizers_count <- sum(temp_75$is_winning_goal, na.rm = TRUE)
# equalizers_count
# # Count total number of equalizer assists
# equalizer_assists_count <- sum(temp_75$is_winning_assist, na.rm = TRUE)
# equalizer_assists_count
# 
# equalizer_assist <- temp_75 %>%
#   filter(is_winning_assist == 1) %>%
#   select(match_id, minute, player.name, opposition_team_name, team_score)
# 
# print(equalizer_assist)
# 
# equalizer_scorers <- temp_75 %>%
#   filter(is_winning_goal == 1) %>%
#   select(match_id, minute, player.name, opposition_team_name, team_score)
# 
# print(equalizer_scorers)
# 
# # Filter rows with the specific match_id
# subset_match <- temp_75 %>%
#   filter(match_id == 3773474)
# 
# # View the subset
# View(subset_match)
# 
# # Filter rows with the specific match_id and penalty winners
# penalty_winners_match <- temp_75 %>%
#   filter(is_penalty_won == 1) %>%
#   select(player.name, minute)
# 
# # Print the results
# print(penalty_winners_match)





###############TESTING(MANUAL)#################
#Checking Distribution
hist(temp_75$clutch_factor_scaled, breaks = 30, main = "Distribution of Clutch Factor", col = "blue")

colnames(temp_75)

#Checking players
top_barca_players <- temp_75 %>%
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


top_barca_players = left_join(top_barca_players, total_player_minutes)

top_barca_players = top_barca_players %>% mutate(nineties = minutes/90)

top_barca_players = top_barca_players %>% mutate(clutch_per90 = total_clutch/nineties)

head(top_barca_players, 10) # Top 10 clutch barca players
View(top_barca_players)
View(top_barca_players1)
View(top_barca_players2)

library(dplyr)

# Step 1: Combine all dataframes and remove NA rows
combined_seasons_clutch <- bind_rows(
  top_barca_players %>% filter(!is.na(player.id)),
  top_barca_players1 %>% filter(!is.na(player.id)),
  top_barca_players2 %>% filter(!is.na(player.id))
) 

# Step 2: Group and aggregate metrics
total_clutch <- combined_seasons_clutch %>%
  group_by(player.id, player.name) %>%
  summarise(
    total_clutch = sum(total_clutch, na.rm = TRUE),
    matches_played = sum(matches_played, na.rm = TRUE),
    minutes = sum(minutes, na.rm = TRUE),
    nineties = sum(nineties, na.rm = TRUE),
    # Recalculate clutch_per90 based on aggregated totals
    clutch_per90 = ifelse(nineties > 0, total_clutch / nineties, 0),
    .groups = 'drop'
  ) %>%
  # For players missing from some seasons, replace NaN with 0
  mutate(clutch_per90 = coalesce(clutch_per90, 0)) %>%
  arrange(desc(total_clutch)) 

# View results
print(total_clutch)


coutinho_clutch_goals <- temp_75_all %>%
  filter(player.name == "Pedro González López", is_winning_goal == 1 | is_equalizer == 1 | is_assist ==1) %>%
  select(minute, match_id, is_winning_goal, is_equalizer, is_assist)

coutinho_clutch_goals

temp_75 %>%
  filter(sub_boost > 1) %>%
  arrange(desc(sub_boost)) %>%
  select(player.name, MinutesPlayed, sub_boost, clutch_factor)

# high_clutch_players <- temp_75 %>%
#   filter(player.name %in% c("Moriba Kourouma Kourouma")) %>%
#   select(player.name, MinutesPlayed, clutch_factor, shot.statsbomb_xg,shot.statsbomb_xga, opponent_difficulty, 
#          is_penalty_won, is_winning_goal, is_equalizer,  is_assist, is_substitution, sub_boost,match_id, is_assist_for_goal,
#          is_winning_assist,is_equalizer_assist,is_assist_when_losing)


#Checking if players with high clutch scores actually scored match-winning goals.
temp_75 %>%
  filter(is_winning_goal == 1) %>%
  summarise(avg_clutch = mean(clutch_factor_scaled, na.rm = TRUE))

#Testing if clutch_factor_scaled is related to goals, xG, and key performance metrics
cor(temp_75$clutch_factor_scaled, temp_75$goal_scored, use = "complete.obs")
cor(temp_75$clutch_factor_scaled, temp_75$shot.statsbomb_xg, use = "complete.obs")











