library(tidyverse)
library(janitor)
library(lubridate)
library(modelr)

# BDC_Final work

scout <- read_csv("hackathon_scouting.csv") # You'll have to change this part if you don't have the file locally

# clean up the data a little bit
scout <- scout %>%
  janitor::clean_names()

scout <- scout %>%
  mutate(game_date = lubridate::parse_date_time(game_date, "y-m-d"))

scout <- scout %>%
  separate(clock, c("minutes_period", "seconds_period", "split_seconds_period"))

scout <- scout %>%
  mutate(minutes_period = as.numeric(minutes_period),
         seconds_period = as.numeric(seconds_period),
         split_seconds_period = as.numeric(split_seconds_period))

# we can remove the split seconds column because it's always 0 in this case

scout <- scout %>%
  select(-split_seconds_period)

scout <- scout %>%
  mutate(detail_1 = as.factor(detail_1))

# Add the info that we need

scout <- scout %>%
  mutate(distance_from_goal = sqrt((x_coordinate - 189)^2 + (y_coordinate - 42.5)^2),
         advantage = ifelse(team == away_team, away_team_skaters - home_team_skaters,
                            home_team_skaters - away_team_skaters),
         power_play = ifelse(advantage > 0, 1, 0),
         goal = ifelse(event == "Goal", 1, 0),
         pass_distance = sqrt((x_coordinate - x_coordinate_2)^2 + (y_coordinate - y_coordinate_2)^2),
         diff_x = x_coordinate_2 - x_coordinate,
         diff_y = y_coordinate_2 - y_coordinate,
         completed_pass = case_when(event == "Play" ~ 1,
                                    event == "Incomplete Play" ~ 0,
                                    TRUE ~ as.numeric(NA)),
         distance_from_goal_2 = sqrt((x_coordinate_2 - 189)^2 + (y_coordinate_2 - 42.5)^2),
         goal_angle = asin((x_coordinate - 189) / distance_from_goal),
         goal_angle_2 = asin((x_coordinate_2 - 189) / distance_from_goal_2),
         index = 1:n())

# Caculate pass completion probability

passes <- scout %>%
  filter(event == "Incomplete Play" | event == "Play")

pass_glm <- glm(completed_pass ~ pass_distance + diff_x + power_play + advantage + distance_from_goal_2 + distance_from_goal, family = "binomial", data = passes)

#summary(pass_glm)

passes <- passes %>%
  add_predictions(pass_glm, var = "xcomp", type = "response") %>%
  mutate(pass_oe = completed_pass - xcomp)

# Create xG model

shots <- scout %>%
  filter(event == "Shot" | event == "Goal") %>%
  mutate(goal = ifelse(event == "Goal", 1, 0))

xG_glm <- glm(goal ~ distance_from_goal + power_play + advantage + goal_angle, family = "binomial", data = shots)

summary(xG_glm)

shots <- shots %>%
  add_predictions(xG_glm, var = "xG", type = "response") %>%
  mutate(goe = goal - xG)

# Find xG for each point of a pass: start and finish

xG_scout_1 <- scout %>%
  filter(event == "Play" | event == "Incomplete Play") %>%
  select(index, distance_from_goal, power_play, advantage, goal_angle, pass_distance, diff_x) %>%
  add_predictions(xG_glm, var = "xG", type = "response") %>%
  select(index, xG)

xG_scout_2 <- scout %>%
  filter(event == "Play" | event == "Incomplete Play") %>%
  select(index, distance_from_goal = distance_from_goal_2, power_play, advantage, 
         goal_angle = goal_angle_2) %>%
  add_predictions(xG_glm, var = "xG_2", type = "response") %>%
  select(index, xG_2)

# add all probabilities to our main data frame

scout <- scout %>%
  left_join(xG_scout_1) %>%
  left_join(xG_scout_2) %>%
  add_predictions(pass_glm, var = "pass_success_prob", type = "response") %>%
  mutate(decision = (xG_2 * pass_success_prob) - xG)

# Aggregate the data at player level

scout %>%
  filter(!is.na(decision), x_coordinate_2 >= 125) %>%
  filter(player != player_2) %>% # there are a few instances where the player "passes" to themself
  group_by(player, team) %>%
  summarize(success_rate = mean(decision > 0),
            count = n()) %>%
  arrange(desc(success_rate)) %>%
  filter(count > 40) %>%
  head(10)

# Aggredate data at team level

scout %>%
  filter(!is.na(decision), x_coordinate_2 >= 125) %>%
  filter(player != player_2) %>%
  group_by(team) %>%
  summarize(success_rate = mean(decision > 0),
            count = n()) %>%
  arrange(desc(success_rate)) %>%
  filter(count > 200) %>%
  head(10)
