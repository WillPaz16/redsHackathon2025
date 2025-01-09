###############################################################################
# Name: Joey Endres
# Team: Miami SpreadHawks
# Project: Red's Hackathon
# January 2025
###############################################################################

# Install packages
library(tidyverse)
library(caret)

# Load in the data
savant <- read.csv("savant_data_2021_2023.csv", stringsAsFactors = T)
lahman <- read.csv("lahman_people.csv", stringsAsFactors = T)
sampleSubmission <- read.csv("sample_submission.csv", stringsAsFactors = T)

# Wrangling
## Take a look at the basics of each
### savant
glimpse(savant)
str(savant)

### lahman
glimpse(lahman)
str(lahman)

### sampleSubmission
glimpse(sampleSubmission)
str(sampleSubmission)

## Do some cleaning of savant
savant <- savant %>% 
  mutate(game_year = as.factor(game_year))

## Create a batter's data frame
batters <- savant %>% 
  select(game_date, batter, events:description, stand:p_throws, type:game_year, on_3b:hc_y,
         hit_distance_sc:launch_angle, estimated_ba_using_speedangle:pitch_number, bat_score:fld_score,
         post_bat_score:of_fielding_alignment, delta_home_win_exp:delta_run_exp, times_faced)

## Create a new data frame that highlights each players' event totals for all 3 seasons
eventTotalsPerBatter <- batters %>% 
  group_by(game_year, batter, events) %>% 
  summarise(count = n()) %>% 
  filter(events != "")

## Create a new data frame that adds up the on base events into one on base variable for each player and season   
onBaseEvents <- c("double", "hit_by_pitch", "home_run", "single", "triple", "walk")  
  
obTotals <- eventTotalsPerBatter %>% 
  filter(events %in% onBaseEvents) %>% 
  group_by(game_year, batter) %>% 
  summarise(onBase = sum(count, na.rm = T), .groups = "drop")

## Create a new data frame that adds up the non on base events into one variable for each player and season   
notOnBaseEvents <- c("double_play", "field_out", "grounded_into_double_play", "sac_bunt", "sac_bunt_double_play",
                     "sac_fly", "sac_fly_double_play", "strikeout", "strikeout_double_play")

notOBTotals <- eventTotalsPerBatter %>% 
  filter(events %in% notOnBaseEvents) %>% 
  group_by(game_year, batter) %>% 
  summarise(notOnBase = sum(count, na.rm = T), .groups = "drop")

eventTotalsPerBatterWide <- eventTotalsPerBatter %>%
  pivot_wider(
    names_from = events,    # Column to create new column names
    values_from = count    # Column to populate new columns with values
  )

print(eventTotalsPerBatterWide)
