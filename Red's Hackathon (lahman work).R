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
lahman <- read.csv("lahman_people.csv", stringsAsFactors = T)

glimpse(lahman)
str(lahman)

# Wrangling
## Cleaning the data by getting rid of players with no mlb id
for (i in 1:nrow(lahman)) {
  if (is.na(lahman$player_mlb_id[i]) || lahman$player_mlb_id[i] == "") {
    lahman <- lahman[-i, ]
  } else {
    {}
  }
}


## create 2 new variables "age" and "timeSinceDebut" and get rid of other unnecessary variables
referenceDate <- ymd("2024-03-20")

cleanLahman <- lahman %>% 
  mutate(
    age = as.integer(interval(birthDate, referenceDate) / years(1)),
    timeSinceDebut = as.integer(interval(debut, referenceDate) / years(1))
  ) %>% 
  select(-(playerID_LAHMAN:birthDay), -(throws:birthDate))

nlevels(cleanLahman$player_mlb_id)





