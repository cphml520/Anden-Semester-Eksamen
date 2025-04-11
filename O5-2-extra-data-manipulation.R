library(mongolite)
library(jsonlite)
library(tidyverse)

# connect to mongo
con_matches <- mongo(collection = "matches",
                        db = "statsbomb",
                        url = "mongodb://localhost")

data_matches <- con_matches$find('{}')
data_matches <- jsonlite::flatten(data_matches)

# check that all the matches are there
sum(data_shots$match_id %in% data_matches$match_id)

# create a new column
data_matches <- data_matches %>%
  mutate(game = paste0(
    home_team.home_team_name, " vs ",
    away_team.away_team_name, " (",
    match_date, ")"
  ))

# replace match_id in data_shot with game from data_matches
data_shots <- data_shots %>%
  left_join(data_matches %>% select(match_id, competition.competition_name ,game), by = "match_id")

#######
# adjust the shot outcome
data_shots <- data_shots %>% 
  mutate(shot.outcome.name = case_when(
    shot.outcome.name == "Off T" ~ "Uden for mål",
    shot.outcome.name == "Saved" ~ "Reddet",
    shot.outcome.name == "Blocked" ~ "Blokeret",
    shot.outcome.name == "Goal" ~ "Mål",
    TRUE ~ shot.outcome.name
  ))

###### Convert the end location to x and y
##### Pull the x and y out of location
data_shots <- data_shots %>%
  mutate(shot.end_location = gsub('c\\(|\\)|\\"', '', shot.end_location)) %>%
  separate(shot.end_location, into = c("end.x", "end.y", "end.z"), sep = ", ", convert = TRUE, fill = "right") %>%
  mutate(across(c(end.x, end.y, end.z), as.numeric))
