library(mongolite)
library(jsonlite)
library(tidyverse)

###########################################
####Indhent data fra MongoDB og filtrer####
###########################################

con_matches <- mongo(collection = "matches",
                    db = "statsbomb",
                    url = "mongodb://localhost")

all_matches <- con_matches$find()
all_matches <- jsonlite::flatten(all_matches)

male_matches <- all_matches %>% filter(home_team.home_team_gender == "male" & away_team.away_team_gender == "male")
female_matches <- all_matches %>% filter(home_team.home_team_gender == "female" & away_team.away_team_gender == "female")

con_events <- mongo(collection = "events",
                    db = "statsbomb",
                    url = "mongodb://localhost")

con_maleevents <- mongo(collection = "maleevents",
                        db = "statsbomb",
                        url = "mongodb://localhost")

fields <- '{"match_id": 1, "type.name": 1, "pass.outcome.name": 1, "pass.length": 1, "pass.type.name": 1,
            "duel.outcome.name": 1, "shot.outcome.name": 1, "shot.type.name": 1, "shot.statsbomb_xg": 1,
            "dribble.outcome.name": 1, "foul_committed.card.name": 1, "_id": 0, "matchId": 1}'

male_events <- con_maleevents$find(query = '{}', fields = fields) %>% jsonlite::flatten()
female_events <- con_events$find(query = '{}', fields = fields) %>% jsonlite::flatten()

male_matches_filtered <- male_matches[,c(1:2,4:5,13,15,18)]
female_matches_filtered <- female_matches[,c(1:2,4:5,13,15,18)]

male_matches_filtered <- male_matches_filtered %>%
  rename(gender = home_team.home_team_gender)

female_matches_filtered <- female_matches_filtered %>%
  rename(gender = home_team.home_team_gender)

male_merged <- male_events %>%
  left_join(male_matches_filtered, by = c("matchId" = "match_id"))

female_merged <- female_events %>%
  left_join(female_matches_filtered, by = c("match_id" = "match_id"))

female_merged <- female_merged %>%
  rename(matchId = match_id)

all_merged <- bind_rows(male_merged, female_merged)
all_merged <- all_merged %>% 
  filter(season.season_name %in% c("2021/2022", "2023/2024", "2020/2021", "2022/2023", "2023", "2022", "2024", "2020"))

### What is the share of games?
all_merged %>% 
  group_by(gender) %>% 
  summarize(total = n_distinct(matchId))

##############################
####Gennemsnitlig Frispark####
##############################

free_kicks_summary <- all_merged %>%
  select(gender, pass.type.name, shot.type.name, matchId) %>%
  filter(!is.na(gender)) %>%
  filter(pass.type.name == "Free Kick" | shot.type.name == "Free Kick") %>%
  group_by(gender, matchId) %>%
  summarise(
    total_free_kicks = n(),
    .groups = "drop"
  )

t.test(total_free_kicks ~ gender, data = free_kicks_summary, var.equal = FALSE)
# signifikant

free_kicks_summary_final <- free_kicks_summary %>%
  group_by(gender) %>%
  summarise(
    avg_free_kicks = round(mean(total_free_kicks)),
    sd_free_kicks = round(sd(total_free_kicks), 2),
    .groups = "drop"
  )

##############################
##Gennemsnitlig Hjørnespark###
##############################

corner_kicks_summary <- all_merged %>%
  select(gender, pass.type.name, shot.type.name, matchId) %>%
  filter(!is.na(gender)) %>%
  filter(pass.type.name == "Corner" | shot.type.name == "Corner") %>%
  group_by(gender, matchId) %>%
  summarise(
    total_corner_kicks = n(),
    .groups = "drop"
  )

t.test(total_corner_kicks ~ gender, data = corner_kicks_summary, var.equal = FALSE)
# ikke signifikant

corner_kicks_summary_final <- corner_kicks_summary %>%
  group_by(gender) %>%
  summarise(
    avg_corner_kicks = round(mean(total_corner_kicks)),
    sd_corner_kicks = round(sd(total_corner_kicks), 2),
    .groups = "drop"
  )


#################################
####Gennemsnitlig straffekort####
#################################

card_counts_avg <- all_merged %>%
  select(gender, foul_committed.card.name, matchId) %>%
  filter(foul_committed.card.name %in% c("Yellow Card", "Red Card"), !is.na(gender)) %>%
  group_by(gender, matchId, foul_committed.card.name) %>%
  summarise(card_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = foul_committed.card.name, values_from = card_count, values_fill = list(card_count = 0))

t.test(`Yellow Card` ~ gender, data = card_counts_avg, var.equal = FALSE)
# signifikant

t.test(`Red Card` ~ gender, data = card_counts_avg, var.equal = FALSE)
# signifikant

card_counts_avg_final <- card_counts_avg %>%
  group_by(gender) %>%
  summarise(
    avg_yellow_cards = round(mean(`Yellow Card`), 2),
    sd_yellow_cards = round(sd(`Yellow Card`), 4),
    avg_red_cards = round(mean(`Red Card`), 2),
    sd_red_cards = round(sd(`Red Card`), 4),
    .groups = "drop"
  )

######################################
####Gennemsnitlig fejlafleveringer####
######################################

incomplete_passes_summary <- all_merged %>%
  filter(type.name == "Pass", !is.na(gender)) %>%
  group_by(gender, matchId) %>%
  summarise(
    total_passes = n(),
    total_incomplete_passes = sum(pass.outcome.name %in% c("Incomplete", "Out", "Unknown", "Pass Offside", "Injury Clearance")),
    total_successful_passes = sum(is.na(pass.outcome.name)),
    .groups = "drop"
  ) %>%
  mutate(
    successful_pass_percentage = (total_successful_passes / total_passes) * 100,
    incomplete_pass_percentage = (total_incomplete_passes / total_passes) * 100
  )

t.test(total_passes ~ gender, data = incomplete_passes_summary, var.equal = FALSE)
# signifikant
t.test(successful_pass_percentage ~ gender, data = incomplete_passes_summary, var.equal = FALSE)
# signifikant

incomplete_passes_summary_final <- incomplete_passes_summary %>%
  group_by(gender) %>%
  summarise(
    avg_passes_per_game = round(mean(total_passes)),
    successful_pass_pct = round(mean(successful_pass_percentage), 2),
    incomplete_pass_pct = round(mean(incomplete_pass_percentage), 2),
    sd_passes_per_game = round(sd(total_passes), 2),
    sd_successful_pass_pct = round(sd(successful_pass_percentage, na.rm = TRUE), 2),
    .groups = "drop"
  )


#######################################
####Gennemsnitlig afleveringslængde####
######################################

pass_length_avg <- all_merged %>%
  select(gender, type.name, pass.length, matchId) %>%
  filter(type.name == "Pass", !is.na(gender)) %>%
  group_by(gender, matchId) %>%
  summarise(
    pass_length_avg = round(mean(pass.length),2),
    .groups = "drop")

t.test(pass_length_avg ~ gender, data = pass_length_avg, var.equal = FALSE)
# ikke signifikant

pass_length_avg_final <- pass_length_avg %>%
  group_by(gender) %>%
  summarise(
    avg_pass_length = round(mean(pass_length_avg),2),
    sd_pass_length = round(mean(pass_length_avg),3),
    .groups = "drop"
  )


#################################
####Gennemsnitlig fejlet skud####
#################################

missed_shots_summary <- all_merged %>%
  select(gender, type.name, shot.outcome.name, matchId) %>%
  filter(type.name == "Shot") %>%
  filter(!is.na(gender)) %>%
  group_by(matchId, gender) %>%
  summarise(
    total_missed_shots = sum(shot.outcome.name %in% c("Off Target", "Saved", "Wayward", "Post", "Blocked", "Saved to Post", "Saved Off Target")),
    total_shots = n(),
    .groups = "drop"
  )

t.test(total_shots ~ gender, data = missed_shots_summary, var.equal = FALSE)
# ikke signifikant

t.test(total_missed_shots ~ gender, data = missed_shots_summary, var.equal = FALSE)
# ikke signifikant

missed_shots_summary_final <- missed_shots_summary %>% 
  group_by(gender) %>%
  summarise(
    avg_missed_shots = round(mean(total_missed_shots)),
    sd_missed_shots = round(sd(total_missed_shots)),
    missed_shot_pct = round((sum(total_missed_shots) / sum(total_shots)) * 100, 2),
    .groups = "drop"
  )


###############################
####Gennemsnitlig scoringer####
###############################

goals_summary <- all_merged %>%
  select(gender, shot.outcome.name, matchId) %>%
  filter(shot.outcome.name == "Goal") %>%
  filter(!is.na(gender)) %>%
  group_by(gender, matchId) %>%
  summarise(
    total_goals = n(),
    .groups = "drop"
  )

goals_summary_final <- goals_summary %>% 
  group_by(gender) %>%
  summarise(
    avg_goals = round(mean(total_goals),2),
    sd_goals = round(sd(total_goals),2),
    .groups = "drop"
  )

###############################
####Gennemsnitlig dueller####
###############################

duels_summary <- all_merged %>% 
  select(gender, type.name, duel.outcome.name, matchId) %>% 
  filter(type.name == "Duel") %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender, matchId) %>% 
  summarize(
    total_duels = n(),
    successful_duels_percentage = round(sum(duel.outcome.name %in% c("Succes In Play", "Won") / total_duels * 100), 1),
    .groups = "drop"
  )

t.test(total_duels ~ gender, data = duels_summary, var.equal = FALSE)
#signifikant

t.test(successful_duels_percentage ~ gender, data = duels_summary, var.equal = FALSE)
#signifikant

duels_summary_final <-duels_summary %>% 
  group_by(gender) %>%
  summarise(
    avg_duels = round(mean(total_duels)),
    sd_duels = round(sd(total_duels)),
    pct_successful_duels = round(mean(successful_duels_percentage), 1),
    sd_successful_duels = round(sd(successful_duels_percentage), 1),
    .groups = "drop"
  )
 

###############################
####Gennemsnitlig dribbeler####
###############################

dribbles_summary <- all_merged %>% 
  select(gender, type.name, dribble.outcome.name, matchId) %>% 
  filter(type.name == "Dribble") %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender, matchId) %>% 
  summarize(
    total_dribbles = n(),
    complete_dribbles_percentage = round(sum(dribble.outcome.name == "Complete") / total_dribbles * 100, 1),
    .groups = "drop"
  )

t.test(total_dribbles ~ gender, data = dribbles_summary, var.equal = FALSE)
#signifikant

t.test(complete_dribbles_percentage ~ gender, data = dribbles_summary, var.equal = FALSE)
#signifikant

dribbles_summary_final <- dribbles_summary %>% 
  group_by(gender) %>%
  summarise(
    avg_dribbles = round(mean(total_dribbles)),
    sd_dribbles = round(sd(total_dribbles),1),
    pct_complete_dribbles = round(mean(complete_dribbles_percentage), 1),
    sd_complete_dribbles_pct = round(sd(complete_dribbles_percentage), 2),
    .groups = "drop"
  )

###############################
####Gennemsnitlig xG####
###############################

xG_summary <- all_merged %>% 
  select(gender, type.name, shot.statsbomb_xg, matchId) %>% 
  filter(type.name == "Shot") %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender, matchId) %>% 
  summarize(
    xG_per_kamp = round(mean(shot.statsbomb_xg), 2),
    .groups = "drop"
  )

t.test(xG_per_kamp ~ gender, data = xG_summary, var.equal = FALSE)
#ikke significant

xG_summary_final <- xG_summary %>% 
  group_by(gender) %>%
  summarise(
    avg_xG = round(mean(xG_per_kamp), 3),
    sd_xG = round(sd(xG_per_kamp), 3),
    .groups = "drop"
  )

###############################
######### Death ball #########
##############################

death_ball <- all_merged %>% 
  select(gender, type.name, matchId) %>% 
  filter(type.name %in% c("Foul Committed", "Injury Stoppage", "Player Off", "Substitution")) %>%
  filter(!is.na(gender)) %>%
  group_by(gender, matchId) %>% 
  summarize(
    death_balls = n(),
    .groups = "drop"
  )
  
t.test(death_balls ~ gender, data = death_ball, var.equal = FALSE)
#signifikant

death_ball_final <- death_ball %>% 
  group_by(gender) %>% 
  summarize(
    avg_death_ball = round(mean(death_balls)),
    .groups = "drop"
  )
  

###############################
########### SUMMARY ###########
##############################

overview <- cbind(free_kicks_summary_final, corner_kicks_summary_final[,-1], card_counts_avg_final[,-1],
                  incomplete_passes_summary_final[,-1], missed_shots_summary_final[,-1],
                  goals_summary_final[,-1], duels_summary_final[,-1], dribbles_summary_final[,-1],
                  xG_summary_final[,-1], death_ball_final[,-1])
overview <- as.data.frame(t(overview))
overview$Category <- row.names(overview)
row.names(overview) <- NULL
colnames(overview) <- c("Women", "Men", "Category")
overview <- overview[-1,]
