library(jsonlite)
library(tidyverse)
library(mongolite)
library(tidyr) 
library(glue)
#### Opgave 6 - Videnskabsteori ####
# Starter ud med noget beskrivende statistik (top 10 blandt kvinder)

all_matches_mongo <- mongo(
  db = "Statsbomb",
  collection = "matches"
)

all_events_mongo <- mongo(
  collection = "events",
  db = "Statsbomb"
)

all_events <- readRDS("Kvinde_data")
all_matches <- all_matches_mongo$find()
all_matches <- fromJSON(toJSON(all_matches), flatten = TRUE) %>%
  as.data.frame()
sum(all_events$type.name == "Pass", na.rm = TRUE)
sum(all_events$type.name == "Shot", na.rm = TRUE)
sum(all_events$shot.outcome.name == "Goal", na.rm = TRUE)

unique(all_matches$competition.competition_name)
#all_events <- as.data.frame(flatten(all_events_mongo$find())) # All kvindelige event data

# Nu sorteres der for kvindelige kampe, og der fokuseres udelukkende på VM
female_matches <- all_matches %>% filter(home_team.home_team_gender == "female" & away_team.away_team_gender == "female")

female_matches_agg <- female_matches %>% group_by(competition.competition_name, season.season_name) %>% 
  summarise(
    antal_kampe = n_distinct(match_id)
  ) %>% 
  arrange(desc(antal_kampe))

View(female_matches_agg)


# Vi fokuserer på FA Women's Super League 2020/2021 og udelukkende de danske kampe

world_cup_df <- female_matches %>% 
  filter(competition.competition_name == "Women's World Cup", 
         season.season_name == 2023)

world_cup_agg <- world_cup_df %>%
  group_by(home_team.home_team_name) %>% 
  summarise(
    antal_kampe = n_distinct(match_id)
  ) %>% 
  arrange(desc(antal_kampe))


world_cup_vl <- world_cup_df$match_id

# Vi filtrere nu for ovenstående Match_id's i event dataene

world_cup_event_df <- all_events %>% filter(match_id %in% world_cup_vl)

world_cup_event_df_agg <- world_cup_event_df %>% group_by(player.name) %>% 
  summarise(
    pass = sum(type.name == "Pass", na.rm = TRUE)/n_distinct(match_id),
    pass_accuracy = sum(pass.outcome.name == "incomplete", na.rm = TRUE)/pass*100,
    Goals = sum(shot.outcome.name == "Goal", na.rm = TRUE)/n_distinct(match_id),
    shots = sum(type.name == "Shot", na.rm = TRUE)/n_distinct(match_id),
    xG_sum = sum(shot.statsbomb_xg, na.rm = TRUE)/n_distinct(match_id),
    xG_mean = xG_sum/shots
  ) %>% arrange(desc(pass))

### Vedkommende vi har valgt at fokusere på er Alessia Russo - anfører for England ved VM 2023.


world_cup_event_df$team.name = gsub(" Women's", "", world_cup_event_df$team.name)

world_cup_event_df <- world_cup_event_df %>% 
  mutate(
    SHOT.ISGOAL = ifelse(shot.outcome.name == "Goal", TRUE, FALSE)
  )

shot_world_cup_agg <- world_cup_event_df %>% group_by(player.name) %>% 
  summarise(
    land = first(team.name),
    shot = sum(type.name == "Shot", na.rm = TRUE),
    xG = sum(shot.statsbomb_xg, na.rm = TRUE)/shot,
    Goals = sum(SHOT.ISGOAL == TRUE, na.rm = TRUE)
  ) %>% arrange(desc(shot)) %>% 
  slice_head(n = 10)

unique(shot_world_cup_agg$land)

View(shot_world_cup_agg)


flag_links <- tibble::tibble(
  land = c("England", "Spain", "France", "Netherlands", "South Africa", "United States"),
  iso = c("gb", "es", "fr", "nl", "za", "us"),
  flag_url = paste0("https://flagcdn.com/w40/", iso, ".png")
)

# 2. Data: Shot stats + merge med flag
shot_world_cup_agg <- world_cup_event_df %>%
  group_by(player.name) %>%
  summarise(
    land = first(team.name),
    shot = sum(type.name == "Shot", na.rm = TRUE),
    xG = sum(shot.statsbomb_xg, na.rm = TRUE)/shot,
    Goals = sum(shot.outcome.name == "Goal", na.rm = TRUE)
  ) %>%
  arrange(desc(shot)) %>%
  slice_head(n = 10) %>%
  left_join(flag_links, by = "land") %>%
  mutate(
    Spiller_med_flag = glue::glue(
      "<img src='{flag_url}' height='20' style='vertical-align:middle;'> {player.name}"
    )
  )

shot_world_cup_agg$Nr = 1:10

# 3. GT tabel med stil som i dit eksempel
gt_table <- shot_world_cup_agg %>%
  mutate(
    Spiller_med_flag = glue::glue(
      "<img src='{flag_url}' height='16' style='vertical-align:middle;'> {player.name}"
    )
  ) %>%
  select(Nr, Spiller_med_flag, shot, xG, Goals) %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  fmt_number(columns = xG, decimals = 3) %>%
  cols_label(
    Spiller_med_flag = "Spiller",
    shot = "Skud",
    xG = "xG/Skud",
    Goals = "Mål"
  ) %>%
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.border.top.style = "none",
    table_body.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.font.weight = "normal",
    table.font.size = px(12),
    table.width = pct(65),
    data_row.padding = px(6)
  ) %>%
  cols_align(
    align = "left",  # 👈 venstrestil spiller + flag
    columns = Spiller_med_flag
  ) %>%
  cols_align(
    align = "center",
    columns = c(shot, Goals)  # resten er centreret
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7f7f7"),
    locations = cells_body(rows = seq(2, nrow(shot_world_cup_agg), 2))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gt_table



# Opgave 6.1 - stabillitet i præstationen.
# Vi skal sammenholde spillernes gennemsnitlig xG for en kamp med den gennemsnitlige xG for hele sæsonen.
# Herpå skal vi måle Std. afvigelsen.
# Vi vælger kun at bryge Alessia Russo

world_cup_event_df <- world_cup_event_df %>%
  filter(type.name == "Shot") %>% 
  group_by(player.name) %>%
  mutate(
    PLAYEREXPERIENCE = ifelse(n() > 10, 1, 0),
    SHOT.ISGOAL = as.factor(SHOT.ISGOAL)
  ) %>%
  ungroup()

# Step 2: Træn modellen
model <- glm(SHOT.ISGOAL ~ shot.statsbomb_xg + PLAYEREXPERIENCE, 
             data = world_cup_event_df, 
             family = binomial())

summary(model)
# Step 3: Lav prediktion på skudniveau
world_cup_event_df <- world_cup_event_df %>%
  mutate(
    predicted_prob = predict(model, newdata = ., type = "response")
  )

# Step 4: Aggreger på spillerniveau
world_cup_xG_justeret <- world_cup_event_df %>%
  group_by(player.name) %>%
  summarise(
    land = first(team.name),
    shot = n(),  # allerede filtreret på 'Shot'
    xG = mean(shot.statsbomb_xg, na.rm = TRUE),
    justeret_xG = mean(predicted_prob, na.rm = TRUE)
  ) %>%
  arrange(desc(shot)) %>%
  slice_head(n = 10) %>%
  left_join(flag_links, by = "land") %>%
  mutate(
    Spiller_med_flag = glue::glue(
      "<img src='{flag_url}' height='20' style='vertical-align:middle;'> {player.name}"
    ),
    Nr = 1:10
  )

gt_table <- world_cup_xG_justeret %>%
  mutate(
    Spiller_med_flag = glue::glue(
      "<img src='{flag_url}' height='16' style='vertical-align:middle;'> {player.name}"
    )
  ) %>%
  select(Nr, Spiller_med_flag, shot, xG, justeret_xG) %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  fmt_number(columns = c(xG, justeret_xG), decimals = 3) %>%
  cols_label(
    Spiller_med_flag = "Spiller",
    shot = "Skud",
    xG = "xG/Skud",
    justeret_xG = "Justeret xG"
  ) %>%
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.border.top.style = "none",
    table_body.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.font.weight = "normal",
    table.font.size = px(12),
    table.width = pct(65),
    data_row.padding = px(6)
  ) %>%
  cols_align(
    align = "left",  # 👈 venstrestil spiller + flag
    columns = Spiller_med_flag
  ) %>%
  cols_align(
    align = "center",
    columns = shot  # resten er centreret
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7f7f7"),
    locations = cells_body(rows = seq(2, nrow(shot_world_cup_agg), 2))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gt_table
