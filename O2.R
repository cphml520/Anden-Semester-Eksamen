####################################################
####Opgave 2.1 – Expected Goals Model (xG model)####
####################################################
library(tidyverse)
library(stringr)
library(caret)
library(pROC)
library(xgboost)
library(Matrix)

###########################################
####Sammensætning af eventdata & player####
###########################################

matchevents_carry <- read.csv("Data/matchevents_carry_sl.csv")
matchevents_common <- read.csv("Data/matchevents_common_sl.csv")
matchevents_groundduel <- read.csv("Data/matchevents_groundduel_sl.csv")
matchevents_infractions <- read.csv("Data/matchevents_infractions_sl.csv")
matchevents_passes <- read.csv("Data/matchevents_passes_sl.csv")
matchevents_possessiontypes <- read.csv("Data/matchevents_possessiontypes_sl.csv")
matchevents_secondarytype <- read.csv("Data/matchevents_secondarytype_sl.csv")
matchevents_shots <- read.csv("Data/matchevents_shots_sl.csv")
players <- read.csv("Data/players_sl.csv")

event_dfs <- list(matchevents_carry, matchevents_groundduel, matchevents_infractions, 
                  matchevents_passes, matchevents_possessiontypes, 
                  matchevents_secondarytype, matchevents_shots)

Events <- matchevents_common
for (df in event_dfs) {
  Events <- full_join(Events, df, by = c("COMPETITION_WYID", "MATCH_WYID", "EVENT_WYID", "PRIMARYTYPE"))
}
Events <- Events %>% filter(COMPETITION_WYID == 335 & SEASON_WYID == 188945)
Events <- left_join(Events, players, by = c("PLAYER_WYID", "COMPETITION_WYID", "SEASON_WYID"))

####################################################
####Sammensætning af teamdata & base matchdetail####
####################################################

matchdetail_base <- read.csv("Data/matchdetail_base_sl.csv")
teammatches <- read.csv("Data/teammatches_sl.csv")
teams <- read.csv("Data/teams_sl.csv")
teammatches <- teammatches %>% filter(SEASON_WYID == 188945, COMPETITION_WYID == 335)
teams <- teams %>% filter(SEASON_WYID == 188945, COMPETITION_WYID == 335)

matches_merged <- teammatches
matches_merged <- merge(matches_merged, teams[, c("TEAM_WYID", "TEAMNAME")], by = "TEAM_WYID", all.x = TRUE)
matches_merged <- merge(matches_merged, matchdetail_base, by = c("COMPETITION_WYID", "MATCH_WYID", "TEAM_WYID"), all.x = TRUE)

home_matches <- matches_merged %>%
  filter(SIDE == "home") %>%
  select(MATCH_WYID, HOMETEAMNAME = TEAMNAME, HOMESCORE = SCORE, HOMESCOREHT = SCOREHT)
away_matches <- matches_merged %>%
  filter(SIDE == "away") %>%
  select(MATCH_WYID, AWAYTEAMNAME = TEAMNAME, AWAYSCORE = SCORE, AWAYSCOREHT = SCOREHT)
match_info <- matches_merged %>%
  select(MATCH_WYID, COMPETITION_WYID, SEASON_WYID, DATE, STATUS, ROUND_WYID, GAMEWEEK) %>%
  distinct()
Matches <- home_matches %>%
  inner_join(away_matches, by = "MATCH_WYID") %>%
  inner_join(match_info, by = "MATCH_WYID")

##########################################
####Data Preparation af Type Variabler####
##########################################

# Filtrer skud-begivenheder
Shots <- Events %>% filter(PRIMARYTYPE == "shot")
# Fjern irrelevante kolonner
Shots <- Shots[,-c(9:10,26:48,75:76,79,84:85,87:88,90:94)]

#######################
####Possessiontypes####
#######################
Shots <- Shots %>%
  mutate(POSSESSIONTYPE = pmap(list(POSSESSIONTYPE1, POSSESSIONTYPE2, POSSESSIONTYPE3, POSSESSIONTYPE4, POSSESSIONTYPE5), 
                               function(...) unique(na.omit(c(...))))) 
unique_possessiontypes <- unique(unlist(Shots$POSSESSIONTYPE))
Shots <- Shots %>%
  mutate(POSSESSIONTYPE_str = map_chr(POSSESSIONTYPE, ~ paste(.x, collapse = ","))) %>%
  replace_na(list(POSSESSIONTYPE_str = ""))
for (possession in unique_possessiontypes) {
  Shots[[possession]] <- str_detect(Shots$POSSESSIONTYPE_str, fixed(possession))
}
Shots <- Shots[,-c(24:28,58:59,61)]

######################
####Secondarytypes####
######################
Shots <- Shots %>%
  mutate(SECONDARYTYPE = pmap(list(SECONDARYTYPE1, SECONDARYTYPE2, SECONDARYTYPE3, SECONDARYTYPE4, SECONDARYTYPE5,
                                   SECONDARYTYPE6, SECONDARYTYPE7, SECONDARYTYPE8, SECONDARYTYPE9, SECONDARYTYPE10), 
                              function(...) unique(na.omit(c(...)))))
unique_secondarytypes <- unique(unlist(Shots$SECONDARYTYPE))
Shots <- Shots %>%
  mutate(SECONDARYTYPE_str = map_chr(SECONDARYTYPE, ~ paste(.x, collapse = ","))) %>%
  replace_na(list(SECONDARYTYPE_str = ""))
for (secondary in unique_secondarytypes) {
  Shots[[secondary]] <- str_detect(Shots$SECONDARYTYPE_str, fixed(secondary))
}
Shots <- Shots[,-c(29:38,62:63,66)]

################################################
####Konverter variabler til korrekte klasser####
################################################
Shots <- Shots %>%
  mutate(across(c(ATTACKWITHSHOT, ATTACKWITHSHOTONGOAL, ATTACKWITHGOAL, 
                  SHOTISGOAL, SHOTONTARGET), as.logical))

#############################################################################
####Forklarende variable, forklaring og grafer (kun for data-engineering)####
#############################################################################
# 1. Skudafstand og vinkel
Shots <- Shots %>%
  mutate(
    SHOTDISTANCE = sqrt((100 - LOCATIONX)^2 + (50 - LOCATIONY)^2)
  )
Shots <- Shots %>% 
  mutate(
    Angle_stolpe_1 = atan2(45 - LOCATIONY, 100 - LOCATIONX) * (180 / pi),
    Angle_stolpe_2 = atan2(55 - LOCATIONY, 100 - LOCATIONX) * (180 / pi),
    SHOTANGLE =  abs(Angle_stolpe_1 - Angle_stolpe_2)
  )
# 2. Tidspunkt i kampen
Shots <- Shots %>%
  mutate(
    MATCHTIME_MIN = MINUTE + (SECOND / 60),
    LATEGAME = ifelse(MINUTE > 75, 1, 0)
  )
# 3. Angrebskarakteristik
Shots <- Shots %>%
  mutate(
    FASTATTACK = ifelse(POSSESSIONDURATION < 5, 1, 0),
    NUMBEROFPASSES = POSSESSIONEVENTSNUMBER
  )
# 4. Skudtype
Shots <- Shots %>%
  mutate(
    SHOTBODYPART_CAT = ifelse(SHOTBODYPART == "head_or_other", 1, 
                              ifelse(SHOTBODYPART %in% c("left_foot", "right_foot"), 0, NA))
  )
# 5. Skudposition og forsvarspres
Shots <- Shots %>%
  mutate(
    SHOTFROMCENTER = ifelse(abs(50 - LOCATIONY) < 10, 1, 0),
    DEFENSEPRESSURE = ifelse(shot_block == 1 | interception == 1, 1, 0)
  )
# 6. Spillerkarakteristika
Shots <- Shots %>%
  group_by(PLAYER_WYID) %>%
  mutate(PLAYEREXPERIENCE = ifelse(n() > 30, 1, 0)) %>%
  ungroup()
Shots <- Shots %>%
  mutate(
    STRONGFOOTSHOT = ifelse(FOOT == "both" | FOOT == "left" | FOOT == "right", 1, 0)
  )
# 7. Besiddelse
Shots <- Shots %>%
  mutate(HIGH_EVENT_POSSESSION = ifelse(POSSESSIONEVENTSNUMBER > median(POSSESSIONEVENTSNUMBER, na.rm = TRUE), TRUE, FALSE)) %>%
  mutate(LONG_POSSESSION = ifelse(POSSESSIONDURATION > median(POSSESSIONDURATION, na.rm = TRUE), TRUE, FALSE))

#########################################################
####Dataframe til beregning af xG og Machine Learning####
#########################################################
# Konverter logiske variabler til faktorer med 0/1 som niveauer
Shots <- Shots %>%
  mutate(across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE), labels = c(0, 1))))
Shots <- Shots %>%
  mutate(across(c(LATEGAME, FASTATTACK, SHOTBODYPART_CAT, SHOTFROMCENTER, 
                  DEFENSEPRESSURE, PLAYEREXPERIENCE, STRONGFOOTSHOT), 
                as.factor))

# Udvælg variable til xG-modellens beregning
Shots_xG <- Shots %>% 
  select(SHOTISGOAL, SHOTDISTANCE, SHOTANGLE, SHOTBODYPART_CAT, LATEGAME, FASTATTACK, NUMBEROFPASSES, 
         SHOTFROMCENTER, DEFENSEPRESSURE, PLAYEREXPERIENCE, STRONGFOOTSHOT, shot_after_corner, 
         shot_after_free_kick, shot_after_throw_in, opportunity, LONG_POSSESSION, counterattack, touch_in_box)

##############################################################
####Opdeling i trænings- og testdata for skud (xG model)####
##############################################################
set.seed(123)
trainIndex <- createDataPartition(Shots_xG$SHOTISGOAL, p = 0.8, list = FALSE)
train_data <- Shots_xG[trainIndex, ]
test_data <- Shots_xG[-trainIndex, ]

###############
####XGBoost####
###############
# Konverter responsvariablen til numerisk
train_data_xgb <- train_data
test_data_xgb <- test_data

train_data_xgb$SHOTISGOAL <- as.numeric(as.character(train_data_xgb$SHOTISGOAL))
test_data_xgb$SHOTISGOAL <- as.numeric(as.character(test_data_xgb$SHOTISGOAL))

# Udarbejd modelmatrix - fjern interceptet
train_matrix_xgb <- model.matrix(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + 
                                   PLAYEREXPERIENCE + shot_after_corner + opportunity + counterattack, 
                                 data = train_data_xgb)[, -1]
test_matrix_xgb <- model.matrix(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + 
                                  PLAYEREXPERIENCE + shot_after_corner + opportunity + counterattack, 
                                data = test_data_xgb)[, -1]

dtrain_xgb <- xgb.DMatrix(data = train_matrix_xgb, label = train_data_xgb$SHOTISGOAL)
dtest_xgb <- xgb.DMatrix(data = test_matrix_xgb, label = test_data_xgb$SHOTISGOAL)

# Træn xGBoost model
xgb_model <- xgboost(data = dtrain_xgb,
                     max.depth = 3,
                     eta = 0.1,
                     nrounds = 100,
                     objective = "binary:logistic",
                     verbose = 0)



###############################
### Udarbejdelse af xP-model###
###############################
teams_df <- distinct(teams[,c(3,5)], TEAM_WYID, .keep_all = TRUE)
shots_df <- left_join(Shots, teams_df, by = "TEAM_WYID")

View(Shots)

shots_xG <- shots_df %>% 
  group_by(MATCH_WYID, OFFICIALNAME) %>% 
  summarise(
    Antal_skud = sum(PRIMARYTYPE == "shot", na.rm = TRUE),
    sum_xG = sum(SHOTXG, na.rm = TRUE)
  ) %>% 
  select(MATCH_WYID, OFFICIALNAME, Antal_skud, sum_xG)  # Ret navnet her

shots_xG_structured <- shots_xG %>%
  group_by(MATCH_WYID) %>%
  arrange(desc(sum_xG)) %>%
  summarise(
    Hometeam = first(OFFICIALNAME),
    Awayteam = last(OFFICIALNAME),
    Hometeam_skud = first(Antal_skud),
    Awayteam_skud = last(Antal_skud),
    Hometeam_xG = first(sum_xG),
    Awayteam_xG = last(sum_xG),
    .groups = 'drop'
  )


### Nu begynder vi på xP-modellen
# Først simuleres alle kampene

set.seed(123)
sim_runs <- 100000

xp_resultater <- shots_xG_structured %>%
  rowwise() %>%
  mutate(
    hjemme_mål = list(rpois(sim_runs, Hometeam_xG)),  # Simuler mål for hjemmehold
    ude_mål = list(rpois(sim_runs, Awayteam_xG)),    # Simuler mål for udehold
    
    # Sandsynligheder for kampens udfald
    P_hjemme_vinder = mean(unlist(hjemme_mål) > unlist(ude_mål)),
    P_uafgjort = mean(unlist(hjemme_mål) == unlist(ude_mål)),
    P_ude_vinder = mean(unlist(hjemme_mål) < unlist(ude_mål)),
    
    # Expected Points beregnet korrekt vha. vægtede sandsynligheder
    xP_Hjemme = 3 * P_hjemme_vinder + 1 * P_uafgjort,
    xP_Ude = 3 * P_ude_vinder + 1 * P_uafgjort
  ) %>%
  select(MATCH_WYID, Hometeam, Awayteam, xP_Hjemme, xP_Ude)

# Omregning til samlet tabel for Wyscout xG
xp_tabel <- xp_resultater %>%
  pivot_longer(cols = c(xP_Hjemme, xP_Ude), names_to = "Type", values_to = "xP") %>%
  mutate(Holdnavn = ifelse(Type == "xP_Hjemme", Hometeam, Awayteam)) %>%
  group_by(Holdnavn) %>%
  summarise(Total_xP = sum(xP)) %>%
  arrange(desc(Total_xP))

xp_tabel$Total_xP = round(xp_tabel$Total_xP,0)
print(xp_tabel)

## Nu gentages ovenstående med vores egen xG
# Først opretter vi en kolonne, med vores xG - dette gøres vha. predict
# Strukturen er pt. lidt fucked, da vi tager udgangspunkt i Mo's data engineering

Shots_matrix <- model.matrix(~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + 
                               PLAYEREXPERIENCE + shot_after_corner + opportunity + counterattack
                             , data = Shots)[, -1]

Shots_dmatrix <- xgb.DMatrix(data = Shots_matrix)

Shots$pred_xG <- predict(xgb_model, newdata = Shots_dmatrix)

Shots <- left_join(Shots, teams_df, by = "TEAM_WYID")

shots_xG_pred <- Shots %>% 
  group_by(MATCH_WYID, OFFICIALNAME) %>% 
  summarise(
    Antal_skud = sum(PRIMARYTYPE == "shot", na.rm = TRUE),
    sum_xG = sum(pred_xG, na.rm = TRUE)
  ) %>% 
  select(MATCH_WYID, OFFICIALNAME, Antal_skud, sum_xG)  # Ret navnet her

shots_pred_xG_structured <- shots_xG_pred %>%
  group_by(MATCH_WYID) %>%
  arrange(desc(sum_xG)) %>%
  summarise(
    Hometeam = first(OFFICIALNAME),
    Awayteam = last(OFFICIALNAME),
    Hometeam_skud = first(Antal_skud),
    Awayteam_skud = last(Antal_skud),
    Hometeam_xG = first(sum_xG),
    Awayteam_xG = last(sum_xG),
    .groups = 'drop'
  )


### Nu begynder vi på xP-modellen
# Først simuleres alle kampene

set.seed(123)
sim_runs <- 100000

xp_resultater_pred <- shots_pred_xG_structured %>%
  rowwise() %>%
  mutate(
    hjemme_mål = list(rpois(sim_runs, Hometeam_xG)),  # Simuler mål for hjemmehold
    ude_mål = list(rpois(sim_runs, Awayteam_xG)),    # Simuler mål for udehold
    
    # Sandsynligheder for kampens udfald
    P_hjemme_vinder = mean(unlist(hjemme_mål) > unlist(ude_mål)),
    P_uafgjort = mean(unlist(hjemme_mål) == unlist(ude_mål)),
    P_ude_vinder = mean(unlist(hjemme_mål) < unlist(ude_mål)),
    
    # Expected Points beregnet korrekt vha. vægtede sandsynligheder
    xP_Hjemme = 3 * P_hjemme_vinder + 1 * P_uafgjort,
    xP_Ude = 3 * P_ude_vinder + 1 * P_uafgjort
  ) %>%
  select(MATCH_WYID, Hometeam, Awayteam, xP_Hjemme, xP_Ude)

# Omregning til samlet tabel for Wyscout xG
xp_tabel_pred <- xp_resultater_pred %>%
  pivot_longer(cols = c(xP_Hjemme, xP_Ude), names_to = "Type", values_to = "xP") %>%
  mutate(Holdnavn = ifelse(Type == "xP_Hjemme", Hometeam, Awayteam)) %>%
  group_by(Holdnavn) %>%
  summarise(Total_xP_prediction = sum(xP)) %>%
  arrange(desc(Total_xP_prediction))

xp_tabel_pred$Total_xP_prediction = round(xp_tabel_pred$Total_xP_prediction,0)
print(xp_tabel_pred)

# Nu tilføjer vi resultaterne for superligaen
superliga_resultater <- data.frame(
  Holdnavn = c("FC Midtjylland", "Brøndby IF", "FC København", "FC Nordsjælland", "AGF",
               "Silkeborg IF", "Randers FC", "Viborg FF", "Vejle Boldklub","Lyngby Boldklub", "Odense BK", "Hvidovre IF"),
  point = c(63, 62, 59, 58, 44, 36, 41, 40, 36, 36, 32, 20)
)

xp_tabel <- left_join(xp_tabel, superliga_resultater, by = "Holdnavn")
xp_tabel <- left_join(xp_tabel, xp_tabel_pred, by = "Holdnavn")
print(xp_tabel)
billeder <- teams[,c(5,11)]

colnames(billeder) = c("Holdnavn","Billeder")

xp_tabel <- left_join(xp_tabel, billeder, by = "Holdnavn")
print(xp_tabel)
xp_tabel <- xp_tabel[,c(1,5,3,2,4)]

# Tilføj logo til holdnavn
xp_tabel <- xp_tabel %>%
  mutate(Hold_med_logo = paste0(
    "<img src='", Billeder, "' height='30' style='vertical-align:middle;'> ",
    Holdnavn
  ))

# Beregn procentændring
xp_tabel <- xp_tabel %>%
  mutate(Procentændring_wyscout_xP = round(((Total_xP - point) / point) * 100, 0),
         Procentændring_vores_xP = round(((Total_xP_prediction - point) / point) * 100, 0)) %>%
  mutate(
    Justeret_xP_wyscout = paste0(
      Total_xP,
      "<sup>",
      case_when(
        Procentændring_wyscout_xP > 0 ~ paste0("<span style='color:green;'>+", Procentændring_wyscout_xP, "%</span>"),
        Procentændring_wyscout_xP < 0 ~ paste0("<span style='color:red;'>", Procentændring_wyscout_xP, "%</span>"),
        TRUE ~ paste0("<span style='color:gray;'>0%</span>")
      ),
      "</sup>"
    ),
    Justeret_xP_vores = paste0(
      Total_xP_prediction,
      "<sup>",
      case_when(
        Procentændring_vores_xP > 0 ~ paste0("<span style='color:green;'>+", Procentændring_vores_xP, "%</span>"),
        Procentændring_vores_xP < 0 ~ paste0("<span style='color:red;'>", Procentændring_vores_xP, "%</span>"),
        TRUE ~ paste0("<span style='color:gray;'>0%</span>")
      ),
      "</sup>"
    )
  )

# Holdene sættes i korrekt rækkefølge i forhold til Mesterskabsspillet og Kvalifikationsspillet (se kolonne point)
xp_tabel <- xp_tabel[c(4,3,1,2,6,7,5,8,11,9,10,12),]

# Opret tabellen
gt_table <- xp_tabel %>%
  select(Hold_med_logo, point, Justeret_xP_wyscout, Justeret_xP_vores) %>%
  gt() %>%
  fmt_markdown(columns = c(Hold_med_logo, Justeret_xP_wyscout, Justeret_xP_vores)) %>%
  cols_label(
    Hold_med_logo = "Hold",
    point = "Point",
    Justeret_xP_wyscout = "Wyscout xP",
    Justeret_xP_vores = "Vores xP"
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
    table.width = pct(70),
    data_row.padding = px(6)
  ) %>%
  cols_align(
    align = "center",
    columns = c(point, Justeret_xP_wyscout, Justeret_xP_vores)
  ) %>%
  # Zebra-striber: lysegrå baggrund på hver anden række
  tab_style(
    style = cell_fill(color = "#f7f7f7"),
    locations = cells_body(rows = seq(2, nrow(xp_tabel), 2))
  ) %>%
  # Kolonneoverskrifter i fed
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Fed sort streg under række 6 (efter Mesterskabsspillet)
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3)
    ),
    locations = cells_body(rows = 6)
  )

gt_table

xp_tabel <- xp_tabel %>%
  mutate(
    abs_error_wyscout = abs(point - Total_xP),
    abs_error_vores = abs(point - Total_xP_prediction)
  )

# Beregn MAE (Mean Absolute Error)
mae_wyscout <- mean(xp_tabel$abs_error_wyscout)
mae_vores <- mean(xp_tabel$abs_error_vores)
print(mae_wyscout)
print(mae_vores)

# Forklaringsgrad
model_wyscout <- lm(point ~ Total_xP, data = xp_tabel)
model_vores <- lm(point ~ Total_xP_prediction, data = xp_tabel)

summary(model_wyscout)$r.squared
summary(model_vores)$r.squared

###################################################
### Opgave 2.2 - Validering af data for xP Model###
###################################################

Brøndby_kampe_df <- Matches %>% filter(HOMETEAMNAME == "Brøndby") %>% 
  arrange(DATE) %>% distinct(MATCH_WYID, .keep_all = TRUE)

# Vi har fået til opgave af at filtrere efter 3 forskellige Brøndby IF kampe.
# Kampene kan vi ikke finde uden at gå udenom en betalingsmur, hvorfor vi har valgt 3 andre kampe:
# 1. Brøndby IF vs. Lyngby 3-0 (MatchID == 5465938)
# 2. Brøndby IF vs. Hvidovre 4-0 (MatchID == 5466013)
# 3. Brøndby IF vs. Kobenhavn 2-3 (MatchID == 5465965)


# 1. Brøndby IF vs. Lyngby 3-0 (MatchID == 5465938)

Brøndby_lyngby_shots <- shots_df %>% filter(MATCH_WYID == 5465938) %>% 
  select(MINUTE, SECOND, PRIMARYTYPE, SHOTONTARGET,SHOTISGOAL, SHORTNAME, OFFICIALNAME)

Brøndby_lyngby_shots_agg <- Brøndby_lyngby_shots %>% group_by(OFFICIALNAME) %>% 
  summarise(
    shots = sum(PRIMARYTYPE == "shot", na.rm = TRUE),
    shot_on_target = sum(SHOTONTARGET == TRUE),
    Mål = sum(SHOTISGOAL == T)
  ) %>%  select(OFFICIALNAME, shots, shot_on_target, Mål)

# 2. Brøndby IF vs. Hvidovre 4-0 (MatchID == 5466013)

Brøndby_hvidovre_shots <- shots_df %>% filter(MATCH_WYID == 5466013) %>% 
  select(MINUTE, SECOND, PRIMARYTYPE, SHOTONTARGET,SHOTISGOAL, SHORTNAME, OFFICIALNAME)

Brøndby_hvidovre_shots_agg <- Brøndby_hvidovre_shots %>% group_by(OFFICIALNAME) %>% 
  summarise(
    shots = sum(PRIMARYTYPE == "shot", na.rm = TRUE),
    shot_on_target = sum(SHOTONTARGET == TRUE),
    Mål = sum(SHOTISGOAL == T)
  ) %>%  select(OFFICIALNAME, shots, shot_on_target, Mål)

# 3. Brøndby IF vs. Kobenhavn 2-3 (MatchID == 5465965)

Brøndby_københavn_shots <- shots_df %>% filter(MATCH_WYID == 5465965) %>% 
  select(MINUTE, SECOND, PRIMARYTYPE, SHOTONTARGET,SHOTISGOAL, SHORTNAME, OFFICIALNAME)

Brøndby_københavn_shots_agg <- Brøndby_københavn_shots %>% group_by(OFFICIALNAME) %>% 
  summarise(
    shots = sum(PRIMARYTYPE == "shot", na.rm = TRUE),
    shot_on_target = sum(SHOTONTARGET == TRUE),
    Mål = sum(SHOTISGOAL == T)
  ) %>%  select(OFFICIALNAME, shots, shot_on_target, Mål)

# Dataene eksporteres til excel, hvor skuddene valideres der.
write_xlsx(Brøndby_lyngby_shots, "/Users/andreasfussinghansen/Dokumenter/Semester_2/Flow_4/Eksamen/Brøndby_lyngby.xlsx")
write_xlsx(Brøndby_hvidovre_shots, "/Users/andreasfussinghansen/Dokumenter/Semester_2/Flow_4/Eksamen/Brøndby_hvidovre.xlsx")
write_xlsx(Brøndby_københavn_shots, "/Users/andreasfussinghansen/Dokumenter/Semester_2/Flow_4/Eksamen/Brøndby_københavn.xlsx")
