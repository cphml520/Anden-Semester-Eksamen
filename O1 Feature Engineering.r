##################################################
####Opgave 1 – Expected Goals Model (xG model)####
##################################################
library(tidyverse)
library(stringr)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(Matrix)
###########################################
####Sammensætning af eventdata & player####
###########################################

matchevents_carry <- read.csv("Data/matchevents_carry.csv")
matchevents_common <- read.csv("Data/matchevents_common.csv")
matchevents_groundduel <- read.csv("Data/matchevents_groundduel.csv")
matchevents_infractions <- read.csv("Data/matchevents_infractions.csv")
matchevents_passes <- read.csv("Data/matchevents_passes.csv")
matchevents_possessiontypes <- read.csv("Data/matchevents_possessiontypes.csv")
matchevents_secondarytype <- read.csv("Data/matchevents_secondarytype.csv")
matchevents_shots <- read.csv("Data/matchevents_shots.csv")
players <- read.csv("Data/players.csv")

event_dfs <- list(matchevents_carry, matchevents_groundduel, matchevents_infractions, 
                  matchevents_passes, matchevents_possessiontypes, 
                  matchevents_secondarytype, matchevents_shots)

Events <- matchevents_common

for (df in event_dfs) {
  Events <- full_join(Events, df, by = c("COMPETITION_WYID", "MATCH_WYID", "EVENT_WYID", "PRIMARYTYPE"))
}

Events_25 <- Events

Events <- Events %>% filter(COMPETITION_WYID == 335 & SEASON_WYID == 188945)

Events <- left_join(Events, players, by = c("PLAYER_WYID","COMPETITION_WYID","SEASON_WYID"))

####################################################
####Sammensætning af teamdata & base matchdetail####
####################################################

matchdetail_base <- read.csv("Data/matchdetail_base.csv")
teammatches <- read.csv("Data/teammatches.csv")
teams <- read.csv("Data/teams.csv")

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

#######################
####Possessiontypes####
#######################

Shots <- Events %>% filter(PRIMARYTYPE =="shot")

Shots <- Shots[,-c(9:10,26:48,75:76,79,84:85,87:88,90:94)]

# Opretter en liste med POSSESSIONTYPES
Shots <- Shots %>%
  mutate(POSSESSIONTYPE = pmap(list(POSSESSIONTYPE1, POSSESSIONTYPE2, POSSESSIONTYPE3, POSSESSIONTYPE4, POSSESSIONTYPE5), 
                               function(...) unique(na.omit(c(...)))))

unique_possessiontypes <- unique(unlist(Shots$POSSESSIONTYPE))

# Omdan listen til en karakterstreng (komma-separeret)
Shots <- Shots %>%
  mutate(POSSESSIONTYPE_str = map_chr(POSSESSIONTYPE, ~ paste(.x, collapse = ","))) %>%
  replace_na(list(POSSESSIONTYPE_str = ""))  # Undgå NA-problemer

# Opretter boolean-kolonner for hver unik værdi
for (possession in unique_possessiontypes) {
  Shots[[possession]] <- str_detect(Shots$POSSESSIONTYPE_str, fixed(possession))
}

# Fjern tidligere possessiontype variabler som ikke længere skal anvendes
Shots <- Shots[,-c(24:28,58:59,61)]

######################
####Secondarytypes####
######################

# Opretter en liste for Secondarytypes
Shots <- Shots %>%
  mutate(SECONDARYTYPE = pmap(list(SECONDARYTYPE1, SECONDARYTYPE2, SECONDARYTYPE3, SECONDARYTYPE4, SECONDARYTYPE5,
                                   SECONDARYTYPE6, SECONDARYTYPE7, SECONDARYTYPE8, SECONDARYTYPE9, SECONDARYTYPE10), 
                              function(...) unique(na.omit(c(...)))))


# Finder alle unikke secondarytype værdier
unique_secondarytypes <- unique(unlist(Shots$SECONDARYTYPE))

# Liste omdannes til karakterstreng
Shots <- Shots %>%
  mutate(SECONDARYTYPE_str = map_chr(SECONDARYTYPE, ~ paste(.x, collapse = ","))) %>%
  replace_na(list(SECONDARYTYPE_str = ""))  # Undgå NA-problemer

# Oprettelse af boolean-kolonner for hver unik secondarytype
for (secondary in unique_secondarytypes) {
  Shots[[secondary]] <- str_detect(Shots$SECONDARYTYPE_str, fixed(secondary))
}

# Fjern tidligere secondarytype variabler som ikke længere skal anvendes
Shots <- Shots[,-c(29:38,62:63,66)]

################################################
####Konverter variabler til korrekte klasser####
################################################

Shots <- Shots %>%
  mutate(across(c(ATTACKWITHSHOT, ATTACKWITHSHOTONGOAL, ATTACKWITHGOAL, 
                  SHOTISGOAL, SHOTONTARGET), as.logical))

######################################################################################################
####Opgave 1.2 – Forklarende variable, forklaring og grafiske illustrationer beskrivende statistik####
######################################################################################################

# 1. Skudafstand og vinkel
Shots <- Shots %>%
  mutate(
    SHOTDISTANCE = sqrt((100 - LOCATIONX)^2 + (50 - LOCATIONY)^2),
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
    STRONGFOOTSHOT = ifelse(FOOT == "both" |
                              (FOOT == "left") | 
                              (FOOT == "right"), 1, 0)
  )

# 7. Besiddelse
Shots <- Shots %>%
  mutate(HIGH_EVENT_POSSESSION = ifelse(POSSESSIONEVENTSNUMBER > median(POSSESSIONEVENTSNUMBER, na.rm = TRUE), TRUE, FALSE))

Shots <- Shots %>%
  mutate(LONG_POSSESSION = ifelse(POSSESSIONDURATION > median(POSSESSIONDURATION, na.rm = TRUE), TRUE, FALSE))

#########################################################
####Dataframe til beregning af xG og Machine Learning####
#########################################################

Shots <- Shots %>%
  mutate(across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE), labels = c(0, 1))))

Shots <- Shots %>%
  mutate(across(c(LATEGAME, FASTATTACK, SHOTBODYPART_CAT, SHOTFROMCENTER, 
                  DEFENSEPRESSURE, PLAYEREXPERIENCE, STRONGFOOTSHOT), 
                as.factor))

Shots_xG <- Shots

Shots_xG <- Shots_xG %>% select(SHOTISGOAL, SHOTDISTANCE, SHOTANGLE, SHOTBODYPART_CAT, LATEGAME, FASTATTACK, NUMBEROFPASSES, 
                                  SHOTFROMCENTER, DEFENSEPRESSURE, PLAYEREXPERIENCE, STRONGFOOTSHOT, shot_after_corner, shot_after_free_kick,
                                  shot_after_throw_in, opportunity, LONG_POSSESSION, counterattack, touch_in_box)

##############################################################
####Opgave 1.1 – Opdeling i trænings- og testdata for skud####
##############################################################

set.seed(123)
trainIndex <- createDataPartition(Shots_xG$SHOTISGOAL, p = 0.8, list = FALSE)
train_data <- Shots_xG[trainIndex, ]
test_data <- Shots_xG[-trainIndex, ]

############################
####Logistisk regression####
############################

Shot_glm <- glm(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + LATEGAME + FASTATTACK + NUMBEROFPASSES + 
               SHOTFROMCENTER + DEFENSEPRESSURE + PLAYEREXPERIENCE + STRONGFOOTSHOT + shot_after_corner + shot_after_free_kick +
               shot_after_throw_in + opportunity + LONG_POSSESSION + counterattack + touch_in_box, 
             data = train_data, family = binomial)

summary(Shot_glm)

Shot_glm2 <- glm(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + 
                   opportunity + counterattack, 
              data = train_data, family = binomial)

summary(Shot_glm2)

####################################
####Konfusionsmatrix & ROC kurve####
####################################

Shots_Forudsigelse <- predict(Shot_glm2, newdata = test_data, type = "response")

Threshold <- 0.4

Shots_ForudsigelseBin <- ifelse(Shots_Forudsigelse > Threshold, "Mål", "Ikke mål")

Konfusion_matrix <- table(Predicted = Shots_ForudsigelseBin, Actual = test_data$SHOTISGOAL)

print(Konfusion_matrix)

ROC_kurve <- roc(test_data$SHOTISGOAL, Shots_Forudsigelse, levels = c("0", "1"), direction = "<")

plot(ROC_kurve, col = "darkgreen", lwd = 2, main = "Den logistiske regression har fornuftige prædiktive evner, men svært ved at forudsige scoringer")

print(ROC_kurve$auc)

######################
####Beslutningstræ####
######################

Shot_tree <- rpart(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT +
                     opportunity + counterattack, 
                   data = train_data, method = "class", cp = 0.01)

tree_sandsynlighed <- predict(Shot_tree, newdata = test_data, type = "prob")[,2]

rpart.plot(Shot_tree, type = 2, extra = 104, box.palette = "Blues", 
           under = TRUE, prefix = "Goal=", 
           nn = FALSE, branch = 1, yesno = 2, main = "Skudafstand og vinkel er afgørende for mål")

#########################################################
####Konfusionsmatrix, ROC kurve & Variable Importance####
#########################################################

tree_sandsynlighed <- predict(Shot_tree, newdata = test_data, type = "prob")[, 2]  # sandsynlighed for mål (klasse 1)

tree_ForudsigelseBin <- ifelse(tree_sandsynlighed > 0.4, "Mål", "Ikke mål")

ROC_tree <- roc(test_data$SHOTISGOAL, tree_sandsynlighed)

plot(ROC_tree, col = "darkgreen", lwd = 2, main = "Beslutningstræet 'skyder' tilfældigt ")

tree_class <- predict(Shot_tree, newdata = test_data, type = "class")

Konfusionsmatrix_tree <- table(Predicted = tree_ForudsigelseBin,
                               Actual = ifelse(test_data$SHOTISGOAL == 1, "Mål", "Ikke mål"))
print(Konfusionsmatrix_tree)

importance_tree <- Shot_tree$variable.importance

importance_tree_df <- data.frame(
  Variable = names(importance_tree),
  Importance = as.numeric(importance_tree)
)

importance_tree_df <- importance_tree_df[order(importance_tree_df$Importance, decreasing = TRUE), ]

barplot(importance_tree_df$Importance,
        names.arg = importance_tree_df$Variable,
        las = 2,
        col = "steelblue",
        main = "Skuddistance- og vinkel er klart de vigtigste variabler til forudsigelse af scoring",
        ylab = "Importance",
        cex.names = 0.8)

#####################
####Random Forest####
#####################

# Find optimal mtry
best_mtry <- tuneRF(
  x = train_data[, c("SHOTDISTANCE", "SHOTANGLE", "SHOTBODYPART_CAT",
                                  "opportunity", "counterattack")],
  y = as.factor(train_data$SHOTISGOAL),
  stepFactor = 1.5,      # Hvor meget mtry øges pr. iteration
  improve = 0.01,        # Mindste forbedring i OOB-error for at fortsætte
  ntreeTry = 500,        # Antal træer pr. forsøg
  trace = TRUE,          # Vis output
  plot = TRUE            # Tegn graf
)

# Udarbejd Random Forest

Shot_randomforest <- randomForest(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT +
                                    opportunity + counterattack, 
                        data = train_data,
                        ntree = 500,        # Antal træer
                        mtry = 2,           # Antal variabler der vælges tilfældigt ved hvert split
                        importance = TRUE)  # Så vi kan se hvilke variable der er vigtigst

print(Shot_randomforest)

#########################################################
####Konfusionsmatrix, ROC kurve & Variable Importance####
#########################################################

Randomforest_sandsynlighed <- predict(Shot_randomforest, newdata = test_data, type = "prob")[, 2]
Randomforest_ForudsigelseBin <- ifelse(Randomforest_sandsynlighed > 0.4, "Mål", "Ikke mål")

Konfusionsmatrix_randomforest <- table(Predicted = Randomforest_ForudsigelseBin,
                                       Actual = ifelse(test_data$SHOTISGOAL == 1, "Mål", "Ikke mål"))

print(Konfusionsmatrix_randomforest)

Randomforest_sandsynlighed <- predict(Shot_randomforest, newdata = test_data, type = "prob")[,2]

ROC_kurve_randomforest <- roc(test_data$SHOTISGOAL, Randomforest_sandsynlighed)
plot(ROC_kurve_randomforest, col = "darkgreen", lwd = 2, main = "Random Forest modellen evner at forudsige")

varImpPlot(Shot_randomforest, main = "Skudafstand- og vinkel er klart de vigtigste variabler til forudsigelse af scoring")


importance_rf <- Shot_randomforest$importance

###############
####XGBoost####
###############

train_data_xgb <- train_data
test_data_xgb <- test_data

train_data_xgb$SHOTISGOAL <- as.numeric(as.character(train_data$SHOTISGOAL))
test_data_xgb$SHOTISGOAL <- as.numeric(as.character(test_data$SHOTISGOAL))

train_matrix_xgb <- model.matrix(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT +  
                               opportunity + counterattack, data = train_data_xgb)[, -1] # -1 fjerner intercept (skæringspunktet) fra matrixen

test_matrix_xgb <- model.matrix(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + 
                                    opportunity + counterattack, data = test_data_xgb)[, -1] # -1 fjerner intercept (skæringspunktet) fra matrixen

dtrain_xgb <- xgb.DMatrix(data = train_matrix_xgb, label = train_data_xgb$SHOTISGOAL)
dtest_xgb <- xgb.DMatrix(data = test_matrix_xgb, label = test_data_xgb$SHOTISGOAL)

xgb_model <- xgboost(data = dtrain_xgb,
                     max.depth = 3,
                     eta = 0.1,
                     nrounds = 100,
                     objective = "binary:logistic",
                     verbose = 0)

#########################################################
####Konfusionsmatrix, ROC kurve & Variable Importance####
#########################################################

forudsigelse_xgb <- predict(xgb_model, dtest_xgb)
forudsigelse_xgb_ForudsigelseBin <- ifelse(forudsigelse_xgb >= 0.4, "Mål", "Ikke mål")

Konfusionsmatrix_xgb <- table(Predicted = forudsigelse_xgb_ForudsigelseBin,
                              Actual = ifelse(test_data_xgb$SHOTISGOAL == 1, "Mål", "Ikke mål"))

print(Konfusionsmatrix_xgb)

ROC_xgb <- roc(test_data_xgb$SHOTISGOAL, forudsigelse_xgb)

plot(ROC_xgb, col = "darkgreen", lwd = 2, main = "XGBoost forudsigelsesevne er klart bedst")

# feature importance

importance_matrix <- xgb.importance(model = xgb_model)

Feature_im <- xgb.plot.importance(importance_matrix, top_n = 10, 
                    main = "XGBoost – Skudafstand er absolut vigtigst for scoring efterfulgt af 'store chancer'", 
                    rel_to_first = TRUE, 
                    xlab = "Relative Importance")

#################################
####Sammenligning af modeller####
#################################

# Sammenligning af modeller vha. ROC kurve
plot(ROC_kurve, col = "blue", lwd = 2, main = "ROC-kurve sammenligning")
lines(ROC_tree, col = "darkgreen", lwd = 2)
lines(ROC_kurve_randomforest, col = "orange", lwd = 2)
lines(ROC_xgb, col = "red", lwd = 2)
legend("bottomright",
       legend = c(
         paste("Logistisk regression (AUC =", round(auc(ROC_kurve), 3), ")"),
         paste("Beslutningstræ (AUC =", round(auc(ROC_tree), 3), ")"),
         paste("Random Forest (AUC =", round(auc(ROC_kurve_randomforest), 3), ")"),
         paste("XGBoost (AUC =", round(auc(ROC_xgb), 3), ")")
       ),
       col = c("blue", "darkgreen", "orange", "red"), lwd = 2)

# Sammenligning af modeller vha. AUC og accuracy
# Predicted probabilities
log_probs <- Shots_Forudsigelse
tree_probs <- tree_sandsynlighed
rf_probs <- Randomforest_sandsynlighed
xgb_probs <- forudsigelse_xgb

# Faktiske labels til testdata
true_labels <- test_data$SHOTISGOAL

# Predicted classes
log_pred <- ifelse(log_probs > 0.4, 1, 0)
tree_pred <- ifelse(tree_probs > 0.4, 1, 0)
rf_pred <- ifelse(rf_probs > 0.4, 1, 0)
xgb_pred <- ifelse(xgb_probs > 0.4, 1, 0)

# Confusion matrices
conf_log <- confusionMatrix(factor(log_pred, levels = c(0, 1)), factor(true_labels, levels = c(0, 1)))
conf_tree <- confusionMatrix(factor(tree_pred, levels = c(0, 1)), factor(true_labels, levels = c(0, 1)))
conf_rf <- confusionMatrix(factor(rf_pred, levels = c(0, 1)), factor(true_labels, levels = c(0, 1)))
conf_xgb <- confusionMatrix(factor(xgb_pred, levels = c(0, 1)), factor(true_labels, levels = c(0, 1)))

# ROC-kurver
roc_log <- roc(true_labels, log_probs)
roc_tree <- roc(true_labels, tree_probs)
roc_rf <- roc(true_labels, rf_probs)
ROC_xgbb <- roc(true_labels, xgb_probs)

# Samlet sammenligningstabel
model_compare <- data.frame(
  Model = c("Logistisk regression", "Beslutningstræ", "Random Forest", "XGBoost"),
  AUC = c(
    round(auc(roc_log), 3),
    round(auc(roc_tree), 3),
    round(auc(roc_rf), 3),
    round(auc(ROC_xgbb), 3)
  ),
  Accuracy = c(
    round(conf_log$overall["Accuracy"], 3),
    round(conf_tree$overall["Accuracy"], 3),
    round(conf_rf$overall["Accuracy"], 3),
    round(conf_xgb$overall["Accuracy"], 3)
  )
)

print(model_compare)

#######################
####Beregning af XG####
#######################

Shots_Forudsigelse <- predict(Shot_glm2, newdata = test_data, type = "response")
test_data$xG_glm <- round(Shots_Forudsigelse,2)

tree_sandsynlighed <- predict(Shot_tree, newdata = test_data, type = "prob")[,2]
test_data$xG_tree <- round(tree_sandsynlighed,2)

Randomforest_sandsynlighed <- predict(Shot_randomforest, newdata = test_data, type = "prob")[,2]
test_data$xG_rf <- round(Randomforest_sandsynlighed,2)

forudsigelse_xgb <- predict(xgb_model, dtest_xgb)
test_data$xG_xgb <- round(forudsigelse_xgb,2)

####################################
####Opgave 1.6 – I virkeligheden####
####################################

########################
####Dataforberedelse####
########################

Events_25 <- Events_25 %>% filter(COMPETITION_WYID == 335 & SEASON_WYID == 189918)

Events_25 <- left_join(Events_25, players, by = c("PLAYER_WYID","COMPETITION_WYID","SEASON_WYID"))

Shots_25 <- Events_25 %>% filter(PRIMARYTYPE =="shot")

#Shots_25 <- Shots_25[,-c(9:10,26:48,75:76,79,84:85,87:88,90:94)]
Shots_25 <- Shots_25[,-c(1:10,14:48,54:58,71:94)]

Shots_25 <- Shots_25 %>%
  mutate(POSSESSIONTYPE = pmap(list(POSSESSIONTYPE1, POSSESSIONTYPE2, POSSESSIONTYPE3, POSSESSIONTYPE4, POSSESSIONTYPE5), 
                               function(...) unique(na.omit(c(...)))))

unique_possessiontypes <- unique(unlist(Shots_25$POSSESSIONTYPE))

Shots_25 <- Shots_25 %>%
  mutate(POSSESSIONTYPE_str = map_chr(POSSESSIONTYPE, ~ paste(.x, collapse = ","))) %>%
  replace_na(list(POSSESSIONTYPE_str = ""))  # Undgå NA-problemer

for (possession in unique_possessiontypes) {
  Shots_25[[possession]] <- str_detect(Shots_25$POSSESSIONTYPE_str, fixed(possession))
}

Shots_25 <- Shots_25 %>%
  mutate(SECONDARYTYPE = pmap(list(SECONDARYTYPE1, SECONDARYTYPE2, SECONDARYTYPE3, SECONDARYTYPE4, SECONDARYTYPE5,
                                   SECONDARYTYPE6, SECONDARYTYPE7, SECONDARYTYPE8, SECONDARYTYPE9, SECONDARYTYPE10), 
                              function(...) unique(na.omit(c(...)))))

unique_secondarytypes <- unique(unlist(Shots_25$SECONDARYTYPE))

Shots_25 <- Shots_25 %>%
  mutate(SECONDARYTYPE_str = map_chr(SECONDARYTYPE, ~ paste(.x, collapse = ","))) %>%
  replace_na(list(SECONDARYTYPE_str = ""))  # Undgå NA-problemer

for (secondary in unique_secondarytypes) {
  Shots_25[[secondary]] <- str_detect(Shots_25$SECONDARYTYPE_str, fixed(secondary))
}

Shots_25$SHOTISGOAL <- as.logical(Shots_25$SHOTISGOAL)

Shots_25 <- Shots_25 %>%
  mutate(
    SHOTDISTANCE = sqrt((100 - LOCATIONX)^2 + (50 - LOCATIONY)^2),
  )

Shots_25 <- Shots_25 %>% 
  mutate(
    Angle_stolpe_1 = atan2(45 - LOCATIONY, 100 - LOCATIONX) * (180 / pi),
    Angle_stolpe_2 = atan2(55 - LOCATIONY, 100 - LOCATIONX) * (180 / pi),
    SHOTANGLE =  abs(Angle_stolpe_1 - Angle_stolpe_2)
  )


Shots_25 <- Shots_25 %>%
  mutate(
    SHOTBODYPART_CAT = ifelse(SHOTBODYPART == "head_or_other", 1, 
                              ifelse(SHOTBODYPART %in% c("left_foot", "right_foot"), 0, NA))
  )

Shots_25 <- Shots_25 %>% select(SHOTISGOAL,SHOTDISTANCE,SHOTANGLE,opportunity,SHOTBODYPART_CAT,corner)

Shots_25 <- Shots_25 %>%
  mutate(across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE), labels = c(0, 1))))

Shots_25 <- Shots_25 %>%
  mutate(across(c(opportunity, corner, SHOTBODYPART_CAT), 
                as.factor))

###############
####XGBoost####
###############

set.seed(123)
trainIndex_25 <- createDataPartition(Shots_25$SHOTISGOAL, p = 0.8, list = FALSE)
train_data_25 <- Shots_xG[trainIndex_25, ]
test_data_25 <- Shots_xG[-trainIndex_25, ]

train_data_xgb_25 <- train_data_25
test_data_xgb_25 <- test_data_25

train_data_xgb_25$SHOTISGOAL <- as.numeric(as.character(train_data_25$SHOTISGOAL))
test_data_xgb_25$SHOTISGOAL <- as.numeric(as.character(test_data_25$SHOTISGOAL))

train_matrix_xgb_25 <- model.matrix(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT +  
                                   opportunity + counterattack, data = train_data_xgb_25)[, -1] # -1 fjerner intercept (skæringspunktet) fra matrixen

test_matrix_xgb_25 <- model.matrix(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + 
                                  opportunity + counterattack, data = test_data_xgb_25)[, -1] # -1 fjerner intercept (skæringspunktet) fra matrixen

dtrain_xgb_25 <- xgb.DMatrix(data = train_matrix_xgb_25, label = train_data_xgb_25$SHOTISGOAL)
dtest_xgb_25 <- xgb.DMatrix(data = test_matrix_xgb_25, label = test_data_xgb_25$SHOTISGOAL)

xgb_model_25 <- xgboost(data = dtrain_xgb_25,
                     max.depth = 3,
                     eta = 0.1,
                     nrounds = 100,
                     objective = "binary:logistic",
                     verbose = 0)

#########################################################
####Konfusionsmatrix, ROC kurve & Variable Importance####
#########################################################

forudsigelse_xgb_25 <- predict(xgb_model_25, dtest_xgb_25)
forudsigelse_xgb_ForudsigelseBin_25 <- ifelse(forudsigelse_xgb_25 >= 0.4, "Mål", "Ikke mål")

Konfusionsmatrix_xgb_25 <- table(Predicted = forudsigelse_xgb_ForudsigelseBin_25,
                              Actual = ifelse(test_data_xgb_25$SHOTISGOAL == 1, "Mål", "Ikke mål"))

print(Konfusionsmatrix_xgb_25)

ROC_xgb_25 <- roc(test_data_xgb_25$SHOTISGOAL, forudsigelse_xgb_25)

plot(ROC_xgb_25, col = "darkgreen", lwd = 2, main = "XGBoost forudsigelsesevne er klart bedst")

# feature importance

importance_matrix_25 <- xgb.importance(model = xgb_model_25)

Feature_im_25 <- xgb.plot.importance(importance_matrix_25, top_n = 10, 
                                  main = "XGBoost – Skudafstand er absolut vigtigst for scoring efterfulgt af 'store chancer'", 
                                  rel_to_first = TRUE, 
                                  xlab = "Relative Importance")

Shot_glm_25 <- glm(SHOTISGOAL ~ SHOTDISTANCE + SHOTANGLE + SHOTBODYPART_CAT + 
                   opportunity + counterattack, 
                 data = train_data_25, family = binomial)

summary(Shot_glm_25)

Shots_Forudsigelse_25 <- predict(Shot_glm_25, newdata = test_data_25, type = "response")

Threshold <- 0.4

Shots_ForudsigelseBin_25 <- ifelse(Shots_Forudsigelse_25 > Threshold, "Mål", "Ikke mål")

Konfusion_matrix_25 <- table(Predicted = Shots_ForudsigelseBin_25, Actual = test_data_25$SHOTISGOAL)

print(Konfusion_matrix_25)

ROC_kurve_25 <- roc(test_data_25$SHOTISGOAL, Shots_Forudsigelse_25, levels = c("0", "1"), direction = "<")

plot(ROC_kurve_25, col = "darkgreen", lwd = 2, main = "Den logistiske regression har fornuftige prædiktive evner, men svært ved at forudsige scoringer")

print(ROC_kurve_25$auc)

shots_goal <- Shots %>% filter(SHOTISGOAL == 1)
shots_goal25 <- Shots_25 %>% filter(SHOTISGOAL == 1)
