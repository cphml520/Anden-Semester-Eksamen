library(tidyverse)
library(skimr)
library(clustMixType)
library(corrplot)

###########################################
#### Data retrival and transofrmation ####
##########################################

passes <- Events %>% 
  filter(PRIMARYTYPE == "pass") %>% 
  select("MATCH_WYID","MATCHPERIOD","MINUTE","SECOND","LOCATIONX","LOCATIONY","ACCURATE","ANGLE","HEIGHT.x","LENGTH","ENDLOCATIONX","ENDLOCATIONY",
         "SECONDARYTYPE1","SECONDARYTYPE2","SECONDARYTYPE3","SECONDARYTYPE4","SECONDARYTYPE5",
         "SECONDARYTYPE6","SECONDARYTYPE7","SECONDARYTYPE8","SECONDARYTYPE9","SECONDARYTYPE10",
         "HEIGHT.y","WEIGHT","BIRTHDATE","ROLENAME","FOOT", "SHORTNAME")


####Secondarytypes
# Opretter en liste for Secondarytypes
passes <- passes %>%
  mutate(SECONDARYTYPE = pmap(list(SECONDARYTYPE1, SECONDARYTYPE2, SECONDARYTYPE3, SECONDARYTYPE4, SECONDARYTYPE5,
                                   SECONDARYTYPE6, SECONDARYTYPE7, SECONDARYTYPE8, SECONDARYTYPE9, SECONDARYTYPE10), 
                              function(...) unique(na.omit(c(...)))))


# Finder alle unikke secondarytype værdier
unique_secondarytypes <- unique(unlist(passes$SECONDARYTYPE))
unique_secondarytypes <- unique_secondarytypes[-3] # removing the empty string

# Liste omdannes til karakterstreng ????????????????
passes <- passes %>%
  mutate(SECONDARYTYPE_str = map_chr(SECONDARYTYPE, ~ paste(.x, collapse = ","))) %>%
  replace_na(list(SECONDARYTYPE_str = ""))  # Undgå NA-problemer

# Oprettelse af boolean-kolonner for hver unik secondarytype
for (secondary in unique_secondarytypes) {
  passes[[secondary]] <- str_detect(passes$SECONDARYTYPE_str, fixed(secondary))
}

# Fjern tidligere secondarytype variabler som ikke længere skal anvendes
passes <- passes[,-c(13:22,29,30)]

#############################
## Prepare for clustering ##
############################

#### CRetae new columns or adjust the exsiting
passes <- passes %>%
  mutate(MATCHPERIOD2= case_when(
    MATCHPERIOD == "1H" & MINUTE >= 45 ~ "1H-OT",
    MATCHPERIOD == "2H" & MINUTE >= 90 ~ "2H-OT",
    MINUTE < 15 ~ "00-14",
    MINUTE < 30 ~ "15-29",
    MINUTE < 45 ~ "30-44",
    MINUTE < 60 ~ "45-59",
    MINUTE < 75 ~ "60-74",
    MINUTE < 90 ~ "75-89",
    TRUE ~ NA_character_
  ))

passes <- passes %>% 
  mutate(ACCURATE = case_when(
    ACCURATE == "true" ~ TRUE,
    TRUE ~ FALSE
  ))

passes$Age <- as.integer(floor(as.numeric(Sys.Date() - as.Date(passes$BIRTHDATE)) / 365.25)) # floor to round to integer

passes <- passes %>% 
  mutate(HEIGHT = case_when(
    HEIGHT.x == "high"~"high",
    TRUE ~ "no"
  ))

#check for types and NAs
str(passes)
skim(passes)

passes <- na.omit(passes) # 453 NAs come from 205 matches

##############################
####CHECK FOR CORRELATION ####
##############################

# Step1
passes_cluster <-  passes %>% 
  select("LOCATIONX", "LOCATIONY", "ACCURATE", "ANGLE","LENGTH", "ENDLOCATIONX", "ENDLOCATIONY",
         "HEIGHT.y", "WEIGHT", "ROLENAME","SHORTNAME", "back_pass",
         "short_or_medium_pass", "forward_pass", "long_pass", "loss", "pass_to_final_third",
         "head_pass", "progressive_pass", "lateral_pass", "linkup_play","under_pressure", "cross",
         "pass_to_penalty_area", "recovery", "carry", "through_pass", "deep_completion",
         "counterpressing_recovery", "touch_in_box", "deep_completed_cross",
         "shot_assist", "key_pass", "hand_pass", "smart_pass", "cross_blocked", "third_assist",
         "assist", "second_assist", "progressive_run", "MATCHPERIOD2", "Age", "HEIGHT")

#Step2
passes_cluster <-  passes %>% 
  select("LOCATIONX", "LOCATIONY", "ACCURATE", "ANGLE","LENGTH", "ENDLOCATIONX", "ENDLOCATIONY",
         "HEIGHT.y", "WEIGHT", "ROLENAME", "SHORTNAME", "loss", "head_pass", "progressive_pass", "linkup_play","under_pressure",
         "recovery", "carry", "through_pass", "counterpressing_recovery", "shot_assist", "key_pass", "hand_pass", "smart_pass", "cross_blocked", "third_assist",
         "assist", "second_assist", "progressive_run", "MATCHPERIOD2", "Age", "HEIGHT")

#Step3
passes_cluster <-  passes %>% 
  select("LOCATIONX", "LOCATIONY", "ACCURATE", "ANGLE","LENGTH", "ENDLOCATIONX", "ENDLOCATIONY",
         "HEIGHT.y", "WEIGHT", "ROLENAME","SHORTNAME", "head_pass", "progressive_pass",
         "linkup_play","under_pressure", "recovery", "carry", "through_pass", "shot_assist", 
         "hand_pass", "cross_blocked", "third_assist", "second_assist", "progressive_run", "MATCHPERIOD2", "Age", "HEIGHT")

#### Check for correlation (variables need to be numeric to be checked)
passes_cor <- passes_cluster %>%
  mutate(across(where(is.logical), as.numeric),
         across(where(is.character), ~ as.numeric(as.factor(.))))

passes_cor <- passes_cor %>%
  select(where(is.numeric))

cor_matrix <- cor(passes_cor, use = "complete.obs")

windows(width = 12, height = 12)
corrplot(cor_matrix,
         method = "number",      # show numbers instead of circles
         type = "lower",         # lower triangle only
         tl.pos = "ld",          # text labels to left and diagonal
         number.cex = 0.7,
         tl.cex = 0.8)

# remove correlated variables (if more than 50% correlated + associated with the numeric values, e.g. short_medium_pass, back_pass)

############################
#### CHECK FOR VARIANCE ####
############################

#### Check for variance
passes_variance <- passes_cluster %>%
  select(where(is.logical)) %>%
  summarise(across(everything(), ~ mean(.) * 100)) %>%
  pivot_longer(everything(), names_to = "Variabel", values_to = "PCT")

# remove variables with too low variance (less than 4%)
# remove player info as it does not cluster with much variance
passes_cluster <- passes %>% 
  select("MATCHPERIOD2","LOCATIONX","LOCATIONY","ACCURATE","ANGLE","HEIGHT","LENGTH","ENDLOCATIONX",
         "ENDLOCATIONY","ROLENAME","head_pass","recovery")

#### Make character and logical values into factors
passes_cluster[] <- lapply(passes_cluster, function(col) {
  if (is.character(col) || is.logical(col)) as.factor(col) else col
})

str(passes_cluster)
skim(passes_cluster)

passes_cluster_scaled <- passes_cluster %>% 
  mutate(
    LENGTH = scale(LENGTH),
    ANGLE = scale(ANGLE)
  )

#### Clustering with k-prototype
set.seed(123)

wss <- numeric()
models <- list()

for (k in 3:8) {
  model <- kproto(passes_cluster_scaled, k)
  wss[k] <- model$tot.withinss
  models[[as.character(k)]] <- model
}

plot(3:8, wss[3:8], type = "b", pch = 19,
     xlab = "Antal af klynger (k)", ylab = "Total sum af kvadrater inden for klynger",
     main = "Albuemetoden for k-prototyper")


# K=3
passes_clustered_3 <- passes_cluster %>%
  mutate(cluster = models[[1]]$cluster)

cluster_summary_3 <- passes_clustered_3 %>%
  group_by(cluster) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.factor), ~ {
      if (all(levels(.x) %in% c("TRUE", "FALSE"))) {
        mean(as.numeric(.x == "TRUE"))
      } else {
        names(sort(table(.x), decreasing = TRUE))[1]  # mode
      }
    })
  )

# K=4
passes_clustered_4 <- passes_cluster %>%
  mutate(cluster = models[[2]]$cluster)

cluster_summary_4 <- passes_clustered_4 %>%
  group_by(cluster) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.factor), ~ {
      if (all(levels(.x) %in% c("TRUE", "FALSE"))) {
        mean(as.numeric(.x == "TRUE"))
      } else {
        names(sort(table(.x), decreasing = TRUE))[1]  # mode
      }
    })
  )

# K=5
passes_clustered_5 <- passes_cluster %>%
  mutate(cluster = models[[3]]$cluster)

cluster_summary_5 <- passes_clustered_5 %>%
  group_by(cluster) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.factor), ~ {
      if (all(levels(.x) %in% c("TRUE", "FALSE"))) {
        mean(as.numeric(.x == "TRUE"))
      } else {
        names(sort(table(.x), decreasing = TRUE))[1]  # mode
      }
    })
  )

# K=6
passes_clustered_6 <- passes_cluster %>%
  mutate(cluster = models[[4]]$cluster)

cluster_summary_6 <- passes_clustered_6 %>%
  group_by(cluster) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.factor), ~ {
      if (all(levels(.x) %in% c("TRUE", "FALSE"))) {
        mean(as.numeric(.x == "TRUE"))
      } else {
        names(sort(table(.x), decreasing = TRUE))[1]  # mode
      }
    })
  )

# K=7
passes_clustered_7 <- passes_cluster %>%
  mutate(cluster = models[[5]]$cluster)

cluster_summary_7 <- passes_clustered_7 %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.factor), ~ {
      if (all(levels(.x) %in% c("TRUE", "FALSE"))) {
        mean(as.numeric(.x == "TRUE"))
      } else {
        names(sort(table(.x), decreasing = TRUE))[1]  # mode
      }
    })
  )

# K=8
passes_clustered_8 <- passes_cluster %>%
  mutate(cluster = models[[6]]$cluster)

cluster_summary_8 <- passes_clustered_8 %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.factor), ~ {
      if (all(levels(.x) %in% c("TRUE", "FALSE"))) {
        mean(as.numeric(.x == "TRUE"))
      } else {
        names(sort(table(.x), decreasing = TRUE))[1]  # mode
      }
    })
  )
