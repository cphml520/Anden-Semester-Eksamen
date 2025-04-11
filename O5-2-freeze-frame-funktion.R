library(mongolite)
library(jsonlite)
library(tidyverse)

###### Pull the data from mongo
#women
con_events <- mongo(collection = "events",
                    db = "statsbomb",
                    url = "mongodb://localhost")

query_shots <- '{"type.name": "Shot"}'

data_shots_W <- con_events$find(query_shots)
data_shots_W <- jsonlite::flatten(data_shots_W)

#men
con_maleevents <- mongo(collection = "maleevents",
                        db = "statsbomb",
                        url = "mongodb://localhost")

data_shots_M <- con_maleevents$find(query_shots)
data_shots_M <- jsonlite::flatten(data_shots_M)

######Create one dataframe with gender as filter
data_shots_M <- data_shots_M[,-1] #extra id column
colnames(data_shots_M)[1] <- c("match_id") # MatchID before
data_shots_W <- data_shots_W[,-30] #shot.match_id is reduntant

#add the gender column
data_shots_M$Gender <- c("Men")
data_shots_W$Gender <- c("Women")

data_shots <- bind_rows(data_shots_W, data_shots_M)

### check for how many for women and men
data_shots %>% group_by(Gender) %>% summarize(Total = n())

##### Define goal post locations
goal_x <- 120 #bx & cx
goal_y1 <- 36 #by
goal_y2 <- 44 #cy

##### Function to check if a player is inside the shot triangle
is_inside_triangle <- function(px, py, ax, ay, bx, by, cx, cy) {
  area <- function(x1, y1, x2, y2, x3, y3) {
    abs((x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2)) / 2.0)
  }
  A <- area(ax, ay, bx, by, cx, cy)
  A1 <- area(px, py, bx, by, cx, cy)
  A2 <- area(ax, ay, px, py, cx, cy)
  A3 <- area(ax, ay, bx, by, px, py)
  
  return(abs(A - (A1 + A2 + A3)) < 1e-6)
}

##### Pull the x and y out of location
data_shots <- data_shots %>%
  mutate(location = gsub('c\\(|\\)|\\"', '', location)) %>%  # Clean location column
  separate(location, into = c("x", "y"), sep = ", ", convert = TRUE) %>%  # Split into x and y
  mutate(across(c(x, y), as.numeric))  # Ensure numeric conversion

######## Check for if the shot should have been a pass #####
for (i in seq_len(nrow(data_shots))) {
  print(paste("Processing row", i))
  
  shot_x <- data_shots$x[i]
  shot_y <- data_shots$y[i]
  
  # Extract opponent and teammate positions
  df_freezeframe <- as.data.frame(data_shots$shot.freeze_frame[i]) %>%
    jsonlite::flatten()
  
  # Skip if no freeze frame data
  if (nrow(df_freezeframe) == 0) {
    data_shots$pos_passes[i] <- list(FALSE)
    next
  }
  
  df_freezeframe <- df_freezeframe %>%
    mutate(location = gsub('c\\(|\\)|\\"', '', location)) %>%
    separate(location, into = c("x", "y"), sep = ", ", convert = TRUE) %>%
    mutate(across(c(x, y), as.numeric)) %>%
    na.omit()
  
  # Compute shooter’s opponent count in triangle
  opponent_count <- sum(
    mapply(is_inside_triangle, df_freezeframe$x, df_freezeframe$y,
           MoreArgs = list(ax = shot_x, ay = shot_y,
                           bx = goal_x, by = goal_y1,
                           cx = goal_x, cy = goal_y2)) &
      !df_freezeframe$teammate
  )
  
  # Get teammates
  teammates <- df_freezeframe %>% filter(teammate)
  
  if (nrow(teammates) == 0) {
    data_shots$pos_passes[i] <- list(FALSE)
    next
  }
  
  for (j in seq_len(nrow(teammates))) {
    teammate_x <- teammates$x[j]
    teammate_y <- teammates$y[j]
    
    teammate_opponent_count <- sum(
      mapply(is_inside_triangle, df_freezeframe$x, df_freezeframe$y,
             MoreArgs = list(ax = teammate_x, ay = teammate_y,
                             bx = goal_x, by = goal_y1,
                             cx = goal_x, cy = goal_y2)) &
        !df_freezeframe$teammate
    )
    
    teammates$better_position[j] <- teammate_opponent_count < opponent_count
    
    if (!is.na(teammates$better_position[j]) && teammates$better_position[j]) {
      intercept_possible <- any(
        sapply(seq_len(nrow(df_freezeframe)), function(k) {
          if (!df_freezeframe$teammate[k]) {
            opp_x <- df_freezeframe$x[k]
            opp_y <- df_freezeframe$y[k]
            
            dx <- teammate_x - shot_x
            dy <- teammate_y - shot_y
            dx_opp <- opp_x - shot_x
            dy_opp <- opp_y - shot_y
            
            t <- ((dx * dx_opp) + (dy * dy_opp)) / (dx^2 + dy^2)
            t <- max(0, min(1, t))
            
            intercept_x <- shot_x + t * dx
            intercept_y <- shot_y + t * dy
            
            dist_opp <- sqrt((opp_x - intercept_x)^2 + (opp_y - intercept_y)^2)
            dist_ball <- sqrt((shot_x - intercept_x)^2 + (shot_y - intercept_y)^2)
            
            return(dist_opp < dist_ball)
          }
          return(FALSE)
        })
      )
      
      teammates$pos_pass[j] <- !intercept_possible
    } else {
      teammates$pos_pass[j] <- NA
    }
  }
  
  successful_passes <- teammates %>% filter(pos_pass == TRUE) %>% pull(player.name)
  data_shots$pos_passes[i] <- list(successful_passes)
}


# Flag rows where pos_passes2 is character(0)
data_shots <- data_shots %>%
  mutate(no_valid_pass = lengths(pos_passes) == 0)

# Count how many teammates were in better position
data_shots <- data_shots %>%
  mutate(
    better_position_count = sapply(pos_passes, function(x) if (is.character(x)) length(x) else 0)
  )

# Calculate percentages by gender
summary <- data_shots %>%
  group_by(Gender) %>%
  summarise(
    total = n(),
    better_options = sum(no_valid_pass == FALSE),
    pct_better_options = round(100 * better_options / total, 1),
    multi_better = sum(better_position_count > 1),
    pct_multi_better = round(100 * multi_better / better_options, 1),
    missed_opportunity = sum(no_valid_pass == FALSE & shot.outcome.name != "Goal"),
    pct_missed_opportunity = round(100 * missed_opportunity / total, 1)
  )

# MEN 84,5 and WOMEN 86,0
# multiple better standing teammates - MEN 1,7 and WOMEN 1,1

