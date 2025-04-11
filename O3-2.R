library(jsonlite)
library(mongolite)
library(tidyverse)
library(ggsoccer)
library(ggrepel)
library(ggforce)
options(scipen = 999)

#### import data
con_meta <- mongo(collection = "meta",
                     db = "tracking",
                     url = "mongodb://localhost")

data_tracking_meta <- con_meta$find()

con_raw <- mongo(collection = "raw",
                 db = "tracking",
                 url = "mongodb://localhost")

data_tracking <- con_raw$find()
colnames(data_tracking)
data_tracking$homePlayers[1]

#### move the player info to the main dataframe
home_players_df <- map_dfr(data_tracking$homePlayers, bind_rows)
away_players_df <- map_dfr(data_tracking$awayPlayers, bind_rows)

home_players_df <- home_players_df %>%
  mutate(
    xyz = str_remove_all(xyz, "c\\(|\\)|\""),
    xyz = str_split(xyz, ",\\s*")            
  ) %>%
  mutate(
    x = as.numeric(map_chr(xyz, 1)),
    y = as.numeric(map_chr(xyz, 2)),
    z = as.numeric(map_chr(xyz, 3))
  ) %>%
  select(-xyz)

away_players_df <- away_players_df %>%
  mutate(
    xyz = str_remove_all(xyz, "c\\(|\\)|\""), # remove c( and quotes
    xyz = str_split(xyz, ",\\s*")             # split into list of 3
  ) %>%
  mutate(
    x = as.numeric(map_chr(xyz, 1)),
    y = as.numeric(map_chr(xyz, 2)),
    z = as.numeric(map_chr(xyz, 3))
  ) %>%
  select(-xyz)

home_players_df <- home_players_df %>%
  mutate(team = "Vejle Boldklub")

away_players_df <- away_players_df %>%
  mutate(team = "Odense Boldklub")

home_players <- as.data.frame(data_tracking_meta$homePlayers)
away_players <- as.data.frame(data_tracking_meta$awayPlayers)
all_players <- rbind(home_players, away_players)

home_players_df <- home_players_df %>%
  mutate(frame = rep(seq_len(nrow(data_tracking)), each = 11))

away_players_df <- away_players_df %>%
  mutate(frame = rep(seq_len(nrow(data_tracking)), each = 11))

players_df <- rbind(home_players_df, away_players_df)

players_df <- players_df %>%
  select(playerId, x, y, team, frame) %>%
  left_join(all_players[, c("name", "ssiId")], by = c("playerId" = "ssiId"))
players_df <- players_df[,-1]

data_tracking <- data_tracking %>%
  mutate(
    xyz = str_remove_all(ball$xyz, "c\\(|\\)|\""),
    xyz = str_split(xyz, ",\\s*"),
    ball.x = as.numeric(map_chr(xyz, 1, .default = NA)),
    ball.y = as.numeric(map_chr(xyz, 2, .default = NA)),
    ball.z = as.numeric(map_chr(xyz, 3, .default = NA))
  ) %>%
  select(-xyz)

data_tracking <- data_tracking %>% 
  arrange(frameIdx)

#### Try plotting some of them
data_tracking_coord <- data_tracking %>%
  select(x = ball.x, y = ball.y, frame = frameIdx) %>%
  mutate(name = NA, team = "ball")

data_tracking_coord <- bind_rows(data_tracking_coord, players_df)

#### adjust the x and y, so they can be plotted
data_tracking_coord <- data_tracking_coord %>%
  mutate(
    x = x + 50,
    y = y + 52
  )


#### Plotting function
plot_frame <- function(df_long, frame_num, output_dir = "frames") {
  df_frame <- df_long %>% filter(frame == frame_num)
 
  p <- ggplot(df_frame, aes(x = x, y = y, color = team)) +
    annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
    geom_point(size = 4) +
    geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
    scale_color_manual(
      values = c(
        "Vejle Boldklub" = "red",
        "Odense Boldklub" = "darkblue",
        "ball" = "yellow"
      )
    ) +
    coord_fixed() +
    theme_pitch()
  
  if (!dir.exists(output_dir)) dir.create(output_dir)
  ggsave(sprintf("%s/frame_%04d.jpg", output_dir, frame_num), p, width = 8, height = 5)
}

#### Loop through frames ####
unique_frames <- sort(unique(data_tracking_coord$frame))

frame_ids <- seq(36600, 36650, by = 10)
for (f in frame_ids[frame_ids %in% unique_frames]) {
  plot_frame(data_tracking_coord, f)
}

#########################
#### PLOTTING PASSES ####
########################

######## STEP1 ###########
frame_26020 <- data_tracking_coord[data_tracking_coord$frame == 26020,]
carrier <- frame_26020 %>% filter(name == "A. Okosun") #ball holder
targets <- frame_26020 %>% filter(team == carrier$team, name != carrier$name, team != "ball") #teammates

#Create dynamic fan shadows toward teammates
fan_angle <- 10 * pi / 180  # 10 degrees

shadow_polys <- purrr::map_dfr(1:nrow(targets), function(i) {
  target <- targets[i, ]
  angle <- atan2(target$y - carrier$y, target$x - carrier$x)
  dist <- sqrt((target$x - carrier$x)^2 + (target$y - carrier$y)^2)
  
  x0 <- carrier$x
  y0 <- carrier$y
  x1 <- x0 + cos(angle - fan_angle) * dist
  y1 <- y0 + sin(angle - fan_angle) * dist
  x2 <- x0 + cos(angle + fan_angle) * dist
  y2 <- y0 + sin(angle + fan_angle) * dist
  
  data.frame(
    x = c(x0, x1, x2),
    y = c(y0, y1, y2),
    group = i
  )
})

# Plot with shadows
ggplot(frame_26020, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    name = "Hold",
    values = c(
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

# Get coordinates for potential passes
to_option1 <- frame_26020 %>% filter(name == "K. Larsen")
to_option2 <- frame_26020 %>% filter(name == "R. Ostrom")
to_option3 <- frame_26020 %>% filter(name == "J. Skjelvik")
to_option4 <- frame_26020 %>% filter(name == "M. Frokjaer-Jensen")

# Plot with shadows and arrows
ggplot(frame_26020, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option1$x, yend = to_option1$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "red", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option2$x, yend = to_option2$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option3$x, yend = to_option3$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option4$x, yend = to_option4$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      name = "Hold",
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

###### STEP2 ##########
frame_26080 <- data_tracking_coord[data_tracking_coord$frame == 26080,]
carrier <- frame_26080 %>% filter(name == "K. Larsen") #ball holder
targets <- frame_26080 %>% filter(team == carrier$team, name != carrier$name, team != "ball") #teammates

#Create dynamic fan shadows toward teammates
fan_angle <- 10 * pi / 180  # 10 degrees

shadow_polys <- purrr::map_dfr(1:nrow(targets), function(i) {
  target <- targets[i, ]
  angle <- atan2(target$y - carrier$y, target$x - carrier$x)
  dist <- sqrt((target$x - carrier$x)^2 + (target$y - carrier$y)^2)
  
  x0 <- carrier$x
  y0 <- carrier$y
  x1 <- x0 + cos(angle - fan_angle) * dist
  y1 <- y0 + sin(angle - fan_angle) * dist
  x2 <- x0 + cos(angle + fan_angle) * dist
  y2 <- y0 + sin(angle + fan_angle) * dist
  
  data.frame(
    x = c(x0, x1, x2),
    y = c(y0, y1, y2),
    group = i
  )
})

# Plot with shadows
ggplot(frame_26080, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    name = "Hold",
    values = c(
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

# Get coordinates for potential passes
to_option1 <- frame_26080 %>% filter(name == "A. Okosun")
to_option2 <- frame_26080 %>% filter(name == "J. Skjelvik")
to_option3 <- frame_26080 %>% filter(name == "J. King")
to_option4 <- frame_26080 %>% filter(name == "J. Thomasen")
to_option5 <- frame_26080 %>% filter(name == "H. Christian Bernat")

# Plot with shadows and arrows
ggplot(frame_26080, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option1$x, yend = to_option1$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option2$x, yend = to_option2$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "red", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option3$x, yend = to_option3$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option4$x, yend = to_option4$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option5$x, yend = to_option5$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      name = "Hold",
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

###### STEP3 ##########
frame_26160 <- data_tracking_coord[data_tracking_coord$frame == 26160,]
carrier <- frame_26160 %>% filter(name == "J. Skjelvik") #ball holder
targets <- frame_26160 %>% filter(team == carrier$team, name != carrier$name, team != "ball") #teammates

#Create dynamic fan shadows toward teammates
fan_angle <- 10 * pi / 180  # 10 degrees

shadow_polys <- purrr::map_dfr(1:nrow(targets), function(i) {
  target <- targets[i, ]
  angle <- atan2(target$y - carrier$y, target$x - carrier$x)
  dist <- sqrt((target$x - carrier$x)^2 + (target$y - carrier$y)^2)
  
  x0 <- carrier$x
  y0 <- carrier$y
  x1 <- x0 + cos(angle - fan_angle) * dist
  y1 <- y0 + sin(angle - fan_angle) * dist
  x2 <- x0 + cos(angle + fan_angle) * dist
  y2 <- y0 + sin(angle + fan_angle) * dist
  
  data.frame(
    x = c(x0, x1, x2),
    y = c(y0, y1, y2),
    group = i
  )
})

# Plot with shadows
ggplot(frame_26160, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    name = "Hold",
    values = c(
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

# Get coordinates for potential passes
to_option1 <- frame_26160 %>% filter(name == "K. Larsen")
to_option2 <- frame_26160 %>% filter(name == "J. Breum")
to_option3 <- frame_26160 %>% filter(name == "J. King")
to_option4 <- frame_26160 %>% filter(name == "J. Thomasen")
to_option5 <- frame_26160 %>% filter(name == "H. Christian Bernat")

# Plot with shadows and arrows
ggplot(frame_26160, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option1$x, yend = to_option1$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option2$x, yend = to_option2$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option3$x, yend = to_option3$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option4$x, yend = to_option4$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "red", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option5$x, yend = to_option5$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      name = "Hold",
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

###### STEP4 ##########
frame_26200 <- data_tracking_coord[data_tracking_coord$frame == 26200,]
carrier <- frame_26200 %>% filter(name == "J. Thomasen") #ball holder
targets <- frame_26200 %>% filter(team == carrier$team, name != carrier$name, team != "ball") #teammates

#Create dynamic fan shadows toward teammates
fan_angle <- 10 * pi / 180  # 10 degrees

shadow_polys <- purrr::map_dfr(1:nrow(targets), function(i) {
  target <- targets[i, ]
  angle <- atan2(target$y - carrier$y, target$x - carrier$x)
  dist <- sqrt((target$x - carrier$x)^2 + (target$y - carrier$y)^2)
  
  x0 <- carrier$x
  y0 <- carrier$y
  x1 <- x0 + cos(angle - fan_angle) * dist
  y1 <- y0 + sin(angle - fan_angle) * dist
  x2 <- x0 + cos(angle + fan_angle) * dist
  y2 <- y0 + sin(angle + fan_angle) * dist
  
  data.frame(
    x = c(x0, x1, x2),
    y = c(y0, y1, y2),
    group = i
  )
})

# Plot with shadows
ggplot(frame_26200, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    name = "Hold",
    values = c(
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

# Get coordinates for potential passes
to_option1 <- frame_26200 %>% filter(name == "K. Larsen")
to_option2 <- frame_26200 %>% filter(name == "J. Breum")
to_option3 <- frame_26200 %>% filter(name == "J. King")
to_option4 <- frame_26200 %>% filter(name == "J. Skjelvik")
to_option5 <- frame_26200 %>% filter(name == "H. Christian Bernat")
to_option6 <- frame_26200 %>% filter(name == "M. Frokjaer-Jensen")

# Plot with shadows and arrows
ggplot(frame_26200, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option1$x, yend = to_option1$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option2$x, yend = to_option2$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option3$x, yend = to_option3$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option4$x, yend = to_option4$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "red", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option5$x, yend = to_option5$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option6$x, yend = to_option6$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      name = "Hold",
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

###### STEP5 ##########
frame_26240 <- data_tracking_coord[data_tracking_coord$frame == 26240,]
carrier <- frame_26240 %>% filter(name == "J. Skjelvik") #ball holder
targets <- frame_26240 %>% filter(team == carrier$team, name != carrier$name, team != "ball") #teammates

#Create dynamic fan shadows toward teammates
fan_angle <- 10 * pi / 180  # 10 degrees

shadow_polys <- purrr::map_dfr(1:nrow(targets), function(i) {
  target <- targets[i, ]
  angle <- atan2(target$y - carrier$y, target$x - carrier$x)
  dist <- sqrt((target$x - carrier$x)^2 + (target$y - carrier$y)^2)
  
  x0 <- carrier$x
  y0 <- carrier$y
  x1 <- x0 + cos(angle - fan_angle) * dist
  y1 <- y0 + sin(angle - fan_angle) * dist
  x2 <- x0 + cos(angle + fan_angle) * dist
  y2 <- y0 + sin(angle + fan_angle) * dist
  
  data.frame(
    x = c(x0, x1, x2),
    y = c(y0, y1, y2),
    group = i
  )
})

# Plot with shadows
ggplot(frame_26240, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    name = "Hold",
    values = c(
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

# Get coordinates for potential passes
to_option1 <- frame_26240 %>% filter(name == "K. Larsen")
to_option2 <- frame_26240 %>% filter(name == "A. Okosun")
to_option3 <- frame_26240 %>% filter(name == "J. King")
to_option4 <- frame_26240 %>% filter(name == "J. Thomasen")
to_option5 <- frame_26240 %>% filter(name == "H. Christian Bernat")

# Plot with shadows and arrows
ggplot(frame_26240, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option1$x, yend = to_option1$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option2$x, yend = to_option2$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "red", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option3$x, yend = to_option3$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option4$x, yend = to_option4$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
  geom_segment(x = carrier$x, y = carrier$y, xend = to_option5$x, yend = to_option5$y,
               arrow = arrow(length = unit(0.2, "cm")), color = "green", linewidth = 1.2) +
    geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      name = "Hold",
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()

###### STEP5 ##########
frame_26300 <- data_tracking_coord[data_tracking_coord$frame == 26300,]
carrier <- frame_26300 %>% filter(name == "A. Okosun") #ball holder
targets <- frame_26300 %>% filter(team == carrier$team, name != carrier$name, team != "ball") #teammates

#Create dynamic fan shadows toward teammates
fan_angle <- 10 * pi / 180  # 10 degrees

shadow_polys <- purrr::map_dfr(1:nrow(targets), function(i) {
  target <- targets[i, ]
  angle <- atan2(target$y - carrier$y, target$x - carrier$x)
  dist <- sqrt((target$x - carrier$x)^2 + (target$y - carrier$y)^2)
  
  x0 <- carrier$x
  y0 <- carrier$y
  x1 <- x0 + cos(angle - fan_angle) * dist
  y1 <- y0 + sin(angle - fan_angle) * dist
  x2 <- x0 + cos(angle + fan_angle) * dist
  y2 <- y0 + sin(angle + fan_angle) * dist
  
  data.frame(
    x = c(x0, x1, x2),
    y = c(y0, y1, y2),
    group = i
  )
})

# Plot with shadows
ggplot(frame_26300, aes(x = x, y = y, color = team)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "forestgreen") +
  geom_polygon(data = shadow_polys, aes(x = x, y = y, group = group),
               fill = "black", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = name), size = 3, max.overlaps = 50, na.rm = TRUE) +
  scale_color_manual(
    name = "Hold",
    values = c(
      "Vejle Boldklub" = "red",
      "Odense Boldklub" = "darkblue",
      "ball" = "yellow")) +
  coord_fixed() +
  theme_pitch()
