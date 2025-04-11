library(tidyverse)

### add game info
matches_merged <- matches_merged %>%
  group_by(MATCH_WYID) %>%
  mutate(game = {
    teams <- unique(paste(TEAMNAME, SIDE))
    teams <- teams[order(grepl("home", teams, ignore.case = TRUE), decreasing = TRUE)]
    teams <- word(teams, 1)  # takes the first word of each
    paste(teams, collapse = " - ")
  }) %>%
  ungroup()

matches_merged <- matches_merged %>%
  mutate(game = paste(game, substr(DATE, 1, 10)))

passes_clustered_7$MATCH_WYID <- passes$MATCH_WYID

passes_clustered_7 <- passes_clustered_7 %>%
  left_join(
    matches_merged %>%
      select(MATCH_WYID, game) %>%
      distinct(MATCH_WYID, .keep_all = TRUE),
    by = "MATCH_WYID"
  )
