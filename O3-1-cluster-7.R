library(tidyverse)

passes_clustered_7 <- passes_clustered_7 %>% 
  mutate(
    ACCURATE = as.logical(ACCURATE),
    head_pass = as.logical(head_pass),
    recovery = as.logical(recovery)
  )

passes_clustered_7$SHORTNAME <- passes$SHORTNAME

# Summarise count, mean and sd
cluster_summary_7 <- passes_clustered_7 %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    across(where(is.numeric) & !all_of("Count"),
           list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))),
    across(where(is.logical), ~ mean(.x, na.rm = TRUE), .names = "{.col}_pct_true")
  )

# Calculate factor level percentages
factor_props <- passes_clustered_7 %>%
  group_by(cluster) %>%
  summarise(across(where(is.factor), ~ list(table(.)))) %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "counts") %>%
  unnest_longer(counts) %>%
  separate_wider_delim(counts_id, delim = ":", names = c("level")) %>%
  rename(count = counts) %>%
  group_by(cluster, variable) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

# Count total occurrences of each factor level
total_props <- passes_clustered_7 %>%
  summarise(across(where(is.factor), ~ list(table(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "counts") %>%
  unnest_longer(counts) %>%
  separate_wider_delim(counts_id, delim = ":", names = c("level")) %>%
  rename(total_count = counts) %>%
  mutate(total_pct = total_count / nrow(passes_clustered_7))

# Join with factor_props and compute pct_from_total
factor_props <- factor_props %>%
  left_join(total_props, by = c("variable", "level"))

factor_props <- factor_props %>%
  mutate(
    pct_from_total = round(count/total_count, 4),
    percentage = round(percentage, 4),
    total_pct = round(total_pct, 4)
    )

# undo the table by turning numeric
factor_props <- as_tibble(factor_props)
cols_to_convert <- intersect(c("count", "percentage", "total_count", "total_pct", "pct_from_all"), names(factor_props))

factor_props <- factor_props %>%
  mutate(across(all_of(cols_to_convert), ~ as.numeric(.)))


