library(tidyverse)
library(ggsoccer)

# plot the start and end locations
ggplot(cluster_summary_7) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "black", fill = "forestgreen") +
  geom_segment(aes(x = LOCATIONX_mean, y = LOCATIONY_mean,
                   xend = ENDLOCATIONX_mean, yend = ENDLOCATIONY_mean,
                   color = as.factor(cluster)),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.2) +
  geom_text(aes(x = LOCATIONX_mean, y = LOCATIONY_mean, label = cluster), 
            color = "black", size = 4, fontface = "bold", show.legend = FALSE) +
  scale_color_discrete(name = "Klynger") +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  ggtitle("Gennemsnitlig start- og slutposition for afleveringer i klyngen") +
  theme_pitch() +
  theme(legend.position = "right")

#### Pie plot for all factor variables  - share in the cluster
## Matchperiod
plot_matchperiod_1 <- factor_props[factor_props$cluster == 1 & factor_props$variable == "MATCHPERIOD2", ]

ggplot(plot_matchperiod_1, aes(x = "", y = percentage, fill = level)) +
  geom_col(width = 1) +
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 4) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Andel af forskellige kampperioder i Klynge 1")

#### how much of the total of that factor in the variable is in the cluster
## Rolename as example
plot_rolename_1 <- factor_props[factor_props$cluster == 1 & factor_props$variable == "ROLENAME", ]

ggplot(plot_rolename_1, aes(x = pct_from_total, y = reorder(level, pct_from_total), fill = level)) +
  geom_col() +
  xlim(0, 1) +
  labs(x = "Andel af total", y = "Level") +
  theme_minimal()


#### Pie plot of shares of clusters per factorlevel
plot_matchperiod_0014 <- factor_props[factor_props$level == "00-14", ]

ggplot(plot_matchperiod_0014, aes(x = "", y = pct_from_total, fill = as.factor(cluster))) +
  geom_col(width = 1) +
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 4) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Afleveringer fra matchperiode 00-14 fordelt i klynger")


cluster_summary_7 <- cluster_summary_7 %>% 
  mutate(
    ACCURATE_pct_total = ACCURATE_pct_true*Count/287158,
    head_pass_pct_total = head_pass_pct_true*Count/287158,
    recovery_pct_total = recovery_pct_true*Count/287158)
