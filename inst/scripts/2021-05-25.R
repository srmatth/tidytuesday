#### 2021-05-25 ----

#### Setup ----

library(tidytuesdayR)
library(ggplot2)
library(ggtext)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(extrafont)
suppressMessages(loadfonts(device = "win"))

dat <- tt_load("2021-05-25")

records <- dat$records
drivers <- dat$drivers

records %>% colnames()
drivers %>% colnames()

View(records)
View(drivers)

#### Records Visualizations ----

# Records over time by track, lap, and shortcut

records %>%
  ggplot() +
  aes(y = time, x = date, color = track) +
  geom_line() +
  facet_grid(shortcut ~ type, labeller = "label_both")

# Let's look at just Mario Raceway (one of my favorites) for three laps

records %>%
  filter(
    type == "Three Lap",
    track == "Mario Raceway"
  ) %>%
  ggplot() +
  aes(x = date, y = time, color = shortcut) +
  # geom_point(pch = 19, size = 3) +
  geom_step(lwd = 2.5) +
  theme_classic() +
  scale_color_manual(values = c("#BF0603", "#708D81")) +
  xlab("Date") +
  ylab("Times (seconds)") +
  ggtitle("Records on Mario Raceway Over Time", "Three Laps") +
  labs(color = "Was a Shortcut Used?") +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 1,
      title.hjust = 0.5
    )
  ) +
  theme(
    legend.background = element_rect(color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.9, 0.5)
  )

# Where do shortcuts help the most?
records %>%
  filter(type == "Three Lap") %>%
  group_by(track, shortcut) %>%
  arrange(time, desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-player, -system_played, -time_period, -record_duration, -date) %>%
  pivot_wider(
    names_from = "shortcut",
    values_from = "time",
    names_prefix = "shortcut_"
  ) %>%
  filter(!is.na(shortcut_Yes)) %>%
  mutate(time_diff = shortcut_No - shortcut_Yes) %>%
  filter(time_diff > 0) %>%
  mutate(track = forcats::fct_reorder(track, time_diff, abs)) %>%
  ggplot() +
  aes(
    x = track, 
    y = time_diff, 
    label = ifelse(
      time_diff > 20,
      stringr::str_c(round(time_diff, 0), "s"),
      " "
    )
  ) +
  geom_bar(stat = "identity", fill = "#385154") +
  geom_text(
    hjust = 1, 
    color = "#ffffff", 
    fontface = "bold",
    family = "Times New Roman",
    size = 5
  ) +
  theme_classic() +
  coord_flip(expand = FALSE) +
  xlab("") +
  ylab("Time Gain from Using the Shortcut (seconds)") +
  ggtitle(
    "Where do Shortcuts Help the Most?", 
    "(Three Lap Races Only - Based on Most Recent Record)"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(
      hjust = 0.5,
      size = 18
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 16
    ),
    plot.background = element_rect(fill = "#F8E4B5", color = NA),
    panel.background = element_rect(fill = "#F8E4B5"),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  )

# How long do records last?

records %>%
  ggplot() +
  aes(y = record_duration, x = date) +
  geom_point()

records %>%
  ggplot() +
  aes(x = date) +
  geom_histogram()


#### Drivers Visualization ----

dat_to_display <- drivers %>%
  group_by(Nation = nation) %>%
  summarize(`Total Records` = sum(records, na.rm = TRUE)) %>%
  arrange(desc(`Total Records`)) %>%
  slice(1:3)

d <- data.frame(nation = "Netherlands", year = 2010, num_records = 200, y = 200, x = 2020)
d$grob <- list(
  gridExtra::tableGrob(
    dat_to_display,
    rows=NULL,
    theme = gridExtra::ttheme_default(
      base_family = "Arial",
      base_colour = "#385154"
    )
  )
)


drivers %>%
  filter(nation %in% c("Australia", "Netherlands", "USA")) %>%
  group_by(nation, year) %>%
  summarize(num_records = sum(records, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    nation = factor(nation, levels = c("USA", "Australia", "Netherlands"))
  ) %>%
  ggplot() +
  aes(x = year, y = num_records, color = nation) +
  geom_line(lwd = 2) +
  facet_wrap(~nation) +
  scale_color_manual(values = c("#9F196B", "#FB8B24", "#0F4C5C")) +
  theme_classic() +
  xlab("Year") +
  ylab("Records Set") +
  ggtitle("Records per Year by Country", "Top 3 Countries") +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  ) + 
  # annotation_custom2(
  #   tg,
  #   xmin=2005, xmax=2015, ymin=75, ymax=175,
  #   data = data.frame(nation = "Netherlands")
  # ) # +
  egg::geom_custom(
    data = d,
    aes(data = grob),
    grob_fun = identity
  )
