#### 2021-06-08 ----


#### Setup ----

## Load Libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)
suppressMessages(loadfonts(device = "win"))

## Load and View the data
tuesdata <- tidytuesdayR::tt_load('2021-06-08')
names(tuesdata)
stocked <- tuesdata$stocked
fishing <- tuesdata$fishing
colnames(stocked)
colnames(fishing)

#### Fishing Dataset ----

View(fishing)

fishing %>%
  select(year, lake, species, grand_total) %>%
  distinct() %>%
  filter(grand_total > 10, lake == "Superior") %>%
  View()

fishing %>%
  select(year, lake, species, grand_total) %>%
  distinct() %>%
  group_by(lake, species) %>%
  summarize(
    grand_total = sum(grand_total, na.rm = TRUE),
    num_years = n(),
    max_in_one_year = max(grand_total, na.rm = TRUE)
  ) %>%
  View()
# Lake Whitefish seems to be the most prevalent, let's look at that one

## Let's look at the number of fish in each lake year over year
lakes <- fishing %>%
  select(year, lake, species, grand_total) %>%
  distinct() %>%
  group_by(lake, year) %>%
  summarize(grand_total = sum(grand_total, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    year > 1915,
    lake %in% c("Erie", "Huron", "Ontario", "Superior")
  ) 

lakes_initial <- lakes %>% 
  group_by(lake) %>%
  slice_min(year) %>%
  select(lake, initial_value = grand_total)

p_num <- lakes %>%
  ggplot() +
  aes(x = year, y = grand_total, color = lake) +
  geom_line(lwd = 1) +
  xlab("Year") +
  ylab("Fish Counted") +
  labs(color = "Lake") +
  ggtitle("Number of Fish Spotted in the Great Lakes", "Per Year, 1916 - 2015") +
  scale_y_continuous(labels = label_thousands) +
  scale_color_manual(
    values = c(
      "#EF476F",
      "#FFD166",
      "#06D6A0",
      "#118AB2"
    )
  ) +
  theme_classic() +
  guides(
    color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    axis.text = element_text(family = "Goudy Old Style"),
    axis.title = element_text(family = "Goudy Old Style"),
    legend.text = element_text(family = "Goudy Old Style"),
    legend.title = element_text(family = "Goudy Old Style"),
    legend.position = c(0.8, 0.9),
    legend.background = element_rect(color = "#073B4C")
  )

p_pct <- lakes %>%
  left_join(lakes_initial, by = "lake") %>%
  mutate(pct_change = grand_total / initial_value) %>%
  select(lake, year, pct_change) %>%
  ggplot() +
  aes(x = year, y = pct_change, color = lake, fill = lake) +
  geom_line() +
  geom_smooth() +
  xlab("Year") +
  ylab("Percent of Fish compared to 1916 Levels") +
  labs(color = "Lake", fill = "Lake") +
  ggtitle("Change in Number of Fish Spotted in the Great Lakes", "Per Year, 1916 Baseline") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c(
      "#EF476F",
      "#FFD166",
      "#06D6A0",
      "#118AB2"
    )
  ) +
  scale_fill_manual(
    values = c(
      "#EF476F",
      "#FFD166",
      "#06D6A0",
      "#118AB2"
    )
  ) +
  theme_classic() +
  guides(
    color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    axis.text = element_text(family = "Goudy Old Style"),
    axis.title = element_text(family = "Goudy Old Style"),
    legend.text = element_text(family = "Goudy Old Style"),
    legend.title = element_text(family = "Goudy Old Style"),
    legend.position = c(0.8, 0.9),
    legend.background = element_rect(color = "#073B4C")
  )

p_num / p_pct

# Look at just the Lake Whitefish
whitefish <- fishing %>%
  select(year, lake, species, grand_total) %>%
  distinct() %>%
  filter(species == "Lake Whitefish") %>%
  group_by(lake, year) %>%
  summarize(grand_total = sum(grand_total, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    year > 1915,
    lake %in% c("Erie", "Huron", "Ontario", "Superior")
  ) 

p_white <- whitefish %>%
  ggplot() +
  aes(x = year, y = grand_total, color = lake, fill = lake) +
  geom_line(lwd = 1) +
  xlab("Year") +
  ylab("Number of Fish") +
  labs(color = "Lake") +
  ggtitle("Number of Lake Whitefish Since 1916") +
  scale_y_continuous(labels = label_thousands) +
  scale_color_manual(
    values = c(
      "#EF476F",
      "#FFD166",
      "#06D6A0",
      "#118AB2"
    )
  ) +
  theme_classic() +
  guides(
    color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    axis.text = element_text(family = "Goudy Old Style"),
    axis.title = element_text(family = "Goudy Old Style"),
    legend.text = element_text(family = "Goudy Old Style"),
    legend.title = element_text(family = "Goudy Old Style"),
    legend.position = c(0.3, 0.8),
    legend.background = element_rect(color = "#073B4C")
  )

# Look at just the Lake Trout
trout <- fishing %>%
  select(year, lake, species, grand_total) %>%
  distinct() %>%
  filter(species == "Lake Trout") %>%
  group_by(lake, year) %>%
  summarize(grand_total = sum(grand_total, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    year > 1915,
    lake %in% c("Erie", "Huron", "Ontario", "Superior")
  ) 

p_trout <- trout %>%
  ggplot() +
  aes(x = year, y = grand_total, color = lake, fill = lake) +
  geom_line(lwd = 1) +
  xlab("Year") +
  ylab("Number of Fish") +
  labs(color = "Lake") +
  ggtitle("Number of Lake Trout Since 1916") +
  scale_y_continuous(labels = label_thousands) +
  scale_color_manual(
    values = c(
      "#EF476F",
      "#FFD166",
      "#06D6A0",
      "#118AB2"
    )
  ) +
  theme_classic() +
  guides(
    color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Goudy Old Style"
    ),
    axis.text = element_text(family = "Goudy Old Style"),
    axis.title = element_text(family = "Goudy Old Style"),
    legend.text = element_text(family = "Goudy Old Style"),
    legend.title = element_text(family = "Goudy Old Style"),
    legend.position = c(0.6, 0.8),
    legend.background = element_rect(color = "#073B4C")
  )


## Put together the final infographic
p_num / p_pct / (p_white | p_trout) + plot_layout(heights = c(0.5, 0.5, 0.25))

