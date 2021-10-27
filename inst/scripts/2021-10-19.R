#### Tidy Tuesday for 2021-10-19 ----

## Setup ----

library(ggplot2)
library(dplyr)
library(patchwork)
library(extrafont)
library(tidyr)
suppressMessages(loadfonts())
library(forcats)
library(stringr)

tuesdata <- tidytuesdayR::tt_load('2021-10-19')

pumpkins <- tuesdata$pumpkins

View(pumpkins)

## EDA ----

pumpkins %>%
  mutate(weight_lbs = as.numeric(weight_lbs)) %>%
  pull(weight_lbs) %>%
  summary()

pumpkins %>%
  mutate(weight_lbs = as.numeric(weight_lbs)) %>%
  ggplot() +
  aes(x = weight_lbs) +
  geom_density()

pumpkins <- pumpkins %>%
  mutate(
    weight_lbs = as.numeric(weight_lbs),
    year = str_sub(id, 1, 4),
    category = str_sub(id, -1, -1),
    grower_first_name = str_extract(grower_name, ",.*$") %>%
      str_remove_all(",") %>%
      str_squish(),
    grower_last_name = str_extract(grower_name, "^.*,") %>%
      str_remove_all(",") %>%
      str_squish()
  ) %>%
  filter(!is.na(weight_lbs)) %>%
  select(
    id,
    year,
    category,
    place,
    weight_lbs,
    grower_first_name,
    grower_last_name,
    state_prov,
    country
  )

giant_pumpkin <- pumpkins %>%
  filter(category == "P")
watermelon <- pumpkins %>%
  filter(category == "W")


plot(density(watermelon$weight_lbs))

watermelon %>%
  count(country)

top_3 <- c("Canada", "Italy", "United States")

watermelon %>%
  filter(country %in% top_3) %>%
  ggplot() +
  aes(x = weight_lbs, fill = country) +
  geom_density() +
  xlab("Weight (lbs)") +
  facet_wrap(~country, ncol = 1) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

# I think I'd rather have the three as separate graphs
# That way there is more flexibility

watermelon %>%
  mutate(place = as.numeric(place)) %>%
  filter(place <= 10) %>%
  count(grower_first_name) %>%
  arrange(desc(n))


watermelon %>%
  mutate(place = as.numeric(place)) %>%
  filter(place <= 10) %>%
  count(grower_last_name) %>%
  arrange(desc(n))

watermelon %>%
  filter(grower_last_name == "Ansems") %>%
  View()

watermelon %>%
  mutate(place = as.numeric(place)) %>%
  filter(place <= 10, country == "United States") %>%
  ggplot() +
  geom_boxplot(
    aes(x = factor(year), y = weight_lbs)
  ) +
  geom_smooth(
    aes(x = as.integer(factor(year)), y = weight_lbs),
    alpha = 0.08,
    fill = "red",
    color = "red",
    method = "loess"
  )

# watermelon %>%
#   mutate(
#     place = as.numeric(place),
#     year = as.numeric(year)
#   ) %>%
#   filter(place <= 10) %>%
#   group_by(year) %>%
#   summarize(
#     max_wt = max(weight_lbs),
#     q75 = quantile(weight_lbs, 0.75),
#     med_wt = median(weight_lbs),
#     q25 = quantile(weight_lbs, 0.25),
#     min_wt = min(weight_lbs),
#     mean_wt = mean(weight_lbs)
#   ) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_point(
#     aes(x = year, y = med_wt)
#   ) +
#   geom_smooth(
#     aes(x = year, y = med_wt),
#     se = FALSE
#   )

## Plot Creation ----


watermelon %>%
  mutate(place = as.numeric(place)) %>%
  filter(place <= 15, country == "United States") %>%
  ggplot() +
  geom_boxplot(
    aes(x = factor(year), y = weight_lbs),
    color = "#FA7921",
    fill = "#FEBF71"
  ) +
  geom_smooth(
    aes(x = as.integer(factor(year)), y = weight_lbs),
    alpha = 0.08,
    fill = "#566E3D",
    color = "#440F0E",
    method = "loess"
  ) +
  theme_classic() +
  xlab("Year") +
  ylab("Weight (lbs)") +
  ggtitle(
    "US Watermelon Weights",
    "Top 15 by Year"
  ) +
  theme(
    text = element_text(
      family = "Times New Roman",
      color = "#440F0E"
    ),
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    panel.background = element_rect(
      fill = "#E2D9B6"
    ),
    plot.background = element_rect(
      fill = "#E2D9B6"
    )
  )


