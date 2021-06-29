## TidyTuesday data for 2021-06-15 ----

#### Setup ----

## Load Libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)
suppressMessages(loadfonts(device = "win"))
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)
library(ggtext)
library(lubridate)
library(forcats)

## Load the data
tuesdata <- tidytuesdayR::tt_load('2021-06-29')

animal_rescue <- tuesdata$animal_rescues

#### Explore the animal_rescue data ----

colnames(animal_rescue)
View(animal_rescue)

animal_rescue %>% count(animal_group_parent) %>%
  arrange(desc(n))

new_animal <- animal_rescue %>%
  mutate(
    month = floor_date(dmy(str_remove(date_time_of_call, " .*$")), unit = "month"),
    hour = as.numeric(str_remove(str_remove(date_time_of_call, "^.* "), ":.*$")),
    animal_group_parent = fct_lump(animal_group_parent, 5),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  select(
    year = cal_year,
    month,
    hour,
    incident_notional_cost,
    animal_group_parent,
    property_category,
    origin_of_call = originof_call,
    latitude,
    longitude
  ) %>%
  filter(latitude > 10)


animal_rescue %>%
  mutate(month = floor_date(dmy(str_remove(date_time_of_call, " .*$")), unit = "month")) %>% 
  group_by(cal_year) %>%
  count() %>%
  ungroup() %>%
  ggplot() +
  aes(x = cal_year, y = n) +
  geom_line()


p1 <- new_animal %>%
  group_by(
    property_category,
    origin_of_call,
    animal_group_parent
  ) %>%
  count() %>%
  ungroup() %>%
  group_by(
    property_category,
    origin_of_call
  ) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = "", y = pct, fill = animal_group_parent) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  facet_grid(property_category ~ origin_of_call) +
  labs(fill = "") +
  theme_void() +
  scale_fill_manual(
    values = c(
      "#81717A",
      "#004643",
      "#D4560C",
      "#7D9D8B", 
      "#D1AC00",
      "#F6BE9A"
    )
  ) +
  theme(
    legend.text = element_text(
      family = "Modern No. 20"
    ),
    legend.title = element_blank(),
    strip.text.x = element_text(
      family = "Modern No. 20",
      angle = 45
    ),
    strip.text.y = element_text(
      family = "Modern No. 20"
    ),
    legend.position = c(0.2, 0.1),
    legend.background = element_rect(
      fill = NA,
      color = "#004643"
    )
  ) +
  guides(
    fill = guide_legend(
      nrow = 2
    )
  )
p1

world <- ne_countries(scale = "medium", returnclass = "sf")  

ggplot(data = world) +
  geom_sf(fill = "#FCD7AD") +
  theme_void() +
  coord_sf(xlim = c(-0.50, 0.50), ylim = c(51.3, 51.7), expand = FALSE) +
  geom_point(
    data = new_animal,
    aes(x = longitude, y = latitude)
  )

# Might be nice to add a map, compose a pretty neat infographic. I could spend a while more on this one





