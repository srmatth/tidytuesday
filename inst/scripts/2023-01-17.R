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

tuesdata <- tidytuesdayR::tt_load('2023-01-17')

artists <- tuesdata$artists

View(artists)

## Exploration ----

## Let's maybe just start off with American Artists and look specifically
## at the time since 2000

us_artists <- artists %>%
  filter(artist_nationality == "American" & year > 1999 &
           artist_gender != "N/A") 

ggplot(us_artists) +
  aes(x = year, y = space_ratio_per_page_total, color = artist_race_nwi) +
  geom_jitter(alpha = 0.7) +
  facet_grid(book~artist_gender) +
  theme_bw() +
  xlab("Year") +
  ylab("Space Ratio in Textbook") +
  labs(color = "Race") +
  scale_color_viridis_d()

## Maybe we can look at unique artists in each country to see country 
## representation in the textbooks
## Let's just look at Gardner for the 16th edition that came out in 2020

gardner_2020 <- artists %>%
  filter(artist_nationality != "N/A", edition_number == 16, book == "Gardner") %>%
  select(artist_name, artist_nationality, artist_gender,
         artist_race) %>%
  distinct() %>%
  group_by(artist_nationality) %>%
  summarize(
    count = n(),
    pct_male = mean(artist_gender == "Male"),
    pct_white = mean(artist_race == "White")
  ) %>%
  arrange(desc(count))

ggplot(gardner_2020) +
  aes(x = pct_male, y = pct_white, size = count) +
  geom_point()



## USA over time
usa_time <- artists %>%
  filter(artist_nationality == "American", book == "Gardner") %>%
  select(artist_name, artist_nationality, artist_gender,
         artist_race, edition_number, year) %>%
  distinct() %>%
  group_by(edition_number, year) %>%
  summarize(
    count = n(),
    pct_male = mean(artist_gender == "Male"),
    pct_white = mean(artist_race == "White")
  ) %>%
  arrange(desc(count))

ggplot(usa_time) +
  aes(x = year, y = pct_male, size = count) +
  geom_point() +
  ylim(c(0.5, 1))

ggplot(usa_time) +
  aes(x = year, y = pct_white) +
  geom_line() +
  geom_point(aes(size = count), color = "blue") +
  ylim(c(0.7, 1)) +
  geom_hline(
    yintercept = 0.758,
    lty = "dashed",
    color = "red"
  ) +
  theme_bw() +
  xlab("Year") +
  ylab("% of American Artists in Garnder that were White")
