#### 2021-08-31 ----

## Setup ----

library(ggplot2)
library(dplyr)
library(patchwork)
library(extrafont)
suppressMessages(loadfonts(device = "win"))
library(forcats)


tuesdata <- tidytuesdayR::tt_load('2021-08-31')

bird_baths <- tuesdata$bird_baths


## Explore ----

colnames(bird_baths)
head(bird_baths)
nrow(bird_baths)

bird_baths %>% group_by(survey_year) %>% summarize(count = sum(bird_count)) %>% arrange(desc(count))
bird_baths %>% group_by(urban_rural) %>% summarize(count = sum(bird_count)) %>% arrange(desc(count))
bird_baths %>% group_by(bioregions) %>% summarize(count = sum(bird_count)) %>% arrange(desc(count))
bird_baths %>% group_by(bird_type) %>% summarize(count = sum(bird_count)) %>% arrange(desc(count))
summary(bird_baths$bird_count)

grouped <- bird_baths %>%
  dplyr::group_by(
    urban_rural,
    bioregions,
    bird_type
  ) %>%
  dplyr::summarize(bird_count = sum(bird_count)) %>%
  ungroup()

## Visualize ----

## Get the 10 most common regions with the 10 most common birds in those regions

regions <- grouped %>%
  filter(!is.na(bioregions)) %>%
  group_by(bioregions) %>%
  summarize(bird_count = sum(bird_count))
## Just kidding, there are 10 regions haha

birds <- grouped %>%
  dplyr::filter(!is.na(bioregions)) %>%
  group_by(bird_type) %>%
  summarize(bird_count = sum(bird_count)) %>% 
  ungroup() %>%
  arrange(desc(bird_count)) %>%
  slice(1:10) %>%
  pull(bird_type)

p_1 <- grouped %>%
  filter(
    !is.na(bioregions),
    bird_type %in% birds,
    bird_count > 0
  ) %>%
  ggplot() +
  aes(x = urban_rural, y = bird_count, fill = urban_rural) +
  geom_bar(stat = "identity", width = 0.6) +
  facet_grid(bird_type ~ bioregions, switch = "y") +
  theme_classic() +
  xlab("Urban (blue) vs. Rural (brown") +
  scale_fill_manual(values = c("#8D4B3F", "#1FA6C1")) +
  theme(
    text = element_text(
      family = "Georgia"
    ),
    legend.position = "None",
    axis.text = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    # strip.text.x = element_text(angle = 270),
    strip.background = element_rect(
      color = NA,
      fill = "#D5C9AD"
    ),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.background = element_rect(
      color = NA,
      fill = "#CAB0A3"
    ),
    panel.background = element_rect(
      color = NA,
      fill = "#DFE2B6"
    )
  )

# how many birds were sighted?
sum(grouped$bird_count)

info_1 <- ggplot() +
  geom_text(
    aes(x = 1, y = 1, label = "11,000"),
    family = "Georgia",
    size = 20,
    color = "#1A889E",
    hjust = 0
  ) +
  geom_text(
    aes(x = 1, y = 2, label = "Over"),
    family = "Georgia",
    size = 10,
    color = "#214E34",
    vjust = 1,
    hjust = 0
  ) +
  geom_text(
    aes(x = 1, y = 0, label = "Birds Sighted"),
    family = "Georgia",
    color = "#214E34",
    size = 10,
    hjust = 0
  ) +
  theme_void() +
  xlim(c(0.5, 2)) +
  ylim(c(-1, 3)) +
  theme(
    panel.background = element_rect(
      color = NA,
      fill = "#DABEC7"
    ),
    plot.background = element_rect(
      color = NA,
      fill = "#DABEC7"
    )
  )

p_2 <- grouped %>%
  group_by(bird_type) %>%
  summarize(bird_count = sum(bird_count)) %>%
  ungroup() %>%
  arrange(desc(bird_count)) %>%
  slice(1:10) %>%
  mutate(bird_type = fct_reorder(.f = bird_type, .x = bird_count)) %>%
  ggplot() +
  aes(x = bird_count, y = bird_type) +
  geom_bar(stat = "identity", fill = "#214E34") +
  theme_classic() +
  xlab("Number Spotted") +
  ggtitle("10 Most Prevalent Birds") +
  theme(
    axis.ticks = element_blank(),
    text = element_text(family = "Georgia"),
    axis.title.y = element_blank(),
    plot.background = element_rect(
      color = NA,
      fill = "#DABEC7"
    ),
    panel.background = element_rect(
      color = NA,
      fill = "#DABEC7"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_blank()
  ) +
  coord_cartesian(expand = FALSE)

p_3 <- grouped %>%
  filter(!is.na(bioregions)) %>%
  group_by(bioregions) %>%
  summarize(bird_count = sum(bird_count)) %>%
  ungroup() %>%
  arrange(desc(bird_count)) %>%
  slice(1:10) %>%
  mutate(bioregions = fct_reorder(.f = bioregions, .x = bird_count)) %>%
  ggplot() +
  aes(x = bird_count, y = bioregions) +
  geom_bar(stat = "identity", fill = "#214E34") +
  theme_classic() +
  xlab("Number Spotted") +
  ggtitle("10 Most Prevalent Bio-Regions") +
  theme(
    axis.ticks = element_blank(),
    text = element_text(family = "Georgia"),
    axis.title.y = element_blank(),
    plot.background = element_rect(
      color = NA,
      fill = "#DABEC7"
    ),
    panel.background = element_rect(
      color = NA,
      fill = "#DABEC7"
    ),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_blank()
  ) +
  coord_cartesian(expand = FALSE)

p_3





(p_3 | p_2) + p_1


