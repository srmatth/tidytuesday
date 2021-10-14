#### 2021-10-12 ----

## Setup ----

library(ggplot2)
library(dplyr)
library(patchwork)
library(extrafont)
library(tidyr)
suppressMessages(loadfonts())
library(forcats)


tuesdata <- tidytuesdayR::tt_load('2021-10-12')

names(tuesdata)

# This one looks like the fisheries vs. aquaculture
capture_fisheries_vs_aquaculture <- tuesdata$`capture-fisheries-vs-aquaculture`
# Per-Capita consumption of fish by year and country
fish_and_seafood_consumption <- tuesdata$`fish-and-seafood-consumption-per-capita`

# This one is just a repeat of what is in the first data frame
# capture_fishery_production <- tuesdata$`capture-fishery-production`

# This could be useful, especially for the entire world data
fish_stocks_withing_sustainable_levels <- tuesdata$`fish-stocks-within-sustainable-levels`

# Breakdown by types of fish
seafood_and_fish_production <- tuesdata$`seafood-and-fish-production-thousand-tonnes`

# Reasons people are catching fish on a global scale since 1950
global_fish_catchery <- tuesdata$`global-fishery-catch-by-sector`

#### Data Exploration ----

catchery <- global_fish_catchery %>%
  select(-Entity, -Code) %>%
  pivot_longer(
    cols = c(
      "Artisanal (small-scale commercial)",
      "Discards",
      "Industrial (large-scale commercial)",
      "Recreational",
      "Subsistence"
    ),
    names_to = "type",
    values_to = "metric_tons"
  )
catchery %>%
  ggplot() +
  aes(x = Year, y = metric_tons, color = type) +
  geom_line()

cap_vs_aq <- capture_fisheries_vs_aquaculture %>%
  magrittr::set_colnames(
    c(
      "entity",
      "code",
      "year",
      "Aquaculture",
      "Capture"
    )
  ) %>%
  filter(code == "OWID_WRL") %>%
  pivot_longer(
    cols = c("Aquaculture", "Capture"),
    names_to = "production_type",
    values_to = "metric_tons"
  )

cap_vs_aq %>%
  ggplot() +
  aes(x = year, y = metric_tons, color = production_type) +
  geom_line()

fish_and_seafood_consumption %>%
  magrittr::set_colnames(
    c("entity", "code", "year", "consumption")
  ) %>%
  filter(code == "OWID_WRL") %>%
  ggplot() +
  aes(x = year, y = consumption) +
  geom_line()


fish_stocks_withing_sustainable_levels %>%
  magrittr::set_colnames(
    c("entity", "code", "year", "sustainable", "exploited")
  ) %>%
  filter(entity == "World") %>%
  tidyr::pivot_longer(
    cols = c("sustainable", "exploited"),
    names_to = "state",
    values_to = "value"
  ) %>%
  ggplot() +
  aes(x = year, y = value, color = state) +
  geom_line()


#### Final Plot Creation ----


p1 <- cap_vs_aq %>%
  ggplot() +
  aes(x = year, y = metric_tons, color = production_type) +
  geom_line(lwd = 1.5) +
  theme_bw() +
  xlab("Year") +
  ylab("Amount of Fish (Metric Tons)") +
  labs(color = "Type of Fish Production") +
  ggtitle("Fish Production Type over Time") +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    legend.position = c(0.85, 0.3),
    plot.title = element_text(
      hjust = 0.5
    ),
    legend.background = element_rect(
      color = "black",
      fill = "#F1E4E7"
    ),
    panel.background = element_rect(
      fill = "#F1E4E7"
    ),
    plot.background = element_rect(
      fill = "#F6F4F5",
      color = NA
    )
  ) +
  scale_color_manual(
    values = c("#A3A2CD", "#04776F")
  ) +
  scale_y_continuous(
    labels = function(x) {
      stringr::str_c(x/1000000, " Million")
    }
  )

p1
# Maybe add a text box about the intersection of these lines when I get a chance



p2 <- fish_and_seafood_consumption %>%
  magrittr::set_colnames(
    c("entity", "code", "year", "consumption")
  ) %>%
  filter(code == "OWID_WRL") %>%
  ggplot() +
  aes(x = year, y = consumption) +
  geom_line(
    color = "#54C6EB",
    lwd = 1
  ) +
  theme_bw() +
  xlab("Year") +
  ylab("Fish and Seafood Consumption (kg per capita)")+
  ggtitle("Change in Fish and Seafood Consumption Over Time") +
  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    plot.title = element_text(
      hjust = 0.5
    ),
    panel.background = element_rect(
      fill = "#EEFBFB"
    ),
    plot.background = element_rect(
      fill = "#F6F4F5",
      color = NA
    )
  )

p2

p3 <- catchery %>%
  ggplot() +
  aes(x = Year, y = metric_tons, color = type) +
  geom_line(lwd = 1) +
  theme_bw() +
  xlab("Year") +
  ylab("Amount of Fish (Metric Tons)") +
  labs(color = "Reason for Fish Production") +
  ggtitle("Fish Production Reason over Time") +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    legend.position = c(0.7, 0.45),
    plot.title = element_text(
      hjust = 0.5
    ),
    legend.background = element_rect(
      color = "black",
      fill = "#EEFBFB"
    ),
    panel.background = element_rect(
      fill = "#EEFBFB"
    ),
    plot.background = element_rect(
      fill = "#F6F4F5",
      color = NA
    )
  ) +
  scale_color_manual(
    values = c("#A3A2CD", "#04776F", "#06EFB1", "#54C6EB", "#2A4750")
  ) +
  scale_y_continuous(
    labels = function(x) {
      stringr::str_c(x/1000000, " Million")
    }
  )

p3


p4 <- fish_stocks_withing_sustainable_levels %>%
  magrittr::set_colnames(
    c("entity", "code", "year", "Sustainably Caught", "Over-Exploited")
  ) %>%
  filter(entity == "World") %>%
  tidyr::pivot_longer(
    cols = c("Sustainably Caught", "Over-Exploited"),
    names_to = "state",
    values_to = "value"
  ) %>%
  ggplot() +
  aes(x = year, y = value, color = state) +
  geom_line(lwd = 1) +
  theme_bw() +
  xlab("Year") +
  ylab("Percent of Fish Species") +
  labs(color = "Fish Species State") +
  ggtitle("Fish Species Status Over Time") +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    legend.position = c(0.2, 0.5),
    plot.title = element_text(
      hjust = 0.5
    ),
    legend.background = element_rect(
      color = "black",
      fill = "#EEFBFB"
    ),
    panel.background = element_rect(
      fill = "#EEFBFB"
    ),
    plot.background = element_rect(
      fill = "#F6F4F5",
      color = NA
    )
  ) +
  scale_color_manual(
    values = c("#54C6EB", "#2A4750")
  ) 

p4

current_pct <- capture_fisheries_vs_aquaculture %>%
  filter(Year == 2018, !is.na(Code)) %>%
  magrittr::set_colnames(
    c("country", "code", "year", "aquaculture", "capture")
  ) %>%
  mutate(
    tot_prod = aquaculture + capture,
    pct_aquaculture = aquaculture / tot_prod
  ) %>%
  filter(tot_prod > 1000000) %>%
  mutate(
    country = fct_reorder(country, -pct_aquaculture)
  )

p5 <- current_pct %>%
  ggplot() +
  aes(x = country, y = pct_aquaculture) +
  geom_bar(stat = "identity", fill = "#A3A2CD") +
  theme_bw() +
  xlab("Country") +
  ylab("Percent of Fish Production from Aquaculture") +
  ggtitle("2018 Aquaculture by Country (>1,000,000 metric tons)") +
  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    plot.title = element_text(
      hjust = 0.5
    ),
    panel.background = element_rect(
      fill = "#F1E4E7"
    ),
    plot.background = element_rect(
      fill = "#F6F4F5",
      color = NA
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  )
p5

## Create total plot

p_tot <- p1 / (p2 | p3 | p4) / p5 +
  plot_annotation(
    title = 'Fish Farming on the Rise',
    subtitle = 'Driven by Consumer Demand, the Industrial Industry, and Over-Fishing Concerns',
    theme = theme(
      text = element_text(
        family = "Times New Roman"
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 28
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 24
      ),
      plot.background = element_rect(
        fill = "#F6F4F5",
        color = NA
      )
    )
  )

p_tot
