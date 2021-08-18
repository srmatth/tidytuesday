## TidyTuesday for 2021-07-27 ----

#### Setup ----

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)
suppressMessages(loadfonts(device = "win"))

tuesdata <- tidytuesdayR::tt_load('2021-07-27')

olympics <- tuesdata$olympics
regions <- tuesdata$regions

colnames(olympics)
colnames(regions)

summer <- olympics %>%
  filter(season == "Summer")
winter <- olympics %>%
  filter(season == "Winter")

#### Exploration ----

summer %>%
  group_by(year, sex) %>%
  count() %>%
  ggplot() +
  aes(x = year, y = n, color = sex) +
  geom_line()

winter %>%
  group_by(year, sex) %>%
  count() %>%
  ggplot() +
  aes(x = year, y = n, color = sex) +
  geom_line()

olympics$sport %>% unique() %>% sort()

summer %>%
  filter(sport == "Swimming") %>%
  group_by(sport, year, sex) %>%
  summarize(avg_height = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(avg_height)) %>%
  ggplot() +
  aes(x = year, y = avg_height, color = sex) +
  geom_line()


#### Visualization ----

custom_col <- list(
  purple = "#190B28",
  lavender = "#685762",
  blue = "#B7B5E4",
  green = "#9B9987",
  red = "#EF798A"
)

## Let's do an infographic focused on height

# height change over time for specific sports (basketball, volleyball, swimming)
p1 <- summer %>%
  filter(sport %in% c("Swimming", "Basketball", "Volleyball")) %>%
  group_by(sport, year, sex) %>%
  summarize(avg_height = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(avg_height)) %>%
  ggplot() +
  aes(x = year, y = avg_height, color = sex) +
  geom_line(lwd = 1.5) +
  facet_wrap(~sport, ncol = 1) +
  scale_color_manual(values = c(custom_col$purple, custom_col$green)) +
  theme_bw() +
  xlab("Year") +
  ylab("Average Medalist Height") +
  ggtitle("Height of Medalists over Time", "Basketball, Swimming, and Volleyball") +
  labs(color = "Sex") +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5
    )
  )+
  theme(
    legend.position = c(0.1, 0.85),
    text = element_text(family = "Baskerville Old Face"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    )
  )

# summer %>% filter(sport == "Basketball", sex == "M") %>% count(noc) %>% arrange(desc(n))
# summer %>%
#   filter(sport == "Basketball", sex == "M", noc %in% c("USA", "BRA", "AUS")) %>%
#   group_by(noc, year) %>%
#   summarize(height = mean(height, na.rm = TRUE)) %>%
#   ggplot() +
#   aes(x = year, y = height, color = noc) +
#   geom_line(lwd = 1.5) +
#   theme_bw()

summer %>% count(noc) %>% arrange(desc(n))

p2 <- summer %>%
  filter(noc %in% c("USA", "GBR", "FRA"), year >= 1920, sex == "M") %>%
  group_by(noc, year) %>%
  summarize(height = mean(height, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = year, y = height, color = noc) +
  geom_line(lwd = 1.5) +
  xlab("Year") +
  ylab("Average Medalist Height (cm)") +
  ggtitle("Average Men's Medalist Height by Country", "France, Great Britain, and US") +
  theme_bw() +
  scale_color_manual(values = c(custom_col$purple, custom_col$red, custom_col$green)) +
  labs(color = "Country") +
  theme(
    legend.position = c(0.8, 0.2),
    text = element_text(family = "Baskerville Old Face"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    )
  )

p3 <- summer %>%
  filter(noc %in% c("USA", "GBR", "FRA"), year >= 1920, sex == "F") %>%
  group_by(noc, year) %>%
  summarize(height = mean(height, na.rm = TRUE)) %>%
  filter(!is.na(height)) %>%
  ggplot() +
  aes(x = year, y = height, color = noc) +
  geom_line(lwd = 1.5) +
  xlab("Year") +
  ylab("Average Medalist Height (cm)") +
  ggtitle("Average Women's Medalist Height by Country", "France, Great Britain, and US") +
  theme_bw() +
  scale_color_manual(values = c(custom_col$purple, custom_col$red, custom_col$green)) +
  labs(color = "Country") +
  theme(
    legend.position = c(0.8, 0.2),
    text = element_text(family = "Baskerville Old Face"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    )
  )

p4 <- summer %>%
  filter(sport %in% c("Gymnastics", "Football", "Athletics")) %>%
  group_by(sport, year, sex) %>%
  summarize(avg_height = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(avg_height)) %>%
  ggplot() +
  aes(x = year, y = avg_height, color = sex) +
  geom_line(lwd = 1.5) +
  facet_wrap(~sport, ncol = 1) +
  scale_color_manual(values = c(custom_col$purple, custom_col$green)) +
  theme_bw() +
  xlab("Year") +
  ylab("Average Medalist Height") +
  ggtitle("Height of Medalists over Time", "Athletics, Football, Gymnastics") +
  labs(color = "Sex") +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5
    )
  )+
  theme(
    legend.position = c(0.1, 0.85),
    text = element_text(family = "Baskerville Old Face"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "#E2E1F4",
      colour = NA
    )
  )

lay <- p1 | p2 | p3 | p4

final <- lay +
  plot_annotation(
    title = 'Olympic Medalist Height over Time',
    theme = theme(
      text = element_text(family = "Baskerville Old Face"),
      plot.title = element_text(
        size = 18,
        hjust = 0.5
      ),
      plot.background = element_rect(
        fill = "#E2E1F4",
        colour = NA
      )
    )
  )

ggsave("inst/visualizations/2021-07-27.pdf", final, width = 20, height = 6.5, units = "in")

