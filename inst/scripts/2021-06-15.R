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

## Load the data
tuesdata <- tidytuesdayR::tt_load('2021-06-15')

tweets <- tuesdata$tweets

#### Explore the tweets data ----

View(tweets)

# I think we should do some sort of map with this

world <- ne_countries(scale = "medium", returnclass = "sf")

most_liked <- tweets %>%
  filter(like_count == max(like_count, na.rm = TRUE))

p <- ggplot(data = world) +
  geom_sf(fill = "#FCD7AD") +
  theme_void() +
  # xlab("Longitude") +
  # ylab("Latitude") +
  coord_sf(xlim = c(-126.01, -64.12), ylim = c(24.50, 51.87), expand = FALSE) +
  # annotation_scale(location = "bl", width_hint = 0.2) +
  # annotation_north_arrow(
  #   location = "bl",
  #   which_north = "true", 
  #   pad_x = unit(0.1, "in"), 
  #   pad_y = unit(0.1, "in"),
  #   style = north_arrow_fancy_orienteering
  # ) +
  geom_point(
    data = tweets,
    aes(
      x = long,
      y = lat,
      size = like_count
    ),
    alpha = 0.5,
    color = "#66A182"
  ) +
  labs(size = "# of Likes") +
  geom_curve(
    aes(
      xend = most_liked$long + 1,
      yend = most_liked$lat + 1,
      x = most_liked$long + 12,
      y = most_liked$lat + 8
    ),
    curvature = -0.2,
    size = 1,
    color = "#C84148",
    arrow = arrow()
  ) +
  ggtext::geom_textbox(
    data = most_liked,
    aes(
      x = long,
      y = lat,
      label = str_c(content) %>%
        str_replace_all("/", "&#47;")
    ),
    nudge_x = 12,
    nudge_y = 8,
    family = "Segoe Print",
    fill = "#F6E0E1",
    height = unit(1.5, "in"),
    width = unit(3.5, "in"),
    box.colour = "#C84148",
    box.size = 1
    
  ) +
  ggtitle("Tweets About the #DuBoisChallenge", "2021-02-07 to 2021-05-07") +
  theme(
    panel.background = element_rect(fill = "#BBD7DD", color = NA),
    legend.position = c(0.92, 0.25),
    legend.background = element_rect(fill = "#F6E0E1", color = "#403D58"),
    plot.title = element_text(
      hjust = 0.5,
      family = "Segoe Print"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Segoe Print"
    ),
    plot.background = element_rect(
      fill = "#F6E0E1",
      color = NA
    ),
    legend.title = element_text(
      family = "Segoe Print"
    ),
    legend.text = element_text(
      family = "Segoe Print"
    )
  ) +
  guides(
    size = guide_legend(
      title.hjust = 0.5,
      title.position = "top"
    )
  ) +
  scale_radius(range = c(5, 20))


p


ggsave(
  filename = "inst/visualizations/2021-06-15.png",
  plot = p,
  height = 8,
  width = 12
)
