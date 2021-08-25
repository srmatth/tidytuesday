## TidyTuesday for 2021-08-24 ----

#### Setup ----

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)
suppressMessages(loadfonts(device = "win"))
library(jpeg)
library(grid)

lemurs <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv'
) %>%
  distinct()

#### Exploration ----

top_7 <- lemurs %>%
  filter(age_category != "adult", preg_status == "NP", age_at_wt_mo < 10) %>%
  group_by(taxon) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  slice(1:7) %>%
  pull(taxon)

qplot(1:10, 1:10, geom="blank") +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  
ruffed <- readJPEG("inst/scripts/img_2021-08-24/black-and-white-ruffed-lemur.jpg")
g_ruffed <- rasterGrob(ruffed, interpolate=TRUE) 

dwarf <- readJPEG("inst/scripts/img_2021-08-24/dwarf.jpg")
g_dwarf <- rasterGrob(dwarf, interpolate=TRUE) 

ringtail <- readJPEG("inst/scripts/img_2021-08-24/ringtail.jpg")
g_ringtail <- rasterGrob(ringtail, interpolate=TRUE) 

sifaka <- readJPEG("inst/scripts/img_2021-08-24/sifaka.jpg")
g_sifaka <- rasterGrob(sifaka, interpolate=TRUE) 

p <- lemurs %>%
  filter(
    age_category != "adult", 
    preg_status == "NP",
    age_at_wt_mo < 10,
    # taxon %in% c("CMED", "LCAT")
    taxon %in% top_7,
    taxon != "DMAD",
    taxon != "MMUR",
    taxon != "VRUB"
  ) %>%
  mutate(
    taxon = case_when(
      taxon == "CMED" ~ "Fat-Tailed Dwarf Lemur",
      taxon == "LCAT" ~ "Ring-Tailed Lemur",
      taxon == "PCOQ" ~ "Coquerel's Sifaka",
      taxon == "VVV" ~ "Black and White Ruffed Lemur"
    )
  ) %>%
  ggplot() +
  aes(x = age_at_wt_mo, y = weight_g, color = taxon) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE, size = 2.25, alpha = 0.8) +
  xlab("Age (months)") +
  ylab("Weight (grams)") +
  labs(color = "Species") +
  ggtitle("Lemur Weight in First 10 Months of Life", "Selected Species") +
  scale_x_continuous(
    breaks = seq(1, 10, by = 1)
  ) +
  theme_classic() +
  guides(
    color = guide_legend(
      title.hjust = 0.5
    )
  ) +
  theme(
    text = element_text(
      family = "MV Boli"
    ),
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(
      color = "black"
    ),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  annotation_custom(g_ruffed, xmin=1, xmax=3, ymin=1000, ymax=2000) +
  annotation_custom(g_dwarf, xmin=6, xmax=8, ymin=0, ymax=800) +
  annotation_custom(g_sifaka, xmin=5, xmax=7, ymin=1200, ymax=2000) +
  annotation_custom(g_ringtail, xmin=8, xmax=10, ymin=500, ymax=1600)

ggsave("inst/visualizations/2021-08-24.pdf", plot = p, height = 10, width = 10)
