# Plotting density charts ----


## Load Packages ----
packages <- c(
  "tidyverse",
  "forcats",
  "ggplot2",
  "ggthemes",
  "gghalves",
  "hrbrthemes",
  #"magick",     ## load images into R
#  "patchwork",  ## combine outputs from ggplot2
  "ggdist",     ## add uncertainity visualizations to ggplot2
  "ggforce",    ## add missing functionality to ggplot2
  "ggtext",     ## add improved text rendering to ggplot2
  "scico",      ## scico color palettes(http://www.fabiocrameri.ch/colourmaps.php) in R 
  "systemfonts" ## use custom fonts (need to be installed on your OS) 
)

package_check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Sample 1 ----
## Data set ----
# this is US data for Unemployment rates and earnings by educational attainment, 2022
# link to the data https://www.bls.gov/emp/tables/unemployment-earnings-education.htm
# Objective is to create one for Australia
penguins <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv') %>% 
  ## correct species name
  mutate(species = if_else(species == "Adelie", "Adélie", species)) %>% 
  ## remove missing observations
  filter(!is.na(bill_length_mm), !is.na(bill_depth_mm))

## Chart ----
# calculate bill ratio and summary stats
df_peng_stats <- 
  penguins %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  filter(!is.na(bill_ratio)) %>% 
  group_by(species) %>% 
  mutate(
    n = n(),
    median = median(bill_ratio),
    max = max(bill_ratio)
  ) %>% 
  ungroup() %>% 
  mutate(species_num = as.numeric(fct_rev(species))) 

## create a second chart with raincloud plots
p2 <- 
  ggplot(df_peng_stats, aes(bill_ratio, species_num, color = species)) +
  stat_summary(
    geom = "linerange",
    fun.min = function(x) -Inf,
    fun.max = function(x) median(x, na.rm = TRUE),
    linetype = "dotted",
    orientation = "y",
    size = .7
  ) +
  geom_point(
    aes(y = species_num - .15), 
    shape = "|",
    size = 5,
    alpha = .33
  ) +
  ggdist::stat_halfeye(
    aes(
      y = species_num,
      color = species,
      fill = after_scale(colorspace::lighten(color, .5))
    ),
    shape = 18,
    point_size = 3,
    interval_size = 1.8,
    adjust = .5,
    .width = c(0, 1)
  ) +
  geom_text(
    aes(x = median, label = format(round(median, 2), nsmall = 2)),
    stat = "unique",
    color = "white",
    family = "Open Sans",
    fontface = "bold",
    size = 3.4,
    nudge_y = .15
  ) +
  geom_text(
    aes(x = max, label = glue::glue("n = {n}")),
    stat = "unique",
    family = "Open Sans",
    fontface = "bold",
    size = 3.5,
    hjust = 0,
    nudge_x = .01,
    nudge_y = .02
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  scale_x_continuous(
    limits = c(1.6, 3.8),
    breaks = seq(1.6, 3.8, by = .2)
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:3,
    labels = c("Gentoo", "Chinstrap", "Adélie")
  ) +
  scale_color_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  scale_fill_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  labs(
    x = "Bill ratio",
    y = NULL,
    subtitle = "B. Raincloud plot showing the distribution of bill ratios, estimated as bill length divided by bill depth.",
    caption = "Data: Gorman, Williams & Fraser (2014) *PLoS ONE* &bull; Illustration: Allison Horst"
  ) +
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.minor = element_blank(),
    #panel.grid.major.x = element_line(size = .15, colour="grey50"),
    #panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 13),
    axis.ticks.length = unit(0, "lines"),
    plot.title.position = 'plot',
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.margin = margin(10, 25, 10, 25)
  )

p2

#ggsave("11_raincloud_plot.pdf", width = 9, height = 5.2, device = cairo_pdf)



# Sample 2 ----

library(palmerpenguins)
library(ggtext)
library(colorspace)
library(ragg)

url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/man/figures/lter_penguins.png"
img <- magick::image_read((url))
pic <- grid::rasterGrob(img, interpolate = TRUE)

pal <- c("#FF8C00", "#A034F0", "#159090")

add_sample <- function(x){
  return(c(y = max(x) + .025, 
           label = length(x)))
}

penguins %>% 
  group_by(species) %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  filter(!is.na(bill_ratio)) %>% 
  ggplot(aes(x = fct_rev(species), y = bill_ratio)) + 
  ggdist::stat_halfeye(
    aes(color = species,
        fill = after_scale(lighten(color, .5))),
    adjust = .5, 
    width = .75, 
    .width = 0,
    justification = -.4, 
    point_color = NA) + 
  geom_boxplot(
    aes(color = species,
        color = after_scale(darken(color, .1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .42, 
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = species,
        color = after_scale(darken(color, .1, space = "HLS"))),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  geom_point(
    aes(fill = species),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  stat_summary(
    geom = "text",
    fun = "median",
    aes(label = round(..y.., 2),
        color = species,
        color = after_scale(darken(color, .1, space = "HLS"))),
    family = "Roboto Mono",
    fontface = "bold",
    size = 4.5,
    vjust = -3.5
  ) +
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(label = paste("n =", ..label..),
        color = species,
        color = after_scale(darken(color, .1, space = "HLS"))),
    #family = "Roboto Condensed",
    size = 4,
    hjust = 0
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  #annotation_custom(pic, ymin = 2.9, ymax = 3.85, xmin = 2.7, xmax = 4.7) +
  scale_y_continuous(
    limits = c(1.57, 3.8),
    breaks = seq(1.6, 3.8, by = .2),
    expand = c(.001, .001)
  ) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  # labs(
  #   x = NULL,
  #   y = "Bill ratio",
  #   title = "Bill Ratios of Brush–Tailed Penguins (*Pygoscelis* spec.)",
  #   subtitle = "Distribution of bill ratios, estimated as bill length divided by bill depth.",
  #   caption = "Gorman, Williams & Fraser (2014) *PLoS ONE* DOI: 10.1371/journal.pone.0090081<br>Visualization: Cédric Scherer  &bull;    Illustration: Allison Horst"
  # ) +
  #theme_minimal(base_family = "Zilla Slab", base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    #axis.text.x = element_text(family = "Roboto Mono"),
    axis.text.y = element_text(
      color = rev(darken(pal, .1, space = "HLS")), 
      size = 18
    ),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    plot.title = element_markdown(face = "bold", size = 21),
    plot.subtitle = element_text(
      color = "grey40", hjust = 0,
      margin = margin(0, 0, 20, 0)
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      color = "grey40", lineheight = 1.2,
      margin = margin(20, 0, 0, 0)),
    plot.margin = margin(15, 15, 10, 15)
  )