## Load Packages ----
library(tidyverse)   ## data science package collection (incl. the ggplot2 package)
library(systemfonts) ## use custom fonts (need to be installed on your OS)  
library(scico)       ## scico color palettes(http://www.fabiocrameri.ch/colourmaps.php) in R 
library(ggtext)      ## add improved text rendering to ggplot2
library(ggforce)     ## add missing functionality to ggplot2
library(ggdist)      ## add uncertainity visualizations to ggplot2
library(magick)      ## load images into R
library(patchwork)   ## combine outputs from ggplot2


penguins <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv') %>% 
  ## correct species name
  mutate(species = if_else(species == "Adelie", "Adélie", species)) %>% 
  ## remove missing observations
  filter(!is.na(bill_length_mm), !is.na(bill_depth_mm))

## assign plot to `g` - we can ad new things to this plot later
## (wrapped in parenthesis so it is assigned and plotted in one step)
(gt <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point(aes(color = body_mass_g), alpha = .6, size = 3.5) + 
    scale_x_continuous(breaks = 3:6 * 10, limits = c(30, 60)) +
    scale_y_continuous(breaks = seq(12.5, 22.5, by = 2.5), limits = c(12.5, 22.5)) +
    scico::scale_color_scico(palette = "bamako", direction = -1) +
    ## markdown formatting using asterisks
    labs(
      title = 'Bill Dimensions of Brush-Tailed Penguins (*Pygoscelis*)',
      subtitle = 'A scatter plot of bill depth versus bill length.',
      caption = 'Data: Gorman, Williams & Fraser (2014) *PLoS ONE*',
      x = '**Bill Length** (mm)', 
      y = '**Bill Depth** (mm)',
      color = 'Body mass (g)'
    ) +
    ## render respective text elements
    theme(
      plot.title = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown(),
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown()
    )
)

## use HTML syntax to change text color
gt_mar <- gt +
  labs(title = 'Bill Dimensions of Brush-Tailed Penguins <i style="color:#28A87D;">Pygoscelis</i>') +
  theme(plot.margin = margin(t = 25))

gt_mar

## use HTML syntax to change font and text size
gt_mar +
  labs(title = 'Bill Dimensions of Brush-Tailed Penguins <b style="font-size:32pt;font-family:blacksword;">Pygoscelis</b>')

## rich text
gt_rich <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species), alpha = .6, size = 3.5) + 
  ## add text annotations for each species
  ggtext::geom_richtext(
    data = tibble(
      x = c(34, 56, 54), y = c(20, 18.5, 14.5),
      species = c("Adélie", "Chinstrap", "Gentoo"),
      lab = c("<b style='font-family:anton;font-size:24pt;'>Adélie</b><br><i style='color:darkgrey;'>P. adéliae</i>", 
              "<b style='font-family:anton;font-size:24pt;'>Chinstrap</b><br><i style='color:darkgrey;'>P. antarctica</i>", 
              "<b style='font-family:anton;font-size:24pt;'>Gentoo</b><br><i style='color:darkgrey;'>P. papua</i>"),
      angle = c(12, 20, 335)
    ),
    aes(x, y, label = lab, color = species, angle = angle), 
    size = 4, fill = NA, label.color = NA,
    lineheight = .3
  ) +
  scale_x_continuous(breaks = 3:6 * 10, limits = c(30, 60)) +
  scale_y_continuous(breaks = seq(12.5, 22.5, by = 2.5), limits = c(12.5, 22.5)) +
  rcartocolor::scale_color_carto_d(palette = "Bold", guide = "none") +
  labs(
    title = 'Bill Dimensions of Brush-Tailed Penguins (*Pygoscelis*)',
    subtitle = 'A scatter plot of bill depth versus bill length.',
    caption = 'Data: Gorman, Williams & Fraser (2014) *PLoS ONE*',
    x = '**Bill Length** (mm)', 
    y = '**Bill Depth** (mm)',
    color = 'Body mass (g)'
  )

(gt_rich +
    theme(
      plot.title = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown(),
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown(),
      plot.margin = margin(25, 6, 15, 6)
    )
)
  