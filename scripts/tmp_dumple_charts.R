
## Horizontal error bar
# https://juliasilge.com/blog/bird-baths/
# also check: https://otho.netlify.app/post/2019-02-02-butter-consumption/
# https://r-charts.com/distribution/dumbbell-plot-ggplot2/
# https://rpubs.com/ageek/ggplot-adv-part2
# https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html
# https://www.cedricscherer.com/2019/05/17/the-evolution-of-a-ggplot/

library(tidyverse)

# Example 1 ----
bird_baths <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv")

top_birds <-
  bird_baths %>%
  filter(is.na(urban_rural)) %>%
  arrange(-bird_count) %>%
  slice_max(bird_count, n = 15) %>%
  pull(bird_type)
       
bird_parsed <-
  bird_baths %>%
  filter(
    !is.na(urban_rural),
    bird_type %in% top_birds
  ) %>%
  group_by(urban_rural, bird_type) %>%
  summarise(bird_count = mean(bird_count), .groups = "drop")

p1 <-
  bird_parsed %>%
  ggplot(aes(bird_count, bird_type)) +
  geom_segment(
    data = bird_parsed %>%
      pivot_wider(
        names_from = urban_rural,
        values_from = bird_count
      ),
    aes(x = Rural, xend = Urban, y = bird_type, yend = bird_type),
    alpha = 0.7, color = "gray70", size = 1.5
  ) +
  geom_point(aes(color = urban_rural), size = 3) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Probability of seeing bird", y = NULL, color = NULL)

p1


# Example 2 ----
# Example 3 ----
# link # also check: https://otho.netlify.app/post/2019-02-02-butter-consumption/
library(tidyverse)
library(tibbletime)
library(scico)

dat_path <- "_data/2-05-milk-product-facts.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "2019/2019-01-29/milk_products_facts.csv")

dat_milkprods <- read_csv(dat_url)

# roll percent over a dataframe
roll_percent <- rollify(.f = function(n) (n[2] - n[1])*100/n[1], 2)

dat <- 
  dat_milkprods %>%
  select(year, butter) %>% 
  # apply on this dataframe, on the column butter
  mutate(percent = roll_percent(butter)) %>% 
  filter(complete.cases(.))

# a limit that centers the divergent palette
lim <- 
  dat$percent %>% 
  range() %>% 
  abs() %>% 
  max()

theme_set(
  theme_minimal() +
    theme(text = element_text(family = "Arial Narrow",
                              colour = "grey40",
                              size = 11),
          axis.title = element_text(size = 14),
          plot.title = element_text(colour = "grey20",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(face = "bold",
                                       size = 12),
          aspect.ratio = .6,   
          plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                               unit = "mm"))
)
p <- 
  dat %>% 
  mutate(yend = butter + (percent/10)) %>% 
  ggplot(aes(x = year,
             y = butter))
p

plt <- 
  p +
  # First the annotations
  annotate(geom = "rect",
           xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = .5) +
  annotate(geom = "text",
           x = 2009, y = 4,
           label = "2008\nEconomic Crisis?",
           family = "Arial Narrow",
           colour = "grey40",
           size = 3, fontface = "bold") +
  # and then the basic geometric objects
  geom_segment(aes(yend = yend,
                   xend = ..x..,
                   colour = percent),
               size = 2,
               arrow = arrow(length = unit(1.2, "mm"),
                             type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
  geom_text(aes(y = case_when(percent > 0 ~ yend + .12,
                              TRUE ~ yend - .12),
                label = percent %>% 
                  round() %>% paste0("%"),
                colour = percent),
            size = 2.7) +
  scale_colour_scico(palette = "roma",
                     direction = 1,
                     limits = c(-lim, lim),
                     guide = FALSE)
  

plt


## packages ----
library(tidyverse)
library(ggsci)
library(showtext)

## load fonts
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

## get data
devtools::source_gist("https://gist.github.com/Z3tt/301bb0c7e3565111770121af2bd60c11")

# Plot
## calculate worldwide average
world_avg <-
  df_ratios %>%
  summarize(avg = mean(student_ratio, na.rm = TRUE)) %>%
  pull(avg)

## tile map as legend
map_regions <-
  df_ratios %>%
  mutate(region = fct_reorder(region, -student_ratio_region)) %>%
  ggplot(aes(x = x, y = y, fill = region, color = region)) +
  geom_tile(color = "white") +
  scale_y_reverse() +
  scale_fill_uchicago(guide = "none") +
  coord_equal() +
  theme_light() +
  theme(
    line = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",
                                   color = "transparent"),
    panel.border = element_rect(color = "transparent"),
    strip.background = element_rect(color = "gray20"),
    axis.text = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  labs(x = NULL, y = NULL)
## coordinates for arrows
arrows <-
  tibble(
    x1 = c(6, 3.65, 1.8, 1.8, 1.8),
    x2 = c(5.6, 4, 2.18, 2.76, 0.9),
    y1 = c(world_avg + 6, 10.5, 9, 9, 77),
    y2 = c(world_avg + 0.1, 18.4, 14.16, 12, 83.42)
  )

## final plot

## set seed to fix position of jittered points
set.seed(2019)

## final plot
df_ratios %>%
  mutate(region = fct_reorder(region, -student_ratio_region)) %>%
  ggplot(aes(x = region, y = student_ratio, color = region)) +
  geom_segment(
    aes(x = region, xend = region,
        y = world_avg, yend = student_ratio_region),
    size = 0.8
  ) +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  coord_flip() +
  annotate(
    "text", x = 6.3, y = 35, family = "Poppins",
    size = 2.7, color = "gray20",
    label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher")
  ) +
  annotate(
    "text", x = 3.5, y = 10, family = "Poppins",
    size = 2.7, color = "gray20",
    label = "Continental average"
  ) +
  annotate(
    "text", x = 1.7, y = 11, family = "Poppins",
    size = 2.7, color = "gray20",
    label = "Countries per continent"
  ) +
  annotate(
    "text", x = 1.9, y = 64, family = "Poppins",
    size = 2.7, color = "gray20",
    label = "The Central African Republic has by far\nthe most students per teacher"
  ) +
  geom_curve(
    data = arrows, aes(x = x1, xend = x2,
                       y = y1, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.3#
  ) +
  annotation_custom(
    ggplotGrob(map_regions),
    xmin = 2.5, xmax = 7.5, ymin = 52, ymax = 82
  ) +
  scale_y_continuous(
    limits = c(1, NA), expand = c(0.02, 0.02),
    breaks = c(1, seq(20, 80, by = 20))
  ) +
  scale_color_uchicago() +
  labs(
    x = NULL, y = "Student to teacher ratio",
    caption = 'Data: UNESCO Institute for Statistics'
  ) +
  theme_light(base_size = 18, base_family = "Poppins") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10),
    plot.caption = element_text(size = 9, color = "gray50"),
    panel.grid = element_blank()
  )
#############
df_sorted <-
  df_ratios %>%
  mutate(region = fct_reorder(region, -student_ratio_region))

ggplot(df_sorted, aes(x = region, y = student_ratio)) +
  geom_boxplot()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))

g <-
  ggplot(df_sorted, aes(x = region, y = student_ratio, color = region)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90), expand = c(0.02, 0.02)) +
  scale_color_uchicago() +
  labs(x = NULL, y = "Student to teacher ratio") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )
g + geom_point(size = 3, alpha = 0.15)
