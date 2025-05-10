library(tidyverse)
library(janitor)
library(showtext)
library(ggsci)
library(ggrepel)
library(ggtext)
library(lubridate)

## check for formating
#https://r-graph-gallery.com/web-streamchart-with-ggstream.html

setwd("C:/Developer")

# load data to be updated
# https://www.abs.gov.au/statistics/measuring-what-matters/measuring-what-matters-themes-and-indicators/prosperous/wages#:~:text=Real%20wages%20are%20calculated%20by,previous%20year%2C%20seasonally%20adjusted).

dat_raw <- read_csv("Real wage growth.csv") |>
           clean_names()

dat <- dat_raw |>
  select(date, cpi = cpi_annual_growth_percent, wage = annual_real_wage_growth_percent) %>% 
  mutate(
    date = as.Date(as.character(date), format = "%d/%m/%Y", origin = "1899-12-30"),
    gap = cpi - wage,
    wage = ifelse(gap < 0, cpi, wage)   # tp prevent overlap
    ) |>
  pivot_longer(!c(date, gap), names_to = "type", values_to = "val")

## smooth data
p <- ggplot() +
  geom_smooth(
    data = dat,
    aes(
      x = date,
      y = val,
      group = type
    ),
    se = FALSE,
    span = 0.15
  )  

# Build the plot to extract the data
plot_data <- ggplot_build(p)

# Extract the smoothed data
smoothed_data <- plot_data$data[[1]] |>
                select(x, y, group) |>
                mutate(type = ifelse(group == 1, "cpi", "wage"),
                       date = rep(unique(dat$date)[-1],2),
                       gap = rep(dat$gap[dat$type=="cpi"][-1],2)
                       ) |>
                select(-group, - x) 

cpi_df <- smoothed_data %>% 
            filter(type == "cpi") |>
            rename(cpi = y, type_cpi = type)

wage_df <- smoothed_data %>% 
            filter(type == "wage") |>
            rename(wage = y, type_wage = type)

plt_dat <- cpi_df |>
            left_join(wage_df, join_by(date, gap))

# Filter the last values and add onto the line plot
#data_ends <- df_long %>% filter(Species == "virginica")

## Chart

## theame
# theme_set(theme_minimal(
#   #base_family = "Reem Kufi", 
#   base_size = 12))
# 
# theme_update(
#   plot.title = element_text(
#     size = 25,
#     face = "bold",
#     hjust = .5,
#     margin = margin(10, 0, 30, 0)
#   ),
#   plot.caption = element_text(
#     size = 9,
#     color = "grey40",
#     hjust = .5,
#     margin = margin(20, 0, 5, 0)
#   ),
#   axis.text.y = element_blank(),
#   axis.title = element_blank(),
#   plot.background = element_rect(fill = "grey88", color = NA),
#   panel.background = element_rect(fill = NA, color = NA),
#   panel.grid = element_blank(),
#   panel.spacing.y = unit(0, "lines"),
#   strip.text.y = element_blank(),
#   legend.position = "bottom",
#   legend.text = element_text(size = 9, color = "grey40"),
#   legend.box.margin = margin(t = 30), 
#   legend.background = element_rect(
#     color = "grey40", 
#     size = .3, 
#     fill = "grey95"
#   ),
#   legend.key.height = unit(.25, "lines"),
#   legend.key.width = unit(2.5, "lines"),
#   plot.margin = margin(rep(20, 4))
# )

# Define the color palette
pal <- c(
  "#FFB400", colorspace::lighten("#FFB400", .25, space = "HLS"),
  "#C20008", colorspace::lighten("#C20008", .2, space = "HLS"),
  "#13AFEF", colorspace::lighten("#13AFEF", .25, space = "HLS"),
  "#8E038E", colorspace::lighten("#8E038E", .2, space = "HLS"),
  "#595A52", colorspace::lighten("#595A52", .15, space = "HLS")
)





ggplot(plt_dat, aes(x = date)) +
  geom_line(aes(y = cpi, color = "CPI"), size = 1.5) +
  geom_line(aes(y = wage, color = "Wage"), size = 1.5) +
  geom_ribbon(aes(ymin = pmin(cpi, wage), ymax = pmax(cpi, wage)), 
             # fill = "grey70",
              #fill = plt_dat$gap,
              alpha = 0.5) +
  scale_color_manual(
    values = c("CPI" = pal[3], "Wage" = pal[5]),
    expand = c(0, 0),
    guide = "none"
    ) +
  scale_fill_gradient2(low='red', mid="gray37", high = "black", 
                       space ="Lab",midpoint = mean(plt_dat$gap)/2) +
  ylab(NULL) +
  xlab(NULL) +
  # geom_text_repel(
  #   aes(label = "Real wage"), data = plt_dat |> tail(1) ,
  #   fontface ="plain", color = "black", size = 3
  # ) +
  # geom_text_repel(
  #   aes(x = "2024-06-01", y = 1.111968, label = "Real wage"), nudge_x = 0.1, direction = "y", hjust = "left"
  # ) +
  theme_minimal( base_size = 12) +
  labs(
    title = "***Have wages kept up with higher prices?***",
    subtitle = "**Annual real wage growth vs CPI annual growth***",
    caption = 'Data: ABS \n Chart: Regional Workforce Assessment') +
  theme(
    plot.title = ggtext::element_textbox_simple(
      size = 18, margin = margin(t = 12)),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 10, margin = margin(b = 12)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, color = "gray50"),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = pal[10],
                                      size = 0.5,
                                      linetype = 2)
    #panel.grid.major.x = element_blank(),
    #

  )


 
