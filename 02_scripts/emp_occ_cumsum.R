# Occupational Growth Rate ----
# for VET skill pathways skill level 2-4

## Load Packages ----
packages <- c(
  "tidyverse",
  "lubridate",
  "clipr",
  "forcats",
  "ggplot2",
  "showtext",
  "ggsci",
  "ggrepel",
  "ggtext"
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
## Parameters
TEAM_DIR <- file.path('I:', 'LMAA Branch', '01_Economic_Modelling_Team')
PROJ_DIR <- file.path(TEAM_DIR, '04_National Skills Agreement')
DATA_in <- file.path(PROJ_DIR, '00_data')

## Load funtion helpers
source('00_modules/data_utility_fn.R')

## Load data ----
# lookup
anzsco_df <- read_csv(file.path(DATA_in, '01_lookup','lookup_anzsco.csv')) %>% 
  filter(level == 1) %>% 
  rename(anzsco1_code = anzsco_code, anzsco1_name = anzsco_name) %>% 
  select(anzsco1_code, anzsco1_name)

# raw data
df_raw <- read_csv(file.path(DATA_in, '00_abs','20240412_lfs_anzsco1_sex_cleared.csv')) 

## Data Prep ----
emp_df <- df_raw %>% 
  filter(lfs_status == 1) %>%
  mutate(date_qtr = as.Date(date_qtr, '%d/%m/%Y')) %>% 
  rename(emp = weight_sum)

emp_chng_tlt <- emp_df %>% 
  group_by(anzsco1_code, date_qtr) %>%
  summarise(emp = sum(emp, na.rm = T), .groups = 'drop') %>% 
  group_by(anzsco1_code) %>% 
  mutate(
    # Quarterly change
    qtr_growth = calc_change_per(emp, date_qtr, len = 1),
    # Annual change
    annl_growth = calc_change_per(emp, date_qtr, len = 4),
    # Five year change
    fiveyr_growth = calc_change_per(emp, date_qtr, len = 20),
  ) %>% 
  ungroup()

#emp_chng_tlt %>% write_clip()

## calculate all Occupation average


gr_cv <- emp_chng_tlt %>% filter(!is.na(fiveyr_growth)) %>% 
  left_join(anzsco_df) %>% 
  relocate(anzsco1_name, .after = anzsco1_code) %>% 
  select(date_qtr, anzsco1_name,  fiveyr_growth) %>% 
  group_by(anzsco1_name) %>% 
  mutate(cv=cumsum(fiveyr_growth)) %>% 
  ungroup() %>% 
  mutate( name_lab = if_else(date_qtr == max(gr_cv$date_qtr), anzsco1_name, NA_character_))
  
# Plot -----
theme_set(theme_light(base_size = 11))

gr_cv %>%
  ggplot(
    # The ggplot object has associated the data for the highlighted countries
    aes(x = date_qtr, y = cv, group = anzsco1_name)
  ) +
  geom_line(
    aes(color = anzsco1_name),
    size = .9
  ) +
  geom_text_repel(
    aes(color = anzsco1_name, label =name_lab),
    #family = "Lato",
    #fontface = "bold",
    size = 3.5,
    direction = "y",
    # xlim = c(2020.8, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    ylim = c(-2, 9.5)
  ) +
  scale_x_date(date_labels = "%b %Y", 
               limits = c(min(gr_cv$date_qtr), max(gr_cv$date_qtr)),
               breaks = seq.Date(min(gr_cv$date_qtr), max(gr_cv$date_qtr),length.out = 10 )
  ) +
  # scale_x_continuous(
  #   expand = c(0, 0),
  #   limits = c(2011, 2024.5), 
  #   breaks = seq(2011, 2024, by = 5)
  # )# +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(-2, 9.5, by = 2.5),
    labels = scales::percent_format() 
  ) + 
  # scale_color_manual(
  #   values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50")
  # ) +
  labs(
    x = NULL, y = "Cumulative growth of 5 year growth rate",
    title = "***Cumulative Growth of 5 year Occupational Growth Rate***",
    subtitle = "***Where VET is the primary pathway (Skill Level 2-4)***",
    caption = 'Data: ABS Labour Force Survey\n Chart: JSA Economic Modelling'
  ) +
  theme(
    legend.position = "none",
    plot.title =ggtext::element_textbox_simple(size = 18, margin = margin(t = 12)
    ),
    plot.subtitle = ggtext::element_textbox_simple(size = 10, margin = margin(b = 18)
    ),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, color = "gray50"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(margin = margin(10, 0, 20, 0),size = 10), ## trbl
    panel.grid = element_blank()
  )





  

################### Other ----
gr_cv %>% 
ggplot(  aes(x = date_qtr, y = cv, colour = factor(anzsco1_name))) + 
  geom_line(size = 1)+
  geom_point()

####### sample 2 ----
## Data
df_mac_raw <- read_csv("C:\\Users\\ss3898\\Downloads\\big-mac.csv")

df_mac <- df_mac_raw %>% 
  # Extract year
  mutate(year = lubridate::year(date)) %>% 
  # Subset variables
  select(date, year, iso_a3, currency_code, name, dollar_price) %>% 
  # If there is more than one record per year/country, use the mean 
  group_by(iso_a3, name, year) %>% 
  summarize(price = mean(dollar_price)) %>% 
  # Keep countries/regions with records for the last 21 years  
  # (from 2000 to 2020 inclusive)
  group_by(iso_a3) %>% 
  filter(n() == 21)

# Also define the group of countries that are going to be highlighted
highlights <- c("EUZ", "CHE", "DNK", "SWE", "BRA", "ARG", "GBR", "USA")
n <- length(highlights)  

countries <- df_mac %>% 
  filter(year == 2008) %>% 
  pull(iso_a3)

df_mac_indexed_2008 <- df_mac %>% 
  # Keep countries that have a record for 2008, the index year.
  group_by(iso_a3) %>%
  filter(iso_a3 %in% countries) %>% 
  # Compute the `price_index`
  mutate(
    ref_year = 2008,
    price_index = price[which(year == 2008)],
    price_rel = price - price_index,
    # Create 'group', used to color the lines.
    group = if_else(iso_a3 %in% highlights, iso_a3, "other"),
    group = as.factor(group)
  ) %>% 
  mutate(
    group = fct_relevel(group, "other", after = Inf),
    name_lab = if_else(year == 2020, name, NA_character_)
  ) %>% 
  ungroup()

########
# This theme extends the 'theme_minimal' that comes with ggplot2.
# The "Lato" font is used as the base font. This is similar
# to the original font in Cedric's work, Avenir Next Condensed.
theme_set(theme_minimal(base_family = "Lato"))


theme_update(
  # Remove title for both x and y axes
  axis.title = element_blank(),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 17, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 28, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "none"
)
######
plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  df_mac_indexed_2008 %>% filter(group != "other"), 
  aes(year, price_rel, group = iso_a3)
) + 
  # Geometric annotations that play the role of grid lines
  geom_vline(
    xintercept = seq(2000, 2020, by = 5),
    color = "grey91", 
    size = .6
  ) +
  geom_segment(
    data = tibble(y = seq(-4, 3, by = 1), x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  geom_segment(
    data = tibble(y = 0, x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey60",
    size = .8
  ) +
  geom_vline(
    aes(xintercept = ref_year), 
    color = "grey40",
    linetype = "dotted",
    size = .8
  ) +
  ## Lines for the non-highlighted countries
  geom_line(
    data = df_mac_indexed_2008 %>% filter(group == "other"),
    color = "grey75",
    size = .6,
    alpha = .5
  ) +
  ## Lines for the highlighted countries.
  # It's important to put them after the grey lines
  # so the colored ones are on top
  geom_line(
    aes(color = group),
    size = .9
  )
plt

plt2 <- plt + 
  annotate(
    "text", x = 2008.15, y = -3.35, 
    label = "2008",
    family = "Lato",
    size = 8,
    color = "grey40",
    hjust = 0
  ) +
  geom_text_repel(
    aes(color = group, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2020.8, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    ylim = c(-4, 3)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2023.5), 
    breaks = seq(2000, 2020, by = 5)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(-4, 3, by = 1),
    labels = glue::glue("{format(seq(-4, 3, by = 1), nsmall = 2)}$")
  )
plt2 


plt3 <- plt2 + 
  scale_color_manual(
    values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50")
  ) #+
labs(
  title = "Compared to the financial crisis in 2008, how much more or less do you have to pay for a Big Mac today?",
  subtitle = "The <i>index chart</i> visualizes the price changes (in USD) of a Big Mac based on a 2008 as index year. The <b>Big Mac Index</b> is published by The Economist as an informal way to provide a test of the<br>extent to which market exchange rates result in goods costing the same in different countries. It <i>seeks to make exchange-rate theory a bit more digestible</i> and takes its name from the Big Mac,<br>a hamburger sold at McDonald's restaurants.",
  caption = "Visualization by Cédric Scherer  •  Data by The Economist  •  The index chart shows the 27 countries that provide Big mac prices for all years from 2000 to 2020. In case a country was reported twice per year, the mean value was visualized."
)
plt3
