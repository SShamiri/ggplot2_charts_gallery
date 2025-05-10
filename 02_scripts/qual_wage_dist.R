# Wage Growth Rate by Education Attainment ----
# Wages Gender Gap


## Load Packages ----
packages <- c(
  "tidyverse",
  "lubridate",
  "clipr",
  "forcats",
  "ggplot2",
  "ggrepel",
  "ggtext",
  "ggh4x",
  "imputeTS"
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
asced_df <- read_csv(file.path(DATA_in, '01_lookup','lookup_asced.csv')) 

# raw data
df_raw <- read_csv(file.path(DATA_in, '00_abs','20240419_wage_asced_sex.csv')) 

## Data Prep ----
df_clean <- df_raw %>% 
  mutate(date_year = as.Date(date_year, '%d/%m/%Y')) %>% 
  left_join(asced_df, by = "asced3_code") 

df_clean #%>% write_clip()

wage_tlt <- df_clean %>% 
  group_by(attainment_level3, date_year) %>% 
  summarise(med_wage = mean(med_wage), .groups = 'drop') %>% 
  group_by(attainment_level3) %>% 
  mutate(
    med_wage = na_ma(med_wage, k=1, weighting = "simple"),
    avg = mean(med_wage, na.rm = T),
    min =  min(med_wage, na.rm = T),
    max =  max(med_wage, na.rm = T)
    ) %>% 
  ungroup() %>% 
  mutate( 
          attainment_level3 = fct_reorder(attainment_level3, avg),
          current_yr = ifelse(date_year == max(date_year), 'current', 'past'),
          attain_num = as.numeric(attainment_level3)
          )


wage_sex <- df_clean %>% 
  group_by(attainment_level3, sex, date_year) %>% 
  summarise(med_wage = mean(med_wage), .groups = 'drop')


wage_sex %>% write_clip()


## Wage distribution
plt_wage <- wage_tlt %>% 
  ggplot(aes(med_wage, attain_num, color = attainment_level3)) +
  stat_summary(
    geom = "linerange",
    fun.min = function(x) -Inf,
    fun.max = function(x) median(x, na.rm = TRUE),
    linetype = "dotted",
    orientation = "y",
    size = .7
  ) +
  geom_point(
    aes(y = attain_num - .15), 
    shape = "|",
    size = 3,
    alpha = .33
  ) +
  ggdist::stat_halfeye(
    aes(
      y = attain_num,
      color = attainment_level3,
      fill = after_scale(colorspace::lighten(color, .5))
    ),
    shape = 18,
    point_size = 3,
    interval_size = 1.8,
    adjust = .5,
    .width = c(0, 1)
  ) +
  geom_text(
    aes(x = med_wage, label = format(round(med_wage, 2), nsmall = 2)),
    stat = "unique",
    color = "white",
    #family = "Open Sans",
    fontface = "bold",
    size = 2.4,
    nudge_y = .1
  ) +
  geom_text(
    aes(x = max, 
        #label = glue::glue("mean = {round(avg,0)}")
        label = glue::glue("mean = {scales::dollar(avg, accuracy = 1)}")
        ),
    stat = "unique",
    fontface = "bold",
    size = 3.5,
    hjust = -0.1,
    nudge_x = .01,
    nudge_y = .02
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  scale_x_continuous(
    limits = c(520, 2500),
    breaks = round(seq(550, 2200, length.out  = 5),0),
    labels = scales::label_currency()
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:6,
    labels = str_wrap(levels(wage_tlt$attainment_level3), 12)
  ) +
  scale_color_manual(values = c("#4b0985", "#2f005f", "#d5a3f9","#d2de5a","#0f2532", "#b91c1c"), guide = "none") +
  scale_fill_manual(values = c("#4b0985", "#2f005f", "#d5a3f9","#d2de5a","#0f2532", "#b91c1c"), guide = "none") +
  labs(
    x = "Median weekly wages", y = NULL,
    title = "***Educational Attainment Wage Distribution***",
    #subtitle = "***Over the period 2016-2024***",
    subtitle = str_c("***Weekly median wages over the period ",
                     format(min(wage_tlt$date_year),"%Y"),
                     " - ",format(max(wage_tlt$date_year),"%Y"),"***"
    ),
    caption = 'Data: ABS Labour Force Survey\n Chart: JSA Economic Modelling'
  ) +
  theme_light(base_size = 11) +
  theme(
    plot.title =ggtext::element_textbox_simple(
      size = 18, margin = margin(t = 12)),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 10, margin = margin(b = 12)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, color = "gray50"),
    axis.title = element_text(size = 11),
    axis.text = element_text(
      lineheight = 1
    ),
    axis.text.x = element_text(
      margin = margin(10, 0, 20, 0),
      size = 10), ## trbl
    axis.line.x = element_line(color = "grey40"),
    legend.position = "top",
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

plt_wage

ggsave(plot = plt_wage , "attain_wage.png", dpi = 320, width = 7 , height = 6.5) 

# Gender gap

sex_gap <- wage_sex %>% 
  filter(date_year == max(date_year)) %>% 
  select(-date_year) %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>% 
  group_by(attainment_level3) %>% 
  mutate(gap = (med_wage[sex== 'Male'] - med_wage[sex=='Female'])/med_wage[sex=='Female']) %>% 
  ungroup() %>% 
  mutate( attainment_level3 = fct_reorder(attainment_level3, med_wage))

sex_wide <- wage_sex %>%
  filter(date_year == max(date_year)) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  pivot_wider(names_from = sex, values_from = med_wage) %>%
  select(-date_year)

# Plt

sex.colors <- c(Male = "#0f2532", Female = "#d5a3f9")

plt_gap <- sex_gap %>%
  ggplot(aes(x = attainment_level3, y = med_wage, color = sex)) +
  geom_segment(
    data = sex_wide,
    aes(x = attainment_level3 , xend = attainment_level3 ,
        y =Female , yend = Male),
    size = 0.8,
    color = 'gray50'
  ) +
  geom_text(
    aes(label= scales::dollar(med_wage, accuracy = 1)),
    nudge_x = 0.2,
    size = 3
    #color =  '#e76af7'
  ) + 
  geom_point(
    aes(x = attainment_level3 , y = med_wage), shape = 18, size = 5
  ) +
  coord_flip(clip = "off") +
  scale_color_manual(values= sex.colors) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  scale_y_continuous(
     limits = c(600, 2100),
     breaks = round(seq(min(sex_gap$med_wage), max(sex_gap$med_wage), length.out  = 5),0),
     labels = scales::label_currency()
  ) +
  # Title and subtitle
  labs(
    x = NULL, y = NULL,
    title = "***Eraning gap varies by education***",
    subtitle = "***Impact on women's earning relative to men (as Aug-2023)***",
    caption = 'Data: ABS Labour Force Survey\n Chart: JSA Economic Modelling'
  ) +
  # themes 
  theme(
    plot.title =ggtext::element_textbox_simple(
      size = 18, margin = margin(t = 12)),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 10, margin = margin(b = 12)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, color = "gray50"),
    panel.background = element_rect(fill="white"),
    panel.grid.minor = element_blank(),
    #panel.grid.major.x = element_line(size = .12, colour="grey50"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.ticks.length = unit(0, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(10, 25, 10, 25),
    #legend.position = 'top',
    legend.position = c(0, 1),
    legend.direction="horizontal",
    legend.title = element_blank()
  )

plt_gap

ggsave(plot = plt_gap , "attain_wage_sex.png", dpi = 320, width = 7 , height = 6.5)   
  



  