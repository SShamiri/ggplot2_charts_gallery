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
# lookup
anzsco_df <- read_csv(file.path(DATA_in, '01_lookup','lookup_anzsco.csv')) %>% 
  filter(level == 1) %>% 
  rename(anzsco1_code = anzsco_code, anzsco1_name = anzsco_name) %>% 
  select(anzsco1_code, anzsco1_name)

# raw data
df_raw <- read_csv(file.path(DATA_in, '00_abs','20240419_wage_anzsco1_sex.csv')) 

df_clean <- df_raw %>% 
  left_join(anzsco_df) %>% 
  select(date_year, anzsco1_code, anzsco1_name, sex, med_wage)
  
wage_sex <- df_clean %>% 
  group_by( anzsco1_name, sex, date_year) %>% 
  summarise(med_wage = mean(med_wage), .groups = 'drop')

# Gender gap

sex_gap <- wage_sex %>% 
  filter(date_year == max(date_year)) %>% 
  select(-date_year) %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>% 
  group_by(anzsco1_name) %>% 
  mutate(gap = (med_wage[sex== 'Male'] - med_wage[sex=='Female'])/med_wage[sex=='Female']) %>% 
  ungroup() %>% 
  mutate(anzsco1_name = fct_reorder(anzsco1_name, gap),
         anzsco1_num = as.numeric(anzsco1_name),
         gap = ifelse(sex == "Male", 0, -1*gap)
         )  

sex_gap  

occ_gap <- sex_gap %>%
  ggplot(aes(x = anzsco1_name, y =gap, color = sex)) +
  geom_segment(
    aes(x = anzsco1_name , xend = anzsco1_name ,
        y =0 , yend = gap),
    size = 0.8,
    color = '#4b0985'
  ) +
  geom_hline(aes(yintercept = 0), color = "gray20", size = 0.6, linetype = 'dashed') +
  geom_point(
    data = sex_gap %>% filter(sex == 'Female'),
    aes(x = anzsco1_name  , y = gap), 
    shape = 21, 
    fill = '#4b0985',
    size = 5
  ) +
  geom_text(
    data = sex_gap %>% filter(sex == 'Female'),
    aes(label= scales::percent(gap, accuracy = 1)),
    nudge_x = 0.25,
    size = 3,
    color =  '#4b0985'
  ) +
  coord_flip(clip = "off") +
  #scale_color_manual(values= sex.colors) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  scale_y_continuous(
    limits = c(min(sex_gap$gap)-0.1, 0.3),
    breaks = seq(min(sex_gap$gap), -0.1, length.out = 5),
    labels = scales::percent_format(accuracy = 1) 
  ) +
  annotate("text", x = 1:8, y = 0.05, label = scales::dollar(sex_gap %>% filter(sex == "Male") %>% 
                                           arrange(anzsco1_num) %>% pull(med_wage)),
           size = 3.5
           ) +
  # Title and subtitle
  labs(
    x = NULL, y = NULL,
    title = "***What women earn relative to men***",
    subtitle = "***Gap varies by choice of occupation (as Aug-2023)***",
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
    legend.position = 'none'
    #legend.position = 'top',
    # legend.position = c(0, 1),
    # legend.direction="horizontal",
    # legend.title = element_blank()
  )
occ_gap

ggsave(plot = , "occ_wage_sex.png", dpi = 320, width = 7 , height = 6.5)

