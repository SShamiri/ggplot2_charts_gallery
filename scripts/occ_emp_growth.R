# Analyising ----


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

emp_chng_sex <- emp_df %>% 
  arrange(anzsco1_code, sex, date_qtr) %>%
  group_by(anzsco1_code, sex) %>% 
  mutate(
    # Quarterly change
    qtr_growth = calc_change_per(emp, dt = date_qtr, len = 1),
    # Annual change
    annl_growth = calc_growth(emp, dt = date_qtr, len = 4),
    # Five year change
    fiveyr_growth = calc_growth(emp,dt = date_qtr, len = 20),
  ) %>%
  ungroup() 

#emp_chng_sex %>% write_clip()


emp_chng_tlt %>% 
  filter(date_qtr == max(date_qtr)) %>% 
  left_join(anzsco_df) %>% 
  relocate(anzsco1_name, .after = anzsco1_code)


## Plot ----
theme_set(theme_light(base_size = 11 
                      #           base_family = "Roboto Condensed"
))

## calculate all Occupation average
gr_4yr <- emp_chng_tlt %>% filter(!is.na(fiveyr_growth)) %>% 
  left_join(anzsco_df) %>% 
  relocate(anzsco1_name, .after = anzsco1_code) %>% 
  group_by(anzsco1_code) %>% 
  mutate(sd = sd(fiveyr_growth),
         avg = mean(fiveyr_growth)) %>% 
  ungroup() %>% 
  mutate( anzsco1_name = fct_reorder(anzsco1_name, sd),
          current_qtr = ifelse(date_qtr == max(date_qtr), 'current', 'past'))

gr_4yr %>% filter(current_qtr == 'current')

occ_avg <-
  gr_4yr %>%
  summarize(avg = mean(fiveyr_growth, na.rm = TRUE)) %>%
  pull(avg)

## coordinates for arrows
# arrows <-
#   tibble(
#     x1 = c(8),
#     x2 = c(7.4),
#     y1 = c(0.55 ),
#     y2 = c(occ_avg + 0.02)
#   )


gr_4yr %>% 
  filter(current_qtr == 'current') 




## set seed to fix position of jittered points
set.seed(2019)

## final plot
gr_4yr %>%
  ggplot(aes(x = anzsco1_name , y = fiveyr_growth)) +
  geom_segment(
    aes(x = anzsco1_name, xend = anzsco1_name,
        y = occ_avg, yend = fiveyr_growth),
    #show.legend=FALSE,
    size = 0.8
  ) +
  geom_hline(aes(yintercept = occ_avg), color = "gray70", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5, color = '#9f1853') +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2,  color = "#2f005f") +
  geom_point(data = gr_4yr %>% filter(current_qtr == 'current'),
             aes(x = anzsco1_name , y = fiveyr_growth), color = '#005d5d', shape = 18, size = 5
  )+
  coord_flip() +
  annotate(
    "text", x = 7.3, y = 0.45,
    size = 2.7, color = "gray20",
    label = glue::glue("Occupations average:\n{round(occ_avg*100, 2)}% of 5 year growth"),
    lineheight = 1
  ) + 
  annotate(
    geom = "segment",
    x = 7.6, xend = 7.6,
    y = 0.45, yend = 0.072,
    arrow = arrow(
      length = unit(10, "pt"),
      type = "closed"
    )
  ) +
  # current growth rate
  annotate(
    geom = "text",
    x = 1.4:8.4,  
    y = c(gr_4yr %>% filter(current_qtr == 'current') %>% arrange(sd) %>% pull(fiveyr_growth)),
    label = str_c(round(gr_4yr %>% filter(current_qtr == 'current') %>% arrange(sd) %>% pull(fiveyr_growth)*100,2),"%"),
    color = "#005d5d",
    size = 2.5,
    hjust = -.8
 
  ) +
  # avg growth rate
  annotate(
    geom = "text",
    x = 1.4:8.4,  
    y = c(gr_4yr %>% filter(current_qtr == 'current') %>% arrange(sd) %>% pull(avg)),
    label = str_c(round(gr_4yr %>% filter(current_qtr == 'current') %>% arrange(sd) %>% pull(avg)*100,2),"%"),
    color = "#9f1853",
    size = 2.5,
    hjust = 1.2
    # fontface = c("plain", "bold")
  ) +
  scale_y_continuous(
    limits = c(-0.3, NA), expand = c(0.02, 0.02),
    breaks = c(seq(-0.3, 0.65, by = 0.15)),
    labels = scales::percent_format() 
  ) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  scale_color_uchicago() +
  labs(
    x = NULL, y = "Growth rate",
    title = "***5 Year Occupational Growth Rate***",
    subtitle = "***Where VET is the primary pathway (Skill Level 2-4)***",
    caption = 'Data: ABS Labour Force Survey\n Chart: JSA Economic Modelling'
  ) +
  annotation_custom(
    
  ) +
  theme_light(base_size = 11) +
  theme(
   # plot.title = ggtext::element_markdown(),
    plot.title =ggtext::element_textbox_simple(
      size = 18, margin = margin(t = 12)),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 10, margin = margin(b = 12)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, color = "gray50"),
    axis.title = element_text(size = 11),
    axis.text = element_text(
      # hjust = 1,
      # vjust = 0,
      lineheight = 1, ## no effect here
      #margin = margin(10, 0, 20, 10),
      #debug = TRUE
    ),
    axis.text.x = element_text(
      margin = margin(10, 0, 20, 0),
      size = 10), ## trbl
    legend.position = "top",
    panel.grid = element_blank()
  )






####################### OTHER ----
# ## load fonts
# font_add_google("Poppins", "Poppins")
# font_add_google("Roboto Mono", "Roboto Mono")
# showtext_auto()
# 
# ## calculate all Occupation average
# gr_4yr <- emp_chng_tlt %>% filter(!is.na(fiveyr_growth)) %>% 
#   mutate(
#     anz_f = fct_reorder(factor(anzsco1_code), - fiveyr_growth) # sort the data
#   )
# 
# occ_avg <-
#   gr_4yr %>%
#   summarize(avg = mean(fiveyr_growth, na.rm = TRUE)) %>%
#   pull(avg)
# 
# ## coordinates for arrows
# arrows <-
#   tibble(
#     y1 = c(0.55),
#     y2 = c(0.07),
#     x1 = c(occ_avg ),
#     x2 = c(occ_avg + 0.1)
#   )
# 
# ## set seed to fix position of jittered points
# set.seed(2019)
# 
# ## final plot
# gr_4yr %>%
#   ggplot(aes(x = anz_f, y = fiveyr_growth, color = anz_f)) +
#   geom_segment(
#     aes(x = anz_f, xend = anz_f,
#         y = occ_avg, yend = fiveyr_growth),
#     size = 0.8
#   ) +
#   geom_hline(aes(yintercept = occ_avg), color = "gray70", size = 0.6) +
#   stat_summary(fun = mean, geom = "point", size = 5) +
#   geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
#   coord_flip() +
#   annotate(
#     "text", x = 8.3, y = 0.55,
#     size = 2.7, color = "gray20",
#     label = glue::glue("Occupations average:\n{round(occ_avg, 2)} of 5 year growth")
#   ) + 
#   # annotate(
#   #   "text", x = 1.9, y = 64, family = "Poppins",
#   #   size = 2.7, color = "gray20",
#   #   label = "The Central African Republic has by far\nthe most students per teacher"
#   # ) +
#   geom_curve(
#     #data = arrows,
#     aes(x = 8 , xend = 7.4,
#         y = 0.55, yend = 0.072),
#     arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
#     color = "gray20", curvature = -0.1
#   ) +
#   scale_y_continuous(
#     limits = c(-0.3, NA), expand = c(0.02, 0.02),
#     breaks = c( seq(-0.3, 0.65, by = 0.15))
#   ) +
# scale_color_uchicago() +
#   labs(
#     x = NULL, y = "Growth rate",
#     title = "5 Year Occupational Growth Rate",
#     caption = 'Data: ABS Labour force survey'
#   ) +
#   theme_light(base_size = 18) +
#   theme(
#     legend.position = "none",
#     axis.title = element_text(size = 12),
#     axis.text.x = element_text( size = 10),
#     plot.caption = element_text(size = 9, color = "gray50"),
#     panel.grid = element_blank()
#   )




########################### simple version #############################################
#theme_set(theme_light(base_size = 18))
# g <-
#   ggplot(df_sorted, aes(x = anz_f, y = fiveyr_growth, color = anz_f)) +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.3, 0.8), expand = c(0.02, 0.02)) +
#   #scale_color_uchicago() +
#   labs(x = NULL, y = "growth") +
#   theme(
#     legend.position = "none",
#     axis.title = element_text(size = 16),
#     axis.text.x = element_text( size = 12),
#     panel.grid = element_blank()
#   )
# 
# g
# g + geom_point(size = 3, alpha = 0.15)
# 
# 
# occ_avg <-
#   emp_chng_tlt %>%
#   summarize(avg = mean(fiveyr_growth, na.rm = TRUE)) %>%
#   pull(avg)
# 
# g +
#   geom_segment(
#     aes(x = anz_f, xend = anz_f,
#         y =occ_avg, yend = fiveyr_growth),
#     size = 0.8
#   ) +
#   geom_hline(aes(yintercept = occ_avg), color = "gray70", size = 0.6) +
#   geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
#   stat_summary(fun = mean, geom = "point", size = 5)
