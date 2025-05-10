# Occupational Growth Rate ----
# for VET skill pathways skill level 2-4
###### Tips
#To save a figure as you have seen in the preview window
#-1 Check the figure in Rstudio Plots window, if not satisfied, click Zoom,
#   drag the pop-out window to your preferred size
#-2 Right click the figure and choose Inspect element,  
# <img id="plot" width="100%" height="100%" src="plot_zoom_png?width=885&amp;height=555">
#-3 COPY"width=885&amp;height=555" and enter the deminssions into ggsave

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
  "ggtext",
  "ggh4x"
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


emp_chng_sex <- emp_df %>% 
  arrange(anzsco1_code, sex, date_qtr) %>%
  group_by(anzsco1_code, sex) %>% 
  mutate(
    # Quarterly change
    qtr_growth = calc_change_per(emp, dt = date_qtr, len = 1),
    # Annual change
    annl_growth = calc_change_per(emp, dt = date_qtr, len = 4),
    # Five year change
    fiveyr_growth = calc_change_per(emp,dt = date_qtr, len = 20),
  ) %>%
  ungroup() 

emp_chng_sex %>% write_clip()


## Plot ----
theme_set(theme_light(base_size = 11 
                      #           base_family = "Roboto Condensed"
))

## calculate all Occupation average
emp_chng_sex %>% filter(!is.na(fiveyr_growth)) %>% 
  left_join(anzsco_df) %>% 
  relocate(anzsco1_name, .after = anzsco1_code) %>% 
  group_by(anzsco1_code) %>% 
  mutate(sd = sd(fiveyr_growth),
         avg = mean(fiveyr_growth),
         min = min(fiveyr_growth),
         max = max(fiveyr_growth)) %>% 
  ungroup() 



gr_sex <- emp_chng_sex %>% filter(!is.na(fiveyr_growth)) %>% 
  left_join(anzsco_df) %>% 
  relocate(anzsco1_name, .after = anzsco1_code) %>% 
  group_by(anzsco1_code, sex) %>% 
  mutate(sd = sd(fiveyr_growth),
         avg = mean(fiveyr_growth),
         min = min(fiveyr_growth),
         max = max(fiveyr_growth)) %>% 
  ungroup() %>% 
  mutate( anzsco1_name = fct_reorder(anzsco1_name, sd),
          current_qtr = ifelse(date_qtr == max(date_qtr), 'current', 'past'),
          anzsco1_num = as.numeric(anzsco1_name),
          colr = ifelse(sex ==1, '#0f2532', '#d5a3f9'),
          sex = ifelse(sex ==1, 'Male', 'Female')
          # adjust values for labeling
          # dif = abs(fiveyr_growth -avg),
          # avg_adj = case_when(
          #   dif < 0.05 & avg < fiveyr_growth ~ avg - 0.03,
          #   dif < 0.05 & avg > fiveyr_growth ~ avg + 0.03,
          #   TRUE ~ avg),
          # fiveyr_adj = case_when(
          #   dif < 0.05 & fiveyr_growth < avg ~ fiveyr_growth - 0.03,
          #   dif < 0.05 & fiveyr_growth > avg ~ fiveyr_growth + 0.03,
          #   TRUE ~ fiveyr_growth )
  )

gr_sex %>% write_clip()

## Plot ----
theme_set(theme_light(base_size = 11 
                      #           base_family = "Roboto Condensed"
))

sex.colors <- c(Male = "#0f2532", Female = "#d5a3f9")

## set seed to fix position of jittered points
set.seed(2019)

## final plot

plt_occ_sex <- gr_sex %>%
  ggplot(aes(x = anzsco1_name , y = fiveyr_growth, color = sex)) +
  # point plot for range of growth rate
  #geom_jitter(size = 2, alpha = 0.25, width = 0.2,  color = "#d2de5a") +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  # Current values Male
  geom_point(data = gr_sex %>% filter(current_qtr == 'current' & sex == "Male"),
             aes(x = anzsco1_name , y = fiveyr_growth),color = "#0f2532", shape = 18, size = 4
  ) +
  # Current values female
  geom_point(data = gr_sex %>% filter(current_qtr == 'current' & sex == "Female"),
             aes(x = anzsco1_name , y = fiveyr_growth),color = "#e76af7", shape = 18, size = 4
  ) +
  geom_hline(aes(yintercept = occ_avg), color = "gray20", size = 0.6, linetype = 'dashed') +
  guides(color="none") +
  coord_flip(clip = "off") +
  scale_color_manual(values= sex.colors) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  scale_y_continuous(
    # limits = c(-0.25, NA), expand = c(0.02, 0.02),
    # breaks = c(seq(-0.25, 0.65, by = 0.15)),
    labels = scales::percent_format() 
  ) +
  # current growth rate
  # annotate(
  #   geom = "text",
  #   x = 1.4:8.4,  
  #   y = c(gr_sex %>% filter(current_qtr == 'current' & sex =='Female') %>% arrange(sd) %>% pull(fiveyr_adj)),
  #   label = str_c(round(gr_sex %>% filter(current_qtr == 'current '& sex =='Female') %>% arrange(sd) %>% pull(fiveyr_growth)*100,2),"%"),
  #   color = "#005d5d",
  #   size = 2.5,
  #   # hjust = -.8
  #   
  # ) +
  geom_text(data = gr_sex %>% filter(current_qtr == 'current' & sex =='Female') %>% arrange(sd),
            aes(label= str_c(round(fiveyr_growth*100,2),"%")),
            nudge_x = -0.35,
            size = 3,
            color =  '#e76af7'
  ) +
  geom_text(data = gr_sex %>% filter(current_qtr == 'current' & sex =='Male') %>% arrange(sd),
            aes(label= str_c(round(fiveyr_growth*100,2),"%")),
            nudge_x = 0.35,
            size = 3,
            color =  '#0f2532'
  ) +
  # legends
  annotate("point", 9.1, 1, 
           shape = 18, size = 4, color = '#e76af7', fill ='#e76af7') +
  annotate("text", 9.1, 1.05,     
           label = "Female Current Growth Rate\n Feb-2024",
           size = 3,
           lineheight = 1, 
           hjust = "left") +
  annotate("point", 8.6, 1, 
           shape = 18, size = 4, color = '#0f2532') +
  annotate("text", 8.6, 1.05,     
           label = str_c("Male Current Growth Rate\n Feb-2024"),
           size = 3,
           lineheight = 1, 
           hjust = "left") +
  # Title and subtitle
  labs(
    x = NULL, y = "Growth rate",
    title = "***5 Year Occupational Growth Rate by Gender***",
    subtitle = "***Where VET is the primary pathway (Skill Level 2-4)***",
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
    plot.margin = margin(10, 25, 10, 25)
  )
plt_occ_sex

#ggsave(plot = plt_occ_sex,"occ_emp_growth_sex.png", dpi = 320, width = 7 , height = 6.5)

## Time series ----
selected_dates <- as.Date(c("2011-08-01", "2014-02-01", "2019-02-01","2024-02-01"), format = "%Y-%m-%d")

# Only colour strips in x-direction
# manually put in colours
strip <- strip_themed(background_x = elem_list_rect(fill = rep('#4b0985', 8)))

plt_sex_t <- gr_sex %>% 
  ggplot(aes(x = date_qtr, y = fiveyr_growth, color = sex)) +
  geom_line() +
  facet_wrap2(vars(anzsco1_name), ncol = 2, 
              scales = 'free_y', strip = strip
  ) +
  scale_x_date(
    
    breaks = selected_dates,
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    labels = scales::percent_format() 
  )  +
  geom_point(data = gr_sex %>% filter(date_qtr %in%  c(selected_dates)),
             color = '#9f1853'
  ) +
  geom_text_repel(data = gr_sex %>% filter(date_qtr %in%  c(selected_dates)),
                  aes(label= str_c(round(fiveyr_growth *100,0),"%")),
                  #color = '#9f1853',
                  size = 3,
                  box.padding = 0.5,
                  #nudge_y = 0.1,
                  direction = "y"
                  
                  
  ) +
  coord_cartesian(clip = "off") +
  scale_color_uchicago() +
  labs(
    x = NULL, y = NULL,
    title = "***5 Year Occupational Growth Rate by Gender***",
    subtitle = "***Where VET is the primary pathway (Skill Level 2-4)***",
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
    axis.text = element_text( lineheight = 1),
    axis.text.x = element_text(
      margin = margin(10, 0, 20, 0),
      size = 8), ## trbl
    panel.grid = element_blank(),
    #panel.border = element_blank(),
    legend.position = 'top',
    strip.background = element_rect(fill="#4b0985")
  )

plt_sex_t

ggsave(plot = plt_sex_t,"occ_emp_t_sex.png", dpi = 320, width = 7 , height = 6.5)
