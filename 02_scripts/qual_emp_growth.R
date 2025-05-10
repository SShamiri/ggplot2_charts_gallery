# Education Attainment Growth Rate ----
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
asced_df <- read_csv(file.path(DATA_in, '01_lookup','lookup_asced.csv')) 

# raw data
df_raw <- read_csv(file.path(DATA_in, '00_abs','20240412_lfs_asced3_sex_cleared.csv')) 

## Data Prep ----
df_clean <- df_raw %>% 
  mutate(date_month = as.Date(date_qtr, '%d/%m/%Y')) %>% 
  # dates before Aug 2015 are mixed of yearly and irregular freq
  filter(date_month >= "2015-08-01") %>% 
  mutate(
    month = month(date_month),
    # Dec month falls into the following year qtr
    year = ifelse(month == 12, year(date_month) + 1, year(date_month)),
    # Align with LFS Quaters (Feb, May, Aug and Nov)
    date_qtr = case_when(
      month %in% c(12,1,2) ~ paste0(year(date_month),"-02-01"),
      month %in% c(3,4,5) ~ paste0(year(date_month),"-05-01"),
      month %in% c(6,7,8) ~ paste0(year(date_month),"-08-01"),
      month %in% c(9,10,11) ~ paste0(year(date_month),"-11-01")
    ),
    date_qtr = as.Date(date_qtr)
  )  %>% 
  group_by(asced3_code, lfs_status, sex, date_qtr) %>%
  summarise(emp = sum( weight_sum , na.rm = TRUE), 
            n = sum(n, na.rm = T), .groups = 'drop') %>% 
  left_join(asced_df, by = "asced3_code") %>% 
  # remove the start and end of a series due to outlier
  filter(date_qtr > '2015-08-01' & date_qtr < '2024-02-01')

#df_clean %>% write_clip()



emp_df <- df_clean %>% 
  filter(lfs_status == 1) 


emp_chng_tlt <- emp_df %>% 
  group_by(attainment_level3, date_qtr) %>% 
  summarise(emp = sum(emp, na.rm = T), .groups = 'drop') %>% 
  group_by(attainment_level3) %>% 
  mutate(
    # Quarterly change
    qtr_growth = calc_change_per(emp, date_qtr, len = 1),
    # Annual change
    annl_growth = calc_change_per(emp, date_qtr, len = 4),
    # Five year change
    fiveyr_growth = calc_change_per(emp, date_qtr, len = 20),
  ) %>% 
  ungroup()


emp_chng_tlt %>% write_clip()

emp_chng_sex <- emp_df %>% 
  arrange(attainment_level3, sex, date_qtr) %>%
  group_by(attainment_level3, sex) %>% 
  mutate(
    # Quarterly change
    qtr_growth = calc_change_per(emp, dt = date_qtr, len = 1),
    # Annual change
    annl_growth = calc_change_per(emp, dt = date_qtr, len = 4),
    # Five year change
    fiveyr_growth = calc_change_per(emp,dt = date_qtr, len = 20),
  ) %>%
  ungroup() 

#emp_chng_sex %>% write_clip()

## Plot ----
theme_set(theme_light(base_size = 11 
                      #           base_family = "Roboto Condensed"
))

## calculate all Occupation average
gr_5yr <- emp_chng_tlt %>% filter(!is.na(fiveyr_growth)) %>% 
  #relocate(anzsco1_name, .after = anzsco1_code) %>% 
  group_by(attainment_level3) %>% 
  mutate(sd = sd(fiveyr_growth),
         avg = mean(fiveyr_growth),
         min = min(fiveyr_growth),
         max = max(fiveyr_growth)) %>% 
  ungroup() %>% 
  mutate( attainment_level3 = fct_reorder(attainment_level3, avg),
          current_qtr = ifelse(date_qtr == max(date_qtr), 'current', 'past'),
          attainment_num = as.numeric(attainment_level3),
          # adjust values for labeling
          dif = abs(fiveyr_growth -avg),
          avg_adj = case_when(
            dif < 0.05 & avg < fiveyr_growth ~ avg - 0.03,
            dif < 0.05 & avg > fiveyr_growth ~ avg + 0.03,
            TRUE ~ avg),
          fiveyr_adj = case_when(
            dif < 0.05 & fiveyr_growth < avg ~ fiveyr_growth - 0.03,
            dif < 0.05 & fiveyr_growth > avg ~ fiveyr_growth + 0.03,
            TRUE ~ fiveyr_growth )
  )

attain_avg <-
  gr_5yr %>%
  summarize(avg = mean(fiveyr_growth, na.rm = TRUE)) %>%
  pull(avg)



## set seed to fix position of jittered points
set.seed(2019)

## final plot
plt_qual_emp <- gr_5yr %>%
  ggplot(aes(x = attainment_level3  , y = fiveyr_growth)) +
  geom_rect(aes(xmin = -Inf, xmax = 7,
                ymin = -Inf, ymax = 0),
            fill = 'grey90',
            alpha = 0.2
  ) +
  # Mean values
  geom_segment(
    aes(x = attainment_level3 , xend = attainment_level3 ,
        y = attain_avg, yend = avg),
    size = 0.8
  ) +
  geom_hline(aes(yintercept = attain_avg), color = "gray20", size = 0.6, linetype = 'dashed') +
  stat_summary(fun = mean, geom = "point", size = 5, color = '#9f1853') +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2,  color = "#2f005f") +
  geom_point(data = gr_5yr %>% filter(current_qtr == 'current'),
             aes(x = attainment_level3 , y = fiveyr_growth), color = '#005d5d', shape = 18, size = 5
  ) +
  coord_flip(clip = "off") +
  annotate(
    "text", x = 2.3, y = max(gr_5yr$fiveyr_growth)-0.08,
    size = 3, color = "gray20",
    label = glue::glue("Overall average:\n{round(attain_avg*100, 2)}% of 5 year growth"),
    lineheight = 1
  ) + 
  annotate(
    geom = "segment",
    x = 2.5, xend = 2.5,
    y = max(gr_5yr$fiveyr_growth)-0.08, yend = attain_avg,
    arrow = arrow(
      length = unit(8, "pt"),
      type = "closed"
    )
  ) +
  # current growth rate
  annotate(
    geom = "text",
    x = 1.4:6.4,  
    y = c(gr_5yr %>% filter(current_qtr == 'current') %>% arrange(avg) %>% pull(fiveyr_adj)),
    label = str_c(round(gr_5yr %>% filter(current_qtr == 'current') %>% arrange(avg) %>% pull(fiveyr_growth)*100,1),"%"),
    color = "#005d5d",
    size = 3
    # hjust = .5
  ) +
  # avg growth rate
  annotate(
    geom = "text",
    x = 1.4:6.4,  
    y = c(gr_5yr %>% filter(current_qtr == 'current') %>% arrange(avg) %>% pull(avg_adj)),
    label = str_c(round(gr_5yr %>% filter(current_qtr == 'current') %>% arrange(avg) %>% pull(avg)*100,1),"%"),
    color = "#9f1853",
    size = 3,
    hjust = -0.2
  ) +
  # legends
  annotate("point", 7,  max(gr_5yr$fiveyr_growth)-0.22, 
           shape = 21, size = 4, color = '#9f1853', fill ='#9f1853') +
  annotate("text", 7, max(gr_5yr$fiveyr_growth)-0.2,     
           label = "Education attainment's average",
           size = 3,
           lineheight = 1, 
           hjust = "left") +
  annotate("point", 6.7, max(gr_5yr$fiveyr_growth)-0.22, 
           shape = 18, size = 4, color = '#005d5d') +
  annotate("text", 6.7, max(gr_5yr$fiveyr_growth)-0.2,     
           #label = str_c("Current Growth Rate\n Nov-2023"),
           label = str_c("Current Growth Rate\n", format(max(gr_5yr$date_qtr), format= "%b-%Y")),
           size = 3,
           lineheight = 1, 
           hjust = "left") +
  scale_y_continuous(
    limits = c(min(gr_5yr$fiveyr_growth), NA), expand = c(0.02, 0.02),
    breaks = c(seq(min(gr_5yr$fiveyr_growth), max(gr_5yr$fiveyr_growth), length.out =  5)),
    labels = scales::percent_format() 
  ) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  scale_color_uchicago() +
  labs(
    x = NULL, y = "Growth rate",
    title = "***5 Year Educational Attainment Growth Rate***",
    #subtitle = "***Over the period 2016-2024***",
    subtitle = str_c("***Over the period ",
                     format(min(emp_df$date_qtr),"%b-%Y"),
                     " - ",format(max(gr_5yr$date_qtr),"%b-%Y"),"***"
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

plt_qual_emp

ggsave(plot = plt_qual_emp, "attain_emp_growth.png", dpi = 320, width = 7 , height = 6.5)


## time series plots ----

selected_dates <- as.Date(c("2020-11-01", "2021-11-01","2022-11-01","2023-11-01"), format = "%Y-%m-%d")
strip <- strip_themed(background_x = elem_list_rect(fill = gr_5yr %>% 
                                                      filter(current_qtr == 'current') %>%
                                                      distinct(attainment_level3, attainment_num, avg,fiveyr_growth ) %>% 
                                                      arrange(attainment_num) %>% 
                                                      mutate(strip_col= ifelse(fiveyr_growth >= avg,'#4b0985', '#9f1853')) %>% 
                                                      pull(strip_col)
))

plt_t <- gr_5yr %>% 
  ggplot(aes(x = date_qtr, y = fiveyr_growth, group = attainment_level3)) +
  geom_line(color = '#4b0985') +
  facet_wrap2(vars(attainment_level3), ncol = 2, 
              scales = 'free_y', strip = strip
  ) +
  scale_x_date(
    breaks = selected_dates,
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    labels = scales::percent_format() 
  )  +
  geom_hline(aes(yintercept = avg), color = "gray20", size = 0.6, linetype = 'dashed') +
  geom_point(data = gr_5yr %>% filter(date_qtr %in%  c(selected_dates[1],selected_dates[length(selected_dates)] )),
             color = '#9f1853'
  ) +
  geom_text_repel(data = gr_5yr %>% filter(date_qtr %in%  c(selected_dates[1],selected_dates[length(selected_dates)])),
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
    title = "***5 Year Educational Attainment Growth Rate***",
    subtitle = str_c("***Over the period ",
                     format(min(emp_df$date_qtr),"%b-%Y"),
                     " - ",format(max(gr_5yr$date_qtr),"%b-%Y"),"***"
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
    axis.text = element_text( lineheight = 1),
    axis.text.x = element_text(
      margin = margin(10, 0, 20, 0),
      size = 8), ## trbl
    panel.grid = element_blank(),
    #panel.border = element_blank(),
    strip.background = element_rect(fill="#4b0985")
  )

 plt_t

 ggsave(plot = plt_t , "attain_emp_t.png", dpi = 320, width = 7 , height = 6.5) 
 