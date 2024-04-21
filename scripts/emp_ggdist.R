
## Load data ----
# look up
asced_lkup <- read_csv(file.path('data', 'lookup_asced.csv'))
# data
df_raw <- read_csv(file.path('data','asced3_sex.csv'))

## Prep data for graph
df_emp <- df_raw %>% 
  left_join(asced_lkup, by = c('asced3_code' = 'asced_code')) %>% 
  filter(lfs_status == 1) %>% 
  mutate(date = as.Date(date_qtr, format = '%d/%m/%Y')) %>% 
  group_by(attain2_name,  date) %>% 
  summarise(emp = sum(weight_sum, na.rm = T)/100000,
            n = sum(n, na.rm = T),
            .groups = 'drop'
  ) %>% 
  filter(attain2_name %in% c('Secondary', 'Diploma', 'Postgraduate Degree')) %>% 
  group_by(attain2_name) %>% 
  mutate(
    n = n(),
    median = median(emp),
    max = max(emp)
  ) %>% 
  ungroup() %>% 
  mutate(attain2_num = as.numeric(fct_rev(attain2_name))) %>% 
  ungroup()

df_emp
## Plot graph ----

plt <- df_emp %>% 
  ggplot(aes(emp, attain2_num, color = attain2_name)) +
  stat_summary(
    geom = "linerange",
    fun.min = function(x) -Inf,
    fun.max = function(x) median(x, na.rm = TRUE),
    linetype = "dotted",
    orientation = "y",
    size = .7
  ) +
  geom_point(
    aes(y = attain2_num - .15), 
    shape = "|",
    size = 5,
    alpha = .33
  ) +
  ggdist::stat_halfeye(
    aes(
      y = attain2_num,
      color = attain2_name,
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
    size = 2.4,
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
    limits = c(0, 55),
    breaks = seq(0, 55, by = 10)
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:3,
    labels = c("Secondary", "Postgraduate", "Diploma")
  ) +
  scale_color_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  scale_fill_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  labs(
    x = "Employment",
    y = NULL,
    subtitle = "Something to put here.",
    caption = "Source: ABS Labour Force Survey "
  ) +
  theme(
    panel.grid.major.x = element_line(size = .35),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.ticks.length = unit(0, "lines"),
    plot.title.position = 'plot',
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.margin = margin(10, 25, 10, 25)
  )
plt
ggsave("emp_dist_plot.png", width = 9, height = 5.2)
unlink("emp_dist_plot.pdf")
#png("emp_dist_plot.png")
#dev.off()
#