

gr_4yr %>%
  ggplot(aes(x = anzsco1_name , y = fiveyr_growth)) +
  
  # Mean values
  geom_segment(
    aes(x = anzsco1_name, xend = anzsco1_name,
        y = occ_avg, yend = avg),
    show.legend=FALSE,
    color = '#9f1853',
    size = 0.8
  ) +
  stat_summary(fun = mean, geom = "point", size = 4, color = '#9f1853') +
  geom_text(data = gr_4yr %>% filter(current_qtr == 'current') %>% arrange(sd),
           aes(label= str_c(round(avg*100,2),"%")),
           nudge_x = 0.35,
           size = 2.5,
           color = '#9f1853'
           ) +
  geom_hline(aes(yintercept = occ_avg), color = "gray70", size = 0.6) +
  # Current values
  geom_point(data = gr_4yr %>% filter(current_qtr == 'current'),
             aes(x = anzsco1_name , y = fiveyr_growth), color = '#005d5d', shape = 18, size = 4
  ) +
  geom_text(data = gr_4yr %>% filter(current_qtr == 'current') %>% arrange(sd),
            aes(label= str_c(round(fiveyr_growth*100,2),"%")),
            nudge_x = -0.35,
            size = 2.5,
            color =  '#005d5d'
  ) +
# point plot for range of growth rate
  geom_jitter(size = 2, alpha = 0.25, width = 0.2,  color = "#2f005f") +
  # dash lines for guidance
  geom_segment(
    aes(x = anzsco1_name, xend = anzsco1_name,
        y = min(fiveyr_growth), yend = min),
    color = "black",
    linetype="dashed",
    size = 0.7,
    alpha = .33
  ) +
  coord_flip(clip = "off") +
  annotate(
    "text", x = 7.3, y = 0.45,
    size = 3, color = "gray20",
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
  # fix the X-axis and y-axis
  scale_x_discrete(labels = scales::label_wrap(15)) +
  scale_y_continuous(
    # limits = c(-0.25, NA), expand = c(0.02, 0.02),
    # breaks = c(seq(-0.25, 0.65, by = 0.15)),
    labels = scales::percent_format() 
  ) +
  # legends
  annotate("point", 9.1, 0.5, 
           shape = 21, size = 4, color = '#9f1853', fill ='#9f1853') +
  annotate("text", 9.1, 0.51,     
           label = "Occupation's Average",
           size = 3,
           lineheight = 1, 
           hjust = "left") +
  annotate("point", 8.7, 0.5, 
           shape = 18, size = 4, color = '#005d5d') +
  annotate("text", 8.7, 0.51,     
           label = str_c("Current Growth Rate\n Feb-2024"),
           size = 3,
           lineheight = 1, 
           hjust = "left") +
  # Title and subtitle
  labs(
    x = NULL, y = "Growth rate",
    title = "***5 Year Occupational Growth Rate***",
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
