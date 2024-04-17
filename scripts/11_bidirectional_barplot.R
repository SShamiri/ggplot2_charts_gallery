# Plotting bidirectional horizontal bar chart ----


## Load Packages ----
packages <- c(
  "tidyverse",
  "forcats",
  "ggplot2",
  "systemfonts"
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

## Data set ----
# this is US data for Unemployment rates and earnings by educational attainment, 2022
# link to the data https://www.bls.gov/emp/tables/unemployment-earnings-education.htm
# Objective is to create one for Australia

edu_df <- tibble(
  edu = c("Doctoral degree", "Professional degree", "Master's degree", 
          "Bachelor's degree", "Associate's degree", "Some college, no degree", 
          "High school diploma", "Less than a high school diploma"),
  wage = c(2083, 2080, 1661, 1432, 1005, 935, 853, 682),
  ue = c(1, 1.4, 1.9, 2.2, 2.7, 3.5, 4, 5.5)
  ) %>% 
  mutate(edu = fct_reorder(edu,wage) )

# 
plot_df <- edu_df %>% 
  pivot_longer(!edu, names_to = 'var', values_to = 'val') %>% 
  mutate(
    val_lab = ifelse(var == 'wage', str_c('$', val), str_c(val, '%')),
    val = ifelse(var == "wage", val *-1/10^3 - mean(edu_df$ue), val)) # adjust to scale values


## wage bar-chart ----
val_adj = (min(abs(plot_df$val)) + mean(edu_df$ue))*-1


plot_df %>% 
  mutate(adj_lab = ifelse(var == 'wage', val_adj +val,  mean(edu_df$ue) +val) # to help with bar values labels
         ) %>%
  # wage bar
  ggplot(aes(x = edu, color = var))+
  geom_linerange(data = plot_df %>% filter(var == 'wage'), 
                 aes(ymin = val_adj, ymax = val_adj +val), size = 8.5, alpha = 0.7) +
  geom_segment(aes(x = 0.6 , y = -18,
                   xend = 8.3, yend =-18 ),
               color = "black",
               linetype="dashed",
               size = 0.7
  ) +
  # ue bar
  geom_linerange(data = plot_df %>% filter(var == 'ue'),
                 aes(ymin = abs(val_adj), ymax = abs(val_adj) +val), size = 8.5, alpha = 0.7) +
  geom_segment(aes(x = 0.6 , y = 22,
                   xend = 8.3, yend =22 ),
               color = "black",
               linetype="dashed",
               size = 0.7
  ) +
  # add bar values labels
  geom_label(aes(x = edu, y = adj_lab , label = val_lab, 
                 #colour = lab_col
                 ),
             hjust = -0.7, 
             vjust = 0.5, 
             colour = "White", 
             fill = NA, 
             label.size = NA, 
            # family="Arial", 
             size = 2.5) +
  # labels for edu in the middle of the graph
  geom_label(aes(x = edu, y = 0, label = edu, 
                 #family = "Inter – Light"
                 ),
             inherit.aes = F,
             size = 2.5, 
             label.padding = unit(0, "lines"), 
             label.size = 0,
             label.r = unit(0.0, "lines"), 
            # fill = "#EFF2F4", 
             alpha = 0.1, 
             color = "#5D646F"
             ) +
  coord_flip() +
  annotate(geom="text", 
           x=0.5, y= c(-18, 22), 
           label= c(paste0("All workers:", '$1,123'),"Total:3.0%") ,
           color="red",
           size = 2.25
  ) +
  annotate(geom="text", 
           x=8.5, y= c(-18, 12), 
           label= c("Median weekly earnings ($)","Unemployment rate (%)") ,
           color="#5D646F",
           size = 3.25
  ) +
  labs(title = "Earninges and Unemplyment rates, 2022",
       #subtitle = "Gender and age groups у 1990-2015 years, million people",
       caption = "Source: ABS labour force survey"
  ) +
  scale_color_manual(name = "", 
                     values = c(wage = "#4b0985",ue = "#2f005f"),
                     labels = c("Median Wage ($)", "Unemployment rate (%)")
                     ) +
  #theme_minimal(base_family = "Arial") +
  theme_minimal() +
  guides(x = 'none') +
  theme(
        #text = element_text(color = "#4b0985"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
       # plot.subtitle = element_text(size = 16, margin = margin(b = 10), hjust = 0.030),
        plot.caption = element_text(size = 6, margin = margin(b = 10, t = 10), color = "#5D646F"),
        #axis.text.x = element_text(size = 12, color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        # legend.position = "top",
        # legend.margin  = unit(0.1, "lines"),
        # legend.text  = element_text(
        #               #family = "Arial",
        #               size = 11),
        # legend.text.align = 0
       legend.position="none"
       )  

 



