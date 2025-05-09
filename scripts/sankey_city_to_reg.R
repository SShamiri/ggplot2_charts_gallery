library(tidyverse)
library(readxl)
library(janitor)
library(ggalluvial)
library(showtext)
library(ggsci)
library(ggrepel)
library(ggtext)



# options(download.file.method = "wininet")
# install.packages("ggsci")

setwd("C:/Developer")

dat_raw <- read_excel("pop_flow_city_to_reg.xlsx", sheet = "data")

dat <- dat_raw |>
  pivot_longer(!State:Total, names_to = "Reg", values_to = "n") |>
  clean_names()

# Chart ----

# set up the colours
colr_ln <- unique(c(dat$state, dat$reg))
#grid_colr <- c("#4b0985", "#2f005f", "#d5a3f9","#d2de5a","#0f2532", "#b91c1c", "#005d5d", "#b98c1c")
grid_colr <-  rainbow(length(colr_ln))
grid_colr <- setNames(grid_colr, colr_ln)

column_colr <- grid_colr
strat_colr <- c(rev(grid_colr))



ggplot( dat,
        aes(y = n, axis1 = reg, axis2 = state)
) +
  geom_alluvium(aes(fill = state), width = 0) +
  scale_fill_manual(values = column_colr) +
  geom_stratum(width = 1/12, fill = paste0(strat_colr), color = "white", lwd= 2) +
  scale_x_discrete(limits = c("reg", "state"),
                   #labels = c("State of Origin", "State of destination"), # add as annotation
                   expand = c(.1, .1)) +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = 1.04, colour = "white", linewidth = 2) +
  geom_vline(xintercept = 1.958, colour = "white", linewidth = 2) +
  annotate(
    geom = "text",
    x = 0.9,
    y = 200,
    label = "Region of destination",
    size = 3,
    color = "firebrick",
    fontface = "bold",
  #  angle = 90
  lineheight = 1, 
  hjust = "left"
  ) +
  annotate(
    geom = "text",
    x = 2.1,
    y = 5.2,
    label = "State of origin" ,
    size = 3,
    color = "firebrick",
    fontface = "bold",
    lineheight = 1, 
    hjust = "left"
    #angle = 90
  ) +
 # theme(legend.position = "none") +
  ylab(NULL) +
  xlab(NULL) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            #color =  strat_colr, 
            color = "white",
            fontface = "bold", size = 3
  ) +
  coord_flip() +
  labs(
    title = "***Capital cities to regional migration***",
    subtitle = "**Population Flow 2016-2021***",
    caption = 'Data: Regional Australia Institute \n Chart: Regional Workforce Assessment') +
  theme_minimal() +
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
        panel.grid.major.x = element_blank()
)

    
