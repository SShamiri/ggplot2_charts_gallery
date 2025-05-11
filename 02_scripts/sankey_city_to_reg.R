library(tidyverse)
#library(readxl)
library(janitor)
library(ggalluvial)
library(showtext)
library(ggsci)
library(ggrepel)
library(ggtext)

# setwd("C:/Developer")
# dat_raw <- read_excel("pop_flow_city_to_reg.xlsx", sheet = "data")

IN_DIR <- file.path("01_data")

## Load data --------------------------------------
dat_raw <- read_csv(file.path(IN_DIR, "city_to_reg.csv"))
pop_raw <- arrow::read_parquet("/Users/samuelshamiri/projects/lf_sim_old/01_data/01_clean/spop_census_2021_visa_countdown.parquet") |>
  select(gccsa_code)

## data prep
dat <- dat_raw |>
 filter(type == "pop") |>
  pivot_longer(!period:tlt, names_to = "Reg", values_to = "n") |>
  clean_names() |>
  rename(state = city) |>
  mutate(prop = n/tlt)

# pop_df <- pop_raw |>
#   group_by(gccsa_code) |>
#   summarise(state_pop = n())
# 
#  dat = dat |>
#   left_join(pop_df, join_by(state == gccsa_code)) |>
#   left_join(pop_df |> rename(reg_pop = state_pop), join_by(reg == gccsa_code)) |>
#    mutate(
#      scale = state_pop/reg_pop,
#      n = scale * n,
#      n = GFE::round_preserve_sum(n, 0)
#    )
# 
# dat |> 
#   select(state, reg,  pop, n) |> 
#   pivot_wider( names_from = reg, values_from = n) |> 
#   clipr::write_clip()

# Chart ----

# Define the color palette
state_col <- c(
  "1GSYD" = "#FFB400", 
  "2GMEL" = "#C20008", 
  "3GBR" = "#00c9a7", 
  "4GADL" = "#8E038E", 
  "5GPER" = "#595A52", 
  "6GHOB" = "#c34a36", 
  "7GDAR" = "#ffc75f", 
  "8ACTE" = "#13AFEF")

reg_col <- c(
  "1RNSW" = colorspace::lighten("#FFB400", .25, space = "HLS"),
  "2RVIC" = colorspace::lighten("#C20008", .2, space = "HLS"),
  "3RQLD" = colorspace::lighten("#00c9a7", .15, space = "HLS"), 
  "4RSAU" = colorspace::lighten("#8E038E", .2, space = "HLS"), 
  "5RWAU" = colorspace::lighten("#595A52", .15, space = "HLS"), 
  "6RTAS" = colorspace::lighten("#c34a36", .15, space = "HLS"), 
  "7RNTE" = colorspace::lighten("#ffc75f", .25, space = "HLS")
)

# set up the colours
colr_ln <- unique(c(dat$state, dat$reg))
#grid_colr <- c("#4b0985", "#2f005f", "#d5a3f9","#d2de5a","#0f2532", "#b91c1c", "#005d5d", "#b98c1c")
#grid_colr <-  rainbow(length(colr_ln))
grid_colr <- c(state_col, reg_col)
grid_colr <- setNames(grid_colr, colr_ln)

column_colr <- grid_colr
strat_colr <- c(rev(grid_colr))
# plot
ggplot(dat,
        aes(y = n, axis1 = reg, axis2 = state)
) +
  geom_alluvium(aes(fill = state), width = 0) +
  scale_fill_manual(values = column_colr) +
  geom_stratum(width = 1/12, fill = paste0(strat_colr), color = "white", lwd= 1.2) +
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

    
