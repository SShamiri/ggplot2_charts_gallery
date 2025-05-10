library(tidyverse)
library(ggplot2)
library(ggsankey)

lkup_df <- read_csv("C:\\Developer\\lf_sim\\01_data\\03_lkup\\gccsa_state_lkup.csv")

dat <- arrow::read_parquet("C:\\Developer\\lf_sim\\01_data\\02_tm\\tm_nim_gccsa.parquet")
dat

df <- dat |>
  left_join(lkup_df |> select(gccsa_code,state_abrev ), join_by(gccsa_code)) |>
  left_join(lkup_df |> select(dest_gccsa = gccsa_code,dest_state = state_abrev ), join_by(dest_gccsa)) |>
  group_by(year, state_abrev, dest_state) |>
  summarise(n = sum(value), .groups = 'drop') |>
  # remove migration to same state
  mutate(n = ifelse(state_abrev == dest_state, 0, n)) |>
  filter(n !=0) |>
  # take single year
  filter(year == 2022) |>
  uncount(n) |>
  select(-year) |>
  make_long(state_abrev, dest_state)
  

# Chart 1
pl <- ggplot(df, aes(x = x
                     , next_x = next_x
                     , node = node
                     , next_node = next_node
                     , fill = factor(node)
                     , label = node)
)

pl

pl <- pl + geom_sankey(flow.alpha = 0.5
                      , node.color = "black"
                      ,show.legend = FALSE) +
        coord_flip() +
  geom_vline(xintercept = 1.04, color = "white", linewidth = 1) + 
  geom_vline(xintercept = 1.958, color = "white", linewidth = 1)

pl <- pl + geom_sankey_label(size = 3, color = "black", fill= "white") + 
  theme_bw() +
  theme(legend.position = "none")

# pl <- pl +geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5)
# pl <- pl +  theme_bw()
#pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
pl <- pl + scale_fill_viridis_d(option = "inferno")
# pl <- pl + labs(title = "Sankey diagram using ggplot")
# pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
# pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')
pl +
  geom_vline(xintercept = 1.04, color = "white", linewidth = 1) + 
  geom_vline(xintercept = 1.958, color = "white", linewidth = 1)
#geom_hline(yintercept = 0, colour = "white", linewidth = )

