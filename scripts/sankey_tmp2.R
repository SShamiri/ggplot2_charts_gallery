library(tidyverse)
library(ggplot2)
library(ggalluvial)

# Load data ----
lkup_df <- read_csv("C:\\Developer\\lf_sim\\01_data\\03_lkup\\gccsa_state_lkup.csv")
dat_raw <- arrow::read_parquet("C:\\Developer\\lf_sim\\01_data\\02_tm\\tm_nim_gccsa.parquet")

dat <- dat_raw |>
  left_join(lkup_df |> select(gccsa_code,state = state_abrev ), join_by(gccsa_code)) |>
  left_join(lkup_df |> select(dest_gccsa = gccsa_code,dest_state = state_abrev ), join_by(dest_gccsa)) |>
  group_by(year, state, dest_state) |>
  summarise(n = sum(value), .groups = 'drop') |>
  # remove migration to same state
  mutate(n = ifelse(state == dest_state, 0, n),
         n = n/1000
  ) |>
  filter(n !=0) |>
  filter(year == 2022) |>
  select(-year)

# Chart ----

# set up the colours
colr_ln <- unique(c(dat$state, dat$dest_state))
grid_colr <- c("#4b0985", "#2f005f", "#d5a3f9","#d2de5a","#0f2532", "#b91c1c", "#005d5d", "#b98c1c")
grid_colr <- setNames(grid_colr, colr_ln)

column_colr <- grid_colr
strat_colr <- c(rev(grid_colr),rev(grid_colr))



ggplot( dat,
    aes(y = n, axis1 = dest_state, axis2 = state)
    ) +
  geom_alluvium(aes(fill = state), width = 0) +
  scale_fill_manual(values = column_colr) +
  geom_stratum(width = 1/15, fill = paste0(strat_colr), color = "white", lwd= 2) +
  scale_x_discrete(limits = c("dest_state", "state"),
                   #labels = c("State of Origin", "State of destination"), # add as annotation
                   expand = c(.1, .1)) +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = 1.04, colour = "white", linewidth = 2) +
  geom_vline(xintercept = 1.958, colour = "white", linewidth = 2) +
  annotate(
    geom = "text",
    x = 1.12,
    y = -4,
    label = "State of destination",
    size = 3,
    color = "firebrick",
    fontface = "bold",
    lineheight = .9,
    angle = 90
  ) +
  annotate(
    geom = "text",
    x = 1.92,
    y = -4,
    label = "State of origin" ,
    size = 3,
    color = "firebrick",
    fontface = "bold",
    lineheight = .9,
    angle = 90
  ) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(color = "black", size=11, angle=90, vjust=.8, hjust=0.8),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
        ) +
  theme(legend.position = "none") +
  ylab(NULL) +
  xlab(NULL) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
             #color =  strat_colr, 
             color = "white",
             fontface = "bold", size = 3
             ) +
   coord_flip()






# generate some example data----
somelongnames <- c("homo sapiens", "homo sapiens", letters[18],
                   "some other long name", letters[seq(4)])

df <- data.frame(x = factor(somelongnames),
                 y = factor(c("this label is long", "Golgi", 
                              letters[13:18])),
                 count = c(2, 10, 4, 5, 5, 1, 9, 3))

ll <- unique(c(as.character(df$x), as.character(df$y)))
grid.col <- rainbow(length(ll))
grid.col <- setNames(grid.col, ll)

# set colours for alluvial plot (this is a little tricky as strata 
# colour ordering is required for ggalluvial strata)
names(df) <- c("Condition1", "Condition2", "value")
levs1 <- levels(df$Condition1) 
levs2 <- levels(df$Condition2)
res1 <- unique(df$Condition1)
res2 <- unique(df$Condition2)
cond1_cols <- grid.col[levs1[levs1 %in% res1]]
cond2_cols <- grid.col[levs2[levs2 %in% res2]]
columnCols <- c(cond1_cols, cond2_cols)
stratCols <- c(rev(cond1_cols), rev(cond2_cols))




# plot alluvial diagram
q <- ggplot(df,
            aes(y = value, axis1 = Condition2, axis2 = Condition1)) +
  geom_alluvium(aes(fill = Condition1), width = 0) +
  scale_fill_manual(values = columnCols) +
  geom_stratum(width = 1/8, fill = paste0(stratCols), color = "white") +
  scale_x_discrete(limits = c("Condition1", "Condition2"), 
                   expand = c(.1, .1)) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(legend.position = "none") +
  ylab(NULL)

# add labels
q +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)),
             color = stratCols, fontface = "bold", size = 3) # +
  #scale_x_discrete(limits=rev) +
  #coord_flip() 

q + ggrepel::geom_text_repel(
  aes(label = ifelse(Condition1 == as.character(res1)[1],as.character(res1)[1], NA)),
  stat = "stratum", size = 4, 
  direction = "y", nudge_x = -.5
) 
