## multi densities on same line 
# link: https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

chic <- readr::read_csv("https://cedricscherer.com/data/chicago-nmmaps-custom.csv")
## only plot extreme season using dplyr from the tidyverse
ggplot(data = dplyr::filter(chic, season %in% c("Summer", "Winter")),
       aes(x = temp, y = year, fill = paste(year, season))) +
  geom_density_ridges(alpha = .7, rel_min_height = .01,
                      color = "white", from = -5, to = 95) +
  scale_fill_cyclical(breaks = c("1997 Summer", "1997 Winter"),
                      labels = c(`1997 Summer` = "Summer",
                                 `1997 Winter` = "Winter"),
                      values = c("tomato", "dodgerblue"),
                      name = "Season:", guide = "legend") +
  theme_ridges(grid = FALSE) +
  labs(x = "Temperature (°F)", y = "Year")
