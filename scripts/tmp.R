# Plotting bidirectional horizontal bar chart ----


## Load Packages ----
packages <- c(
  "tidyverse",
  "forcats",
  "ggplot2",
  "systemfonts",
  "ggpol",
  "grid",
  "gtable"
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

dd <- edu_df %>% 
  #mutate(wage = wage/10^3) %>%
  pivot_longer(!edu, names_to = 'var', values_to = 'val') %>% 
  group_by(var) %>% 
  mutate(vv = val/sum(val),
         vv = ifelse(var == 'ue', -1* vv, vv))

p = ggplot(dd, aes(x = edu, y = vv, fill = var)) +
  geom_bar(stat = "identity") +
  facet_share(~var, dir = "h", scales = "free", reverse_num = TRUE) +   # note: scales = "free"
  coord_flip() +
  theme_bw() +
  #theme_minimal() #+
 # labs(y = "Count", x = "Age Band", title = " ") #+
  scale_fill_manual(values = c("pink", "blue")) +
theme(plot.margin = margin(t = 25, r = 25, b = 10, l = 0)) 
p

gp <- ggplotGrob(p)

gp$layout #helps you to understand the gtable object 
gtable_show_layout(gp) #helps you to understand the gtable object 
gp$widths
gp$widths[14] <- unit(0, 'cm') # you can modify this to your liking

grid.newpage()
grid.draw(gp)

##
df <- data.frame(age = sample(1:20, 1000, replace = TRUE), 
                 gender = c("M","F"), levels = c("M", "F"))

# Get the count per age and sex
df$count <- 1
df$age = paste(df$age, "some long string that is too long") # Added long labels
df <- aggregate(count ~ gender + age, data = df, length)

# For the horizontally shared axis, if we want to mirror the axes,
# we have to multiply the first panel by -1, and use coord_flip().
df_h <- df 
df_h$count = ifelse(df_h$gender == "F", df_h$count * -1, df_h$count)


pp <- 
  ggplot(df_h, aes(x = factor(age), y = count, fill = gender)) + 
  geom_col() +
  coord_flip()+
  facet_share(~ gender, dir = "h", scales = "free", reverse_num = TRUE) +
  labs(x = "Age", y = "Count") + 
  theme(legend.position = "bottom")

pp

gpp <- ggplotGrob(pp)

gpp$layout #helps you to understand the gtable object 
#gtable_show_layout(gp) #helps you to understand the gtable object 
gpp$widths[4] <- unit(0, 'cm') # you can modify this to your liking

grid.newpage()
grid.draw(gpp)

####
df <- tibble(
  Population = c(5, 8.7, 16.7, 24.8, 38, -4.6, -6.4, -16.1, -39.6, -55.3),
  Gender = c("Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female"),
  AgeBand = c("65-69", "70-74", "75-79", "80-84", "85+", "65-69", "70-74", "75-79", "80-84", "85+")
)
ggplot(df, aes(x = AgeBand, y = Population, fill = Gender)) +
  geom_bar(stat = "identity") +
  facet_share(~Gender, dir = "h", scales = "free", reverse_num = TRUE) +   # note: scales = "free"
  coord_flip() +
  theme_bw() +
  #theme_minimal() #+
  labs(y = "Count", x = "Age Band", title = " ") +
  scale_fill_manual(values = c("pink", "blue"))
