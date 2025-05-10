library(ggimage)


# example 1: geom_image ----
x <- c(2, 2, 2, 2, 2, 3, 3, 3.5, 3.5, 4)
y <- c(2, 3, 4, 5, 6, 4, 6, 3, 5, 2)
d <- data.frame(x = x, y = y)

img <- ("https://www.r-project.org/logo/Rlogo.png")
ggplot(d, aes(x, y)) + geom_image(image = img, size = .1) +
  xlim(0, 6) + ylim(0, 7) +
  ggtitle("Plot R with Rstudio Symbols")

img <- list.files(system.file("extdata", package="ggimage"),
                  pattern="png", full.names=TRUE)

# example 2: geom_icon() ----
#geom_icon() provides support for icons on https://ionic.io/ionicons. 
d <- data.frame(x = rep(1:5, 3),
                y = (rep(3:1, each = 5)))

d$icon <- c('bed', 'fast-food', 'bus', 'business', 'book', 'call', 'ice-cream', 'mail-unread', 'musical-notes', 'flask', 'language', 'pizza', 'beer', 'walk' ,'bed')

ggplot(d, aes(x,y)) + 
  geom_icon(aes(image=icon)) + 
  xlim(0, 10) + ylim(0, 4) + 
  geom_text(aes(label = icon), size=2, vjust = -4) + 
  ggtitle("Daily Tasks in Icons") +
  theme_void()

# example 3: geom_emoji() ----
# Each emoji has a corresponding unicode. We can take a look at some examples of
#the Emoji unicode here https://apps.timwhitlock.info/emoji/tables/unicode.
library(emojifont)
search_emoji('work')
emoji(search_emoji('work'))

set.seed(0)
iris2 <- iris[sample(1:nrow(iris), 30), ]
model <- lm(Petal.Length ~ Sepal.Length, data = iris2)
iris2$fitted <- predict(model)

p <- ggplot(iris2, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_linerange(aes(ymin = fitted, ymax = Petal.Length),
                 colour = "red") +
  geom_abline(intercept = model$coefficients[1],
              slope = model$coefficients[2]) +
  ggtitle("Regression on Petal Length and Sepal Length")

p + ggimage::geom_emoji(
  aes(image = ifelse(abs(Petal.Length-fitted) > 0.5, '1f645', '1f600')), cex=0.06)

# example 4
library(personograph)

data <- list(first=0.89, second=0.06, third=0.05)
# With colors
personograph(data, n.icons=64, dimensions=c(8, 8), colors=list(first="grey", second="blue", third="red"))
