
#simulate data

y <- rnorm(10000)
mean(y)

ss <- c()
pred <- seq(-1,1,length.out = 10)
for (i in pred) {
  res <- sum((i - y)^2)
  ss <- c(ss, res)
}
ss
frame <- 1:length(ss)
my_df <-data.frame(pred,ss)

plot(pred,ss)

library(gganimate)

#animate the predictions appearing
a <- my_df %>%
  ggplot(aes(pred,ss, group = pred)) +
  geom_point(size = 5) +
  theme_minimal() +
  labs(x = 'our prediction', y = 'how much we miss (sum of squares)') +
  transition_reveal(along = pred)
animate(a)


my_df %>%
  ggplot(aes(pred,ss)) +
  geom_point(size = 5) +
  theme_minimal() +
  labs(x = 'our prediction', y = 'how much we miss (sum of squares)') +
  transition_time(pred) +
  shadow_mark(past = T, future=F, alpha=0.9)



# testing datasets ####
library(tidyverse)
d <- data("midwest")
glimpse(midwest)


table(midwest$state)
