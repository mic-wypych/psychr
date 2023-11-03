## vectors

library(microbenchmark)

v1 <- rep(c("a","b","c"), 1e7)
v2 <- rep(c("a","b","c"), 1e5)



microbenchmark(c(v1,v2), append(v1,v2))


## homework ideas

#' can I make some simple math problem (like probabilities stuff) and make students code it in R?
#' 
#' Something like: Suppose you are interested in assessing how effective a given drug is
#' You have the following data: Out of 900 people who took the drug 657 got better
#' Out of 1000 people who did not take the drug, 540 got better. Represent this information
#' as a dataframe with columns coding those who took a drug or didn't take it and rows representing those who got better or not.
#' Next calculate the percentage of people who got better in the drug and no drug conditions.


#' Create a 
#' 

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


midwest %>%
  group_by(state)%>%
  reframe(q_10 = quantile(midwest$area, c(.1, .5, .9))) %>%
  


quantile(midwest[which(midwest$state == "IN"),]$area, c(.1,.5,.9))



list_q <- tapply(midwest$percadultpoverty, midwest$state, quantile, c(.1,.5,.9))

df_q <- data.frame()
df_q <- bind_rows(list_q[[1]], list_q[[2]], list_q[[3]], list_q[[4]], list_q[[5]])
colnames(df_q) <- c("q_10", "q_50", "q_90")

plot(1:5, df_q$q_10, ylim = c(0,20), type = "l", col = 2, lwd = 2, lty = 2)
lines(1:5, df_q$q_50, col = 4, lwd = 2, lty = 1)
lines(1:5, df_q$q_90, col = 2, lwd = 2, lty = 2)
axis(side = 3, at = 1:5, labels = unique(midwest$state))


midwest %>%
  ggplot() +
  geom_point(aes(x = percadultpoverty, y = percollege, color =popdensity), alpha = .8) +
  scale_color_gradient(values = c("#AAAAAA", "#CCCFFF"))


midwest %>%
  summarise(mean_perblack = mean(percblack),
            mean_percasian = mean(percasian),
            mean_percind = mean(percamerindan),
            mean_percother = mean(percother), .by = state) %>%
  pivot_longer(cols = mean_perblack:mean_percother, names_to = "group", values_to = "percent")


midwest %>%
  group_by(state) %>%
  summarise(mean_pct_college = mean(percollege),
            sd_pct_college = sd(percollege),
            n = n(),
            se_pct_college = sd_pct_college/sqrt(n()))

test <- ecdf(midwest$percollege)
midwest %>%
  ggplot(aes(x = percollege)) +
  stat_function(fun = test, geom = "line") +
  geom_density()



### tidying data ----

data("cms_patient_experience")
d
glimpse(cms_patient_experience)


iris


apply(iris[,sapply(iris, is.numeric)], 2, mean)



library(jsonlite)

read_json("https://collectionapi.metmuseum.org/public/collection/v1/search?q=Abakanowicz", simplifyVector = T)


#create some data

condition <- rep(c("exp", "control"), 100)
gender_pop <- rep(c("male", "female"),100)
gender<- sample(gender_pop, 100, replace = T)
age <- round(rnorm(100, 50, 15), 0)
attention <- ifelse(condition == "exp",  sample(1:6, 100, replace = T, prob = c(.05,.1,.2,.2,.3,.15)), sample(1:6, 100, replace = T, prob = c(.15,.3,.2,.2,.1,.05)))
reading <- 

flat_for_load <- data.frame(condition, attention, gender, age)
write.csv(flat_for_load, "data/experiment_read_attention.csv")


#create excelk data
#data split by some countries?

#sheet 1
state <- rep("PL", 350)
score_1 <- rnorm(350, 100, 15)
city <- sample(c("Rural", "City below 20k", "City between 20k and 50k", "City aboe 50k"), 350, replace = T)
score_2 <- rnorm(350, 50, 6.3)
n_obs <- rbinom(350, 3, .3)

sheet_1 <- data.frame(state, score_1, city, score_2, n_obs)
#sheet 2
state <- rep("DE", 430)
score_1 <- rnorm(430, 105, 13)
city <- sample(c("Rural", "City below 20k", "City between 20k and 50k", "City aboe 50k"), 430, replace = T)
score_2 <- rnorm(430, 43, 7.3)
n_obs <- rbinom(430, 3, .5)

sheet_2 <- data.frame(state, score_1, city, score_2, n_obs)

# sheet 3
N <- 133
state <- rep("SK", N)
score_1 <- rnorm(N, 93, 10.6)
city <- sample(c("Rural", "City below 20k", "City between 20k and 50k", "City aboe 50k"), N, replace = T)
score_2 <- rnorm(N, 51, 8.1)
n_obs <- rbinom(N, 3, .2)

sheet_3 <- data.frame(state, score_1, city, score_2, n_obs)

#sheet 4
N <- 542
state <- rep("FR", N)
score_1 <- rnorm(N, 99, 6.4)
city <- sample(c("Rural", "City below 20k", "City between 20k and 50k", "City aboe 50k"), N, replace = T)
score_2 <- rnorm(N, 57, 4.3)
n_obs <- rbinom(N, 3, .7)

sheet_4 <- data.frame(state, score_1, city, score_2, n_obs)

#sheet 5
N <- 97
state <- rep("CH", N)
score_1 <- rnorm(N, 103, 11)
city <- sample(c("Rural", "City below 20k", "City between 20k and 50k", "City aboe 50k"), N, replace = T)
score_2 <- rnorm(N, 53, 7.6)
n_obs <- rbinom(N, 3, .4)

sheet_5 <- data.frame(state, score_1, city, score_2, n_obs)


library(writexl)
write_xlsx(list("PL" = sheet_1, "DE" = sheet_2, "SK" = sheet_3, "FR" = sheet_4, "CH" = sheet_5), path = "data/geo_scores.xlsx")
