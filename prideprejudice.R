#Project 3 Dracula and Prejudice
library(tidyverse)
library(nnet)
library(ggplot2)
library(stopwords)
train <- read.csv("C:\\Users\\Matt\\Desktop\\Stats Stuff\\Applied Regression\\Project 3\\Dracula and Prejudice\\train.csv")
test <- read.csv("C:\\Users\\Matt\\Desktop\\Stats Stuff\\Applied Regression\\Project 3\\Dracula and Prejudice\\test.csv")

stop_words <- stopwords(language = "en", source = "snowball")

train <- train %>% group_by(sentence, vampires) %>%
  summarise(prob_stop_words = mean(word %in% stop_words))

test <- test %>% group_by(sentence) %>%
  summarise(prob_stop_words = mean(word %in% stop_words))

mod <- glm(vampires ~ prob_stop_words, family = "binomial", data = train)

summary(mod)

pred <- predict.glm(mod, newdata = test, type = "response")


hist(pred)


# Metrics::logLoss(df$vampires, predict(mod, type = "response"))
