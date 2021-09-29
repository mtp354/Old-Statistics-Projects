library(tidyverse)
library(ggplot2)
library(pls)
library(data.table)

dat <- read.csv("C:\\Users\\Matt\\Desktop\\Stats Stuff\\Applied Regression\\train.csv", stringsAsFactors = F)
test_raw <- read.csv("C:\\Users\\Matt\\Desktop\\Stats Stuff\\Applied Regression\\test.csv", stringsAsFactors = F)


dat$id <- NULL
target <- dat$target
dat$target <- NULL
N <- 1 


# mylist <- rbindlist(lapply(1:5, function(N) {
#   results <- replicate(50, {
#     train_indices <- sample(250, 230, T)
#     test_indices <- (1:250)[-train_indices]
# 
#     train <- dat[train_indices,]
#     test <- dat[test_indices,]
#     highly_correlated <- sort(abs(cor(train, target[train_indices])), index.return = T)$ix[(300 - N): 300]
#     paste(names(train)[highly_correlated], collapse = " + ")
#     f <- as.formula(paste("target ~ ",
#                           paste(names(train)[highly_correlated], collapse = " + ")))
#     train$target <- target[train_indices]
#     mod <- glm(formula = f, data = train, family = "binomial")
#     mod_final <- MASS::stepAIC(mod)
#     Metrics::auc(target[test_indices],
#                  predict(mod_final, newdata = test, type = "response"))
#   })
#   data.frame(N = N, mu = mean(results), sigma = sd(results))
# }))

dat <- read.csv("C:\\Users\\Matt\\Desktop\\Stats Stuff\\Applied Regression\\train.csv", stringsAsFactors = F)


mod <- glm(target ~ X129 + X156 + X217 + X43 + X65 + X33 + X91 + X283 + X217 + X114 + X298 + X105
           , data = dat, family = "binomial")
mod_final <- MASS::stepAIC(mod)

new_predictions <- predict(mod, newdata = test_raw, type = "response")
submit <- data.frame(id = test_raw$id, target = new_predictions)
write.csv(submit, "submissionExternal", row.names = F)



