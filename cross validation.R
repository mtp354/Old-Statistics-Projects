#customised cross-validation
library(tidyverse)
library(ggplot2)
library(pls)
library(Metrics)
library(data.table)

data <- read.csv("C:\\Users\\Matt\\Desktop\\Stats Stuff\\Applied Regression\\train.csv", stringsAsFactors = F)
data$id <- NULL


train_indices <- sample(1:250, 238, replace = F)
test_indices <- (1:250)[-train_indices]

train <- data[train_indices,]
test <- data[test_indices,]

target <- train$target
train$target <- NULL

#introducing tuning parameter
n <- 1
#Choosing highest 2 correlations
cor_indices <- sort(abs(cor(target, train)), index.return = T)$ix[(300-n):300]
RHS <- paste(names(train)[cor_indices], collapse = " + ")
f <- as.formula(paste(c("target ~ ", RHS), collapse = ""))

#model construction
train$target <- target
mod <- glm(formula = f, family = "binomial", data = train)
#summary(mod)
predicted <- predict(mod, newdata = test, type = "response")
#predicted

auc(test$target, predicted)



############################    Replicate       #########################################

auc_data <- replicate(500, {
  train_indices <- sample(1:250, 238, replace = F)
  test_indices <- (1:250)[-train_indices]
  
  train <- data[train_indices,]
  test <- data[test_indices,]
  
  target <- train$target
  train$target <- NULL
  
  #introducing tuning parameter
  n <- 1
  #Choosing highest 2 correlations
  cor_indices <- sort(abs(cor(target, train)), index.return = T)$ix[(300-n):300]
  RHS <- paste(names(train)[cor_indices], collapse = " + ")
  f <- as.formula(paste(c("target ~ ", RHS), collapse = ""))
  
  #model construction
  train$target <- target
  mod <- glm(formula = f, family = "binomial", data = train)
  #summary(mod)
  predicted <- predict(mod, newdata = test, type = "response")
  #predicted
  
  auc(test$target, predicted)
})
auc_data <- auc_data[is.finite(auc_data)]
mean(auc_data)
sd(auc_data)

#using the tuning parameter
rbindlist(lapply(1:5, function(n) {
  results <- replicate(50, {
    train_indices <- sample(250, 230, T)
    test_indices <- (1:250)[-train_indices]
    
    train <- data[train_indices,]
    test <- data[test_indices,]
    highly_correlated <- sort(abs(cor(train, target[train_indices])), index.return = T)$ix[(300 - n): 300]
    paste(names(train)[highly_correlated], collapse = " + ")
    f <- as.formula(paste("target ~ ", 
                          paste(names(train)[highly_correlated], collapse = " + ")))
    train$target <- target[train_indices]
    mod <- glm(formula = f, data = train, family = "binomial")
    mod_final <- MASS::stepAIC(mod)
    Metrics::auc(target[test_indices],
                 predict(mod_final, newdata = test, type = "response"))
  })
  data.frame(N = n, mu = mean(results), sigma = sd(results))
}))

