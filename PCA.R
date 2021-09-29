library(caret)
library(tidyverse)

data(tecator)
cor(absorp)

?tecator


pca_model <- prcomp(absorp)
plot(pca_model$sdev)


new_absorp <- predict(pca_model, newdata = absorp)
cor(new_absorp)
new_absorp <- as.data.frame(new_absorp)
new_absorp[, 1:6]
new_absorp$response <- endpoints[, 1]


mod <- lm(response ~ ., data = new_absorp)
summary(mod)

target <- dat$target

dat$id <- NULL
dat$target <- NULL
pca_model <- prcomp(dat)
plot(pca_model$sdev)

pca_train <- predict(pca_model, newdata = dat)
pca_train <- pca_train[,1:15]
pca_train <- as.data.frame(pca_train)
pca_train$target <- target
