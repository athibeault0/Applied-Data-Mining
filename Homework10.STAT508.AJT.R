#Data Assignment Lesson 10
#Written by Arielle Thibeault

#1. Training set containing a random sample of 800 observations, and a test set 
#containing the remaining observations

library(tidyverse)
library(ggthemes)
library(caret)
library(ISLR)
library(e1071)
set.seed(17)

#1. Create a training set
n <- dim(OJ)[1]
n.train <- 800
train <- sample(1:n, n.train)
test <- (1:n)[-train]
n.test <- length(test)

#2. Fit a support vector classifier
svm.lin <- svm(Purchase~., data = OJ, kernel = "linear", cost = 0.01)
summary(svm.lin)

#3. What are the training and test error rates?

#training error
ypred.train.lin <- predict(svm.lin , newdata = OJ[train,])
table(predict = ypred.train.lin, truth = OJ[train,]$Purchase)
#       truth
#predict  CH  MM
#     CH 424  76
#     MM  60 240
mean(ypred.train.lin != OJ[train,]$Purchase) #0.17

#testing error
ypred.test.lin <- predict(svm.lin, newdata = OJ[test,])
table(predict = ypred.test.lin, truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 152  24
#     MM  17  77
mean(ypred.test.lin != OJ[test,]$Purchase) #0.1518519

#4. Use tune() to select the optimal cost
tune.lin <- tune(svm, Purchase~ ., data = OJ,
                 kernel = "linear", ranges = list(
                   cost = c(0.01, 0.1, 1, 10))
)
summary(tune.lin)
#   cost     error dispersion
#1  0.01 0.1691589 0.03453591
#2  0.10 0.1691589 0.02937283
#3  1.00 0.1700935 0.03406909
#4 10.00 0.1663551 0.03463412

#here we see that the cost = 10 value has the smallest CV error so we'll use this
#going forward

#5. Compute train and test error rates for new cost

#training error
ypred.train.lin.best <- predict(tune.out$best.model, newdata = OJ[train,])
table(predict = ypred.train.lin.best, truth = OJ[train,]$Purchase)
#       truth
#predict  CH  MM
#     CH 426  74
#     MM  58 242
mean(ypred.train.lin.best != OJ[train,]$Purchase) #0.165

#testing error
ypred.test.lin.best <- predict(tune.out$best.model, newdata = OJ[test,])
table(predict = ypred.test.lin.best, truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 153  24
#     MM  16  77
mean(ypred.test.lin.best != OJ[test,]$Purchase) #0.1481481

#6. Repeat 2 - 5 using SVM with a radial kernel, use a default gamma
svm.rad <- svm(Purchase~., data = OJ, kernel = "radial",  cost = 0.01)
summary(svm.rad)

ypred.train.rad <- predict(svm.rad, OJ[train,])
table(predict = ypred.train.rad,truth = OJ[train,]$Purchase)
#       truth
#predict  CH  MM
#     CH 479 297
#     MM   5  19
mean(ypred.train.rad != OJ[train,]$Purchase) #0.3775

ypred.test.rad <- predict(svm.rad, OJ[test,])
table(predict = ypred.test.rad,truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 167  91
#     MM   2  10
mean(ypred.test.rad != OJ[test,]$Purchase) #0.3444444

tune.rad <- tune(svm, Purchase~ ., data = OJ,
                 kernel = "radial", ranges = list(
                   cost = c(0.01, 0.1, 1, 10))
)
summary(tune.rad)
#cost     error dispersion
#1  0.01 0.3897196 0.06550210
#2  0.10 0.1859813 0.04304254
#3  1.00 0.1738318 0.04789815
#4 10.00 0.1841121 0.05521142

#the model for cost = 1 is the best

ypred.train.rad.best <- predict(tune.rad$best.model, newdata = OJ[train,])
table(predict = ypred.train.rad.best, truth = OJ[train,]$Purchase)
#       truth
#predict  CH  MM
#     CH 442  86
#     MM  42 230
mean(ypred.train.rad.best != OJ[train,]$Purchase) #0.16

ypred.test.rad.best <- predict(tune.rad$best.model, newdata = OJ[test,])
table(predict = ypred.test.rad.best, truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 155  22
#     MM  14  79
mean(ypred.test.rad.best != OJ[test,]$Purchase) #0.1333333

#7. Repeat 2 - 5 using SVM with a 2nd degree polynomial kernel, use a default gamma
svm.poly <- svm(Purchase~., data = OJ, kernel = "polynomial",  cost = 0.01, degree = 2)
summary(svm.poly)

ypred.train.poly <- predict(svm.poly, OJ[train,])
table(predict = ypred.train.poly,truth = OJ[train,]$Purchase)
#        truth
#predict  CH  MM
#     CH 479 297
#     MM   5  19
mean(ypred.train.poly != OJ[train,]$Purchase) #0.3775

ypred.test.poly <- predict(svm.poly, OJ[test,])
table(predict = ypred.test.poly,truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 167  91
#     MM   2  10
mean(ypred.test.poly != OJ[test,]$Purchase) #0.3444444

tune.poly <- tune(svm, Purchase~ ., data = OJ,
                 kernel = "polynomial", degree = 2, 
                 ranges = list(cost = c(0.01, 0.1, 1, 10))
)
summary(tune.poly)
#cost     error dispersion
#1  0.01 0.3691589 0.04208491
#2  0.10 0.3009346 0.03140088
#3  1.00 0.1943925 0.02523172
#4 10.00 0.1757009 0.03077655

#the model for cost = 10 is the best

ypred.train.poly.best <- predict(tune.poly$best.model, newdata = OJ[train,])
table(predict = ypred.train.poly.best, truth = OJ[train,]$Purchase)
#       truth
#predict  CH  MM
#     CH 440  81
#     MM  44 235
mean(ypred.train.poly.best != OJ[train,]$Purchase) #0.15625

ypred.test.poly.best <- predict(tune.poly$best.model, newdata = OJ[test,])
table(predict = ypred.test.poly.best, truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 158  22
#     MM  11  79
mean(ypred.test.poly.best != OJ[test,]$Purchase) #0.1222222