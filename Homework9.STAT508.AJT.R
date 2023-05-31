#Data Assignment #9
#Written by Arielle Thibeault, with guidance from ISLR text

library(ISLR2)
library(corrplot)
library(MASS)
names(Weekly)
dim(Weekly)
summary(Weekly)

attach(Weekly)
plot(Volume, main = "Volume Distribution")

#data visualizations
par(mfrow=c(2,4))
for (i in 1:9) {
  hist(Weekly[,i], main = "", ylab = "Freq", ylim = c(0, 500),
       xlab = names(Weekly)[i])
}

corrplot(cor(Weekly[,-9]), method="square")

#kNN with k = 1
library(class)
train <- (Weekly$Year < 2009)
Weekly.train <- as.matrix(Weekly$Lag2[train])
Weekly.test <- as.matrix(Weekly$Lag2[!train])
train.Direction <- Weekly$Direction[train]
Direction.0910 <- Weekly$Direction[!train]

set.seed(17)
Weeklyknn.pred <- knn(Weekly.train, Weekly.test, train.Direction, k=1)
table(Weeklyknn.pred, Direction.0910)
#Weeklyknn.pred Down Up
#Down   21 29
#Up     22 32
mean(Weeklyknn.pred == Direction.0910)
#0.5096154

#Test some other values for k
#K=10
Weekly.train <- as.matrix(Lag2[train])
Weekly.test <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]

set.seed(17)
Weeklyknn.pred <- knn(Weekly.train, Weekly.test, train.Direction, k=10)
table(Weeklyknn.pred, Direction.0910)
mean(Weeklyknn.pred == Direction.0910)

#K=100
Week.train <- as.matrix(Lag2[train])
Week.test <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(17)
Weeklyknn.pred <- knn(Weekly.train, Weekly.test, train.Direction, k=100)
table(Weeklyknn.pred, Direction.0910)
mean(Weeklyknn.pred == Direction.0910)

#Different attempts
#Logistic Regression with Interaction Lag2:Lag4
Weekly.fit <- glm(Direction~Lag2:Lag4+Lag2, data = Weekly, family = "binomial", subset = train)
Weeklylog.prob <- predict(Weekly.fit, Weekly.0910, type = "response")
Weeklylog.pred <- rep("Down", length(Weeklylog.prob))
Weeklylog.pred[Weeklylog.prob > 0.5] = "Up"
Direction.0910 <- Direction[!train]
table(Weeklylog.pred, Direction.0910)
mean(Weeklylog.pred == Direction.0910)
#0.5769231

#LDA with Interaction Lag2:Lag4
Weeklylda.fit <- lda(Direction~Lag2:Lag4+Lag2, data = Weekly,family = "binomial", subset = train)
Weeklylda.pred <- predict(Weeklylda.fit, Weekly.0910)
table(Weeklylda.pred$class, Direction.0910)
mean(Weeklylda.pred$class==Direction.0910)
#0.5865385

#QDA with Interaction Lag2:Lag4
Weeklyqda.fit = qda(Direction ~ poly(Lag2,2), data = Weekly, subset = train)
Weeklyqda.pred = predict(Weeklyqda.fit, Weekly.0910)$class
table(Weeklyqda.pred, Direction.0910)
mean(Weeklyqda.pred==Direction.0910)
#0.625