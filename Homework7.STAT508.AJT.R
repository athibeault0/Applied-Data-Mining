#Data Assignment Lesson 7
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

#Use the full data set to perform a logistic regression with Direction as the
#response and the five lag variables plus Volume as predictors. Use the summary 
#function to print the results. Do any of the predictors appear to be statistically 
#significant? If so, which ones?

Weekly.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = "binomial")
summary(Weekly.fit)
#from the summary, it appears that only the variable Lag2 (and the intercept) is 
#significant for alpha = 0.05, as all the coefficient estimate p-values are much
#larger than alpha

#Compute the confusion matrix and overall fraction of correct predictions. 
#Explain what the confusion matrix is telling you about the types of mistakes made 
#by logistic regression.

Weeklylog.prob <- predict(Weekly.fit, type = "response")
Weeklylog.pred <- rep("Down", length(Weeklylog.prob))
Weeklylog.pred[Weeklylog.prob > 0.5] = "Up"
table(Weeklylog.pred, Direction)
#using the generic formula (# of successes)/(# of trials) we see that the model
#predicted the stock market trend correctly 611/1089 = 56.11% of the time, which
#isn't great

#Now fit the logistic regression model using a training data period from 1990 to 
#2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall 
#fraction of correct predictions for the held out data (that is, the data from 2009 
#and 2010).

#the training data is data with the year less than 2009
train <- (Year < 2009)
Weekly.0910 <- Weekly[!train,]
Weekly.fit <- glm(Direction~Lag2, data = Weekly, family = "binomial", subset = train)
summary(Weekly.fit)
Weeklylog.prob <- predict(Weekly.fit, Weekly.0910, type = "response")
Weeklylog.pred <- rep("Down", length(Weeklylog.prob))
Weeklylog.pred[Weeklylog.prob > 0.5] = "Up"
Direction.0910 <- Direction[!train]
table(Weeklylog.pred, Direction.0910)
#here, the model predicted the stock market trend correctly 65/104 = 62.50% of
#of the time. This prediction accuracy with the training data  and only predictor
#Lag2 is better than the whole data set prediction error, but still low 
#(maybe average in realistic practice)

#Repeat using LDA

Weeklylda.fit <- lda(Direction~Lag2, data = Weekly, family = binomial, subset = train)
summary(Weeklylda.fit)
Weeklylda.pred <- predict(Weeklylda.fit, Weekly.0910)
table(Weeklylda.pred$class, Direction.0910)
mean(Weeklylda.pred$class==Direction.0910)
#this model predicts the stock market trend correctly again 65/104 = 62.50% of
#the time

#Repeat using QDA

Weeklyqda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
summary(Weeklyqda.fit)
Weeklyqda.pred <- predict(Weeklyqda.fit, Weekly.0910)$class
table(Weeklyqda.pred, Direction.0910)
mean(Weeklyqda.pred==Direction.0910)
#this model predicts the stock market trend correctly 61/104 = 58.65% of the time

#Which of these methods appears to provide the best results on this data?

#Experiment with different combinations of predictors, including possible 
#transformations and interactions, for each of the methods. Report the variables, 
#method, and associated confusion matrix that appears to provide the best results 
#on the held out data.

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