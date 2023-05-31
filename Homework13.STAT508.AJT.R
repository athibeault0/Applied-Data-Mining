#Data Assignment 13
#Written by Arielle Thibeault

library(ISLR2)
attach(Wage)
summary(Wage)
names(Wage)

#data visualizations
par(mfrow=c(2,4))
for (i in 3:9) {
  plot(Wage[,i], main = "", ylab = "Freq", xlab = names(Wage)[i])
}
for (i in 1:3) {
  hist(Wage[,i], main = "", ylab = "Freq", xlab = names(Wage)[i])
}
for (i in 10:11) {
  hist(Wage[,i], main = "", ylab = "Freq", xlab = names(Wage)[i])
}
summary(Wage$age)
summary(Wage$wage)

#here we will clean up some of the variables so we can use them in our models
Wage$maritl <- as.numeric(Wage$maritl)
Wage$race <- as.numeric(Wage$race)
Wage$education <- as.numeric(Wage$education)
Wage$region <- as.numeric(Wage$region)
Wage$jobclass <- as.numeric(Wage$jobclass)
Wage$health <- as.numeric(Wage$health)
Wage$health_ins <- as.numeric(Wage$health_ins)

#Which predictors do we need? 
model1 <- lm(wage ~ maritl + race + education + jobclass + health + health_ins, data = Wage)
summary(model1)
#we notice that all predictors apart from race are significant predictors
#for wage at the 0.01 confidence level
model1a <- lm(wage ~ maritl + education + jobclass + health + health_ins, data = Wage)
summary(model1a)
#this model again but without race
anova(model1, model1a)

#Polynomial Regression
fit.1 <- lm(wage ~ maritl, data = Wage)
fit.2 <- lm(wage ~ poly(maritl, 2), data = Wage)
fit.3 <- lm(wage ~ poly(maritl, 3), data = Wage)
fit.4 <- lm(wage ~ poly(maritl, 4), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4)
#the p-value comparing the second degree model and the first has a 
#p-value of approx 0 and appears to provide a better fit to the data
#than the base model
coef(fit.2)

fit.1 <- lm(wage ~ race, data = Wage)
fit.2 <- lm(wage ~ poly(race, 2), data = Wage)
fit.3 <- lm(wage ~ poly(race, 3), data = Wage)
anova(fit.1, fit.2, fit.3)
#the third degree model
coef(fit.3)

fit.1 <- lm(wage ~ education, data = Wage)
fit.2 <- lm(wage ~ poly(education, 2), data = Wage)
fit.3 <- lm(wage ~ poly(education, 3), data = Wage)
fit.4 <- lm(wage ~ poly(education, 4), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4)
#second degree model
coef(fit.2)

fit.1 <- lm(wage ~ jobclass, data = Wage)
anova(fit.1)

fit.1 <- lm(wage ~ health, data = Wage)
anova(fit.1)

fit.1 <- lm(wage ~ health_ins, data = Wage)
anova(fit.1)

#now that we've addressed the polynomial aspect to each variable, let's compare
#the model with no polynomial elements to one with the preferred polynomial
#degree per predictor
model2 <- lm(wage ~ poly(maritl, 2) + poly(race, 3) + poly(education, 2) + jobclass + health + health_ins, data = Wage)
model2a <- lm(wage ~ poly(maritl, 2) + poly(education, 2) + jobclass + health + health_ins, data = Wage)
anova(model1, model1a, model2, model2a)
#from the output we see that the third and fourth models have the smaller RSS
#values with the third model having the smallest at 3493158, with 6 degrees
#of freedom, an F statistic of 36.2375, and a p-value of approximately 0
coef(model2)

#now some predictions
preds <- predict(model2, newdata = Wage, se = TRUE)
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(model2, col = "darkgrey")

#Splines
library(splines)
#let's look at the education groups and see what we can discern from each
fit <- lm(wage ~ bs(education, knots = c(1.5, 2.5, 3.5, 4.5)), data = Wage)
pred <- predict(fit, newdata = Wage, se = T)
plot(education, wage, col = "grey")
lines(fit)
#this doesn’t really work for non-continuous variables

#Local regression
fit <- loess(wage ~ education, span = .2, data = Wage)
fit2 <- loess(wage ~ education, span = .5, data = Wage)
plot(education, wage, cex = .5, col = "darkgrey")

plot(education, wage, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ education, span = .2, data = Wage)
fit2 <- loess(wage ~ education, span = .5, data = Wage)
lines(length(fit), fit)

#local regression building blocks in GAMs
library(gam)
fit1 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage) #the model we ran in Lab
fit2 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education + jobclass, data = Wage)
fit3 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education + maritl, data = Wage)
fit4 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education + jobclass + maritl, data = Wage)
anova(fit1, fit2, fit3, fit4)
plot(fit2, se = T, col = "green")
coef(fit2)

fit5 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education + health, data = Wage)
fit6 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education + health_ins, data = Wage)
fit7 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education + jobclass + maritl + health + health_ins, data = Wage)
anova(fit1, fit5, fit6, fit7)
plot(fit7, se = T, col = "green")
coef(fit7)

#finally, a comparison between the "best" of both model groups
anova(fit1, fit2, fit7)
#it seems like model 3 (fit7) is best here
preds <- predict(fit7, newdata = Wage)
plot.Gam(fit7, se = TRUE, col = "green")

#finally, a GAM with all the predictors
fit8 <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + maritl + race + education + jobclass + health + health_ins, data = Wage)
anova(fit7, fit8)
#so it appears fit7 is the best here
anova(fit1, fit7)
plot(wage, preds, xlim = c(40,300), ylim = c(40,300))
plot(fit7)
