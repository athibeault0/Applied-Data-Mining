#Data Analysis Assignment Lesson 6
#Written by Arielle Thibeault, and code taken from a previous assignment
install.packages("pls")
library(pls)

data(BostonHousing2)
dim(BostonHousing2)
View(BostonHousing2)
summary(BostonHousing2)

#below we're just going to get a feel of what the data looks like
plot(BostonHousing2$town, xlab = "Location (Town)", ylab = "Frequency", 
     main = "Observations Collected Per Town")

#to expand further, we'll pull a matrix view of histograms for the 
#quantitative variables in BostonHousing2
par(mfrow=c(2,4))
for (i in 2:9) {
  hist(BostonHousing2[,i], main = "", ylab = "Freq", ylim = c(0, 500),
       xlab = names(BostonHousing2)[i])
}
for (i in 11:19) {
  hist(BostonHousing2[,i], main = "", ylab = "Freq", ylim = c(0, 500),
       xlab = names(BostonHousing2)[i])
}
BostonHousing2[,c("crim")] <- log(BostonHousing2[,c("crim")])

#plot
hist(BostonHousing2[, "crim"], main="", ylab="Freq", ylim = c(0, 100),
     xlab = paste(c("crim"), "_log", sep=""))
names(BostonHousing2)[which(names(BostonHousing2) == c("crim"))] = paste(c("crim"),
                                                                         "_log", sep="")
#checking the correlation before fitting our lm
ggcorr(BostonHousing2, low = "springgreen4", mid = "white", high = "hotpink4") +
  ggtitle(label = "Correlation Plot")
cor.test(BostonHousing2$rad,BostonHousing2$tax)
cor.test(BostonHousing2$crim_log,BostonHousing2$tax)

#removing highly correlated variables and "Town" as it will only be an indicator
BostonHousing3 <- BostonHousing2[,-c(1,5,15)]

#Now we start with PCR
x <- model.matrix(crim_log~., BostonHousing3)[, -1]
y <- BostonHousing3$crim_log
train <- sample(1:nrow(x), nrow(x) / 2) 
test <- (-train)
y.test <- y[test]

set.seed(17)
pcr.fit <- pcr(crim_log~., data = BostonHousing3, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP", main = "MSEP Per Number of Pricipal Components")
MSEP(pcr.fit)
#We see that the minimum CV error is 0.7760 when there are 14 principal components

#PCR on the training data
set.seed(17)
pcr.fit <- pcr(crim_log~., data = BostonHousing3, subset = train,
               scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP", main = "MSEP Per Number of Pricipal Components")
MSEP(pcr.fit)
#In the training set, the minimum CV error is 0.8322 with 14 components 
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 14)
mean((pcr.pred - y.test)^2)
#Test MSE = 0.7809
pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 14)
summary(pcr.fit)
#this is the full data set PCR, with 99.43% of the variation explained by the
#14 principal components

#Moving onto PLS
set.seed (17)
pls.fit <- plsr(crim_log~., data = BostonHousing3, subset = train,
                scale = TRUE, validation = "CV")
summary(pls.fit)
#minimum train CV error is 0.9138 with 6 components
validationplot(pls.fit, val.type = "MSEP", main = "MSEP Per Number of Pricipal Components")
pls.pred <- predict(pls.fit, x[test, ], ncomp = 6)
mean((pls.pred - y.test)^2)
#The test MSE is 0.7718, higher than PCR
pls.fit <- plsr(crim_log~., data = BostonHousing3, scale = TRUE, ncomp = 6)
summary(pls.fit)

#Again, for 14 components
pls.pred <- predict(pls.fit, x[test, ], ncomp = 14)
mean((pls.pred - y.test)^2)
pls.fit <- plsr(crim_log~., data = BostonHousing3, scale = TRUE, ncomp = 14)
summary(pls.fit)
