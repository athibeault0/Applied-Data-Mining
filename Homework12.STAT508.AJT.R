#Data Assignment Lesson 12
#Written by Arielle Thibeault

library(ISLR)
install.packages("tree")
library(tree)
set.seed(17)

#1. Create a training set containing a random sample of 800 observations, and a 
#test set containing the remaining observations.
n <- dim(OJ)[1]
train <- sample(1:n, 800)
test <- (1:n)[-train]

#2. Fit a tree to the training data, with Purchase as the response and the other 
#variables as predictors. Use the summary() function to produce summary statistics 
#about the tree, and describe the results obtained. What is the training error 
#rate? How many terminal nodes does the tree have?

tree.train <- tree(Purchase~ ., data = OJ[train,])
summary(tree.train)
#training error = 0.1688
#terminal nodes = 7
plot(tree.train)
text(tree.train, pretty=0)

ypred <- predict(tree.train, data = OJ[train,], type = 'class')
table(predict = ypred, truth = OJ[train,]$Purchase)
#       truth
#predict  CH  MM
#     CH 383  34
#     MM 101 282
mean(ypred != OJ[train,]$Purchase) #training error rate = 0.16875

#3. Type in the name of the tree object in order to get a detailed text output. 
#Pick one of the terminal nodes, and interpret the information displayed.

tree.train

#node), split, n, deviance, yval, (yprob)
#* denotes terminal node

#1) root 800 1073.00 CH ( 0.60500 0.39500 )  
#2) LoyalCH < 0.482304 304  332.80 MM ( 0.23684 0.76316 )  
#4) LoyalCH < 0.0842785 78   25.43 MM ( 0.03846 0.96154 ) *
#  5) LoyalCH > 0.0842785 226  278.10 MM ( 0.30531 0.69469 )  
#10) SalePriceMM < 2.04 125  122.30 MM ( 0.19200 0.80800 ) *
#  11) SalePriceMM > 2.04 101  138.80 MM ( 0.44554 0.55446 ) *
#  3) LoyalCH > 0.482304 496  451.20 CH ( 0.83065 0.16935 )  
#6) LoyalCH < 0.740621 221  277.50 CH ( 0.67873 0.32127 )  
#12) ListPriceDiff < 0.235 86  116.90 MM ( 0.41860 0.58140 )  
#24) DiscCH < 0.115 79  103.90 MM ( 0.36709 0.63291 ) *
#  25) DiscCH > 0.115 7    0.00 CH ( 1.00000 0.00000 ) *
#  13) ListPriceDiff > 0.235 135  116.70 CH ( 0.84444 0.15556 ) *
#  7) LoyalCH > 0.740621 275  104.70 CH ( 0.95273 0.04727 ) *

#Interpret node 24: 
#Split: root, LoyalCH > 0.482304, LoyalCH < 0.740621, ListPriceDiff < 0.235, DiscCH < 0.115
#n = 79 observations fit into this category
#deviance = 103.90
#yval = MM
#fraction of observations in this branch that take on MM and CH (0.36709, 0.63291)

#4. Create a plot of the tree, and interpret the results.

plot(tree.train)
text(tree.train, pretty=0)
#here we can see the splits of the tree from our tree model. The first split is
#for LoyalCH < 0.482304, then there are splits at LoyalCH < 0.0842785 and LoyalCH 
#< 0.740621, and so on. The splits here show us the significant criteria for reducing
#RSS by which following these paths for a certain observation would lead to a 
#classification that is the most likely 

#the most important indicator of Purchase seems to be LoyalCH, if it's value is less
#than 0.482304 we can follow the tree down to see that most observations will be
#classified as MM and observations with LoyalCH greater than 0.482304 are most 
#likely to be classified as CH

#5. Predict the response on the test data, and produce a confusion matrix comparing 
#the test labels to the predicted test labels. What is the test error rate?

tree.test <- tree(Purchase~ ., data = OJ[test,])
ypred <- predict(tree.test, newdata = OJ[test,], type = 'class')
table(predict = ypred, truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 159  16
#     MM  10  85
mean(ypred != OJ[test,]$Purchase) #test error rate = 0.0962963

#6. Apply the cv.tree() function to the training set in order to determine the 
#optimal tree size.

set.seed(17)
tree.cv <- cv.tree(tree.train, FUN = prune.misclass)
tree.cv

#7. Produce a plot with tree size on the x-axis and cross-validated classification 
#error rate on the y-axis.

plot(tree.cv$size, tree.cv$dev, type = 'b')

#8. Which tree size corresponds to the lowest cross-validated classification error rate?

#The tree size of 5 seems to have the lowest CV-error of 157. The tree size 7 also
#has a very similar error 158, but we prefer to use the 5 size because it will
#simplify the tree and increase interpretability

#9. Produce a pruned tree corresponding to the optimal tree size obtained using 
#cross-validation. If cross-validation does not lead to selection of a pruned 
#tree, then create a pruned tree with five terminal nodes.

tree.prune <- prune.tree(tree.train, best = 5)
plot(tree.prune)
text(tree.prune, pretty = 0)

#10. Compare the training error rates between the pruned and unpruned trees. Which 
#is higher?

summary(tree.train) #error = 0.1688
summary(tree.prune) #error = 0.1775
#we can see the pruned tree has a slightly higher training error rate

#11. Compare the test error rates between the pruned and unpruned trees. Which is higher?

#pruned tree test error
ppred <- predict(tree.prune, newdata = OJ[test,], type = 'class')
table(predict = ppred, truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 129  16
#     MM  40  85
mean(ppred != OJ[test,]$Purchase) #0.2074074

#we can reuse the results from before for the unpruned tree test error
tree.test <- tree(Purchase~ ., data = OJ[test,])
summary(tree.test)
ypred <- predict(tree.test, newdata = OJ[test,], type = 'class')
table(predict = ypred, truth = OJ[test,]$Purchase)
#       truth
#predict  CH  MM
#     CH 159  16
#     MM  10  85
mean(ypred != OJ[test,]$Purchase) #test error rate = 0.0962963

#clearly here the unpruned is best with the test error rate 0.0962963
