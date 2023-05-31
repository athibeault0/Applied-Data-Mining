#Data Assignment Lesson 11
#Written by Arielle Thibeault, using provided code for data cleaning

install.packages("dendextend")
install.packages("readxl")
library(readxl)
library(ggplot2)
library(dendextend)
eretail = read_excel("Desktop/STAT 508/Online Retail.xlsx")
dim(eretail)
names(eretail)

eretail = eretail[eretail$Country != "Unspecified",] # remove 'unspecified' country
eretail = eretail[eretail$Quantity > 0,] # remove returns/cancellations

IDtab = table(eretail$Country, eretail$CustomerID) # crosstab country by customer ID
IDtab = apply(IDtab >0, 2, sum) # is any customer ID duplicated across countries?
duplicateIDs = names(IDtab[IDtab > 1]) # duplicate IDs to clean up
eretail = eretail[!is.element(eretail$CustomerID, duplicateIDs),]
rm(IDtab)

eretail$InvoiceMth = substr(eretail$InvoiceDate, 1, 7) # extract month of invoice
eretail = eretail[as.vector(eretail$InvoiceMth) != "2011-12",] # remove December 2011 as it only covers first week

eretail$Amount = eretail$Quantity * eretail$UnitPrice # compute amount per invoice item

eaggr = aggregate(Amount~Country+CustomerID, data=eretail, sum) # compute aggregate amount spent per customer
row.names(eaggr) = eaggr$CustomerID
eaggr = eaggr[,-2]
eaggr = cbind(eaggr, aggregate(InvoiceMth~CustomerID, data=eretail, min)[,-1]) # 1st month of customer interaction
names(eaggr)[3] = "FirstMth"
eaggr = cbind(eaggr, aggregate(InvoiceMth~CustomerID, data=eretail, max)[,-1]) # last month of cust. interaction
names(eaggr)[4] = "LastMth"

# relabel months and compute duration of customer interaction
eaggr$FirstMth = as.factor(eaggr$FirstMth)
eaggr$LastMth = as.factor(eaggr$LastMth)
levels(eaggr$FirstMth) = 1:12
levels(eaggr$LastMth) = 1:12
eaggr$Months = as.numeric(eaggr$LastMth) - as.numeric(eaggr$FirstMth) + 1

eaggr = cbind(eaggr, apply( table(eretail$CustomerID, eretail$InvoiceMth) , 1, sum ) )
names(eaggr)[6] = "Purchases"

# Some useful statistics (which you may or may not decide to use)
eaggr$Amount.per.Purchase = eaggr$Amount / eaggr$Purchases
eaggr$Purchases.per.Month = eaggr$Purchases / eaggr$Months
eaggr$Amount.per.Month = eaggr$Amount / eaggr$Months

eaggr[1:30,]

#Using cluster analysis, your aim is to identify whether this company's consumers 
#can be segmented meaningfully by the recency and duration of their interaction 
#with the company, the frequency of their purchases/orders, and the amount they 
#spent (amounts are in Sterling pounds). Further segmentation by country would 
#also be very helpful for marketing purposes.

eaggr['Country'][eaggr['Country'] == "United Kingdom"] <- 1
eaggr['Country'][eaggr['Country'] == "Iceland"] <- 2
eaggr['Country'][eaggr['Country'] == "Finland"] <- 3
eaggr['Country'][eaggr['Country'] == "Italy"] <- 4
eaggr['Country'][eaggr['Country'] == "Norway"] <- 5
eaggr['Country'][eaggr['Country'] == "Bahrain"] <- 6
eaggr['Country'][eaggr['Country'] == "Spain"] <- 7
eaggr['Country'][eaggr['Country'] == "Portugal"] <- 8
eaggr['Country'][eaggr['Country'] == "Switzerland"] <- 9
eaggr['Country'][eaggr['Country'] == "Austria"] <- 10
eaggr['Country'][eaggr['Country'] == "Cyprus"] <- 11
eaggr['Country'][eaggr['Country'] == "Belgium"] <- 12
eaggr['Country'][eaggr['Country'] == "Denmark"] <- 13
eaggr['Country'][eaggr['Country'] == "Australia"] <- 14
eaggr['Country'][eaggr['Country'] == "France"] <- 15
eaggr['Country'][eaggr['Country'] == "Germany"] <- 16
eaggr['Country'][eaggr['Country'] == "RSA"] <- 17
eaggr['Country'][eaggr['Country'] == "Sweden"] <- 18
eaggr['Country'][eaggr['Country'] == "Israel"] <- 19
eaggr['Country'][eaggr['Country'] == "Saudi Arabia"] <- 20
eaggr['Country'][eaggr['Country'] == "Poland"] <- 21
eaggr['Country'][eaggr['Country'] == "USA"] <- 22
eaggr['Country'][eaggr['Country'] == "Greece"] <- 23
eaggr['Country'][eaggr['Country'] == "United Arab Emirates"] <- 24
eaggr['Country'][eaggr['Country'] == "Singapore"] <- 25
eaggr['Country'][eaggr['Country'] == "Japan"] <- 26
eaggr['Country'][eaggr['Country'] == "Netherlands"] <- 27
eaggr['Country'][eaggr['Country'] == "Lebanon"] <- 28
eaggr['Country'][eaggr['Country'] == "Brazil"] <- 29
eaggr['Country'][eaggr['Country'] == "Czech Republic"] <- 30
eaggr['Country'][eaggr['Country'] == "EIRE"] <- 31
eaggr['Country'][eaggr['Country'] == "Channel Islands"] <- 32
eaggr['Country'][eaggr['Country'] == "European Community"] <- 33
eaggr['Country'][eaggr['Country'] == "Lithuania"] <- 34
eaggr['Country'][eaggr['Country'] == "Canada"] <- 35
eaggr['Country'][eaggr['Country'] == "Malta"] <- 36

#So here, we want the recency, duration, frequency, and amount
#recency = eaggr$LastMth
#duration = eaggr$Months
#frequency = eaggr$Purchases.per.Month
#amount = eaggr$Amount

y <- data.frame(eaggr$Country, eaggr$LastMth, eaggr$Months, eaggr$Purchases.per.Month, eaggr$Amount)
y.short <- data.frame(eaggr[1:30,]$Country, eaggr[1:30,]$LastMth, eaggr[1:30,]$Months, eaggr[1:30,]$Purchases.per.Month, eaggr[1:30,]$Amount)

pr.out <- prcomp(y, scale = TRUE)
summary(pr.out)
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var

PVE <- pr.var / sum(pr.var) * 100 # in percentage
PVE

plot(pr.out)
#it seems like 3 PCs/ clusters from the plot

##shortened PCA
pr.out.short <- prcomp(y.short, scale = TRUE)
summary(pr.out.short)
pr.out.short$sdev
pr.var.short <- pr.out.short$sdev^2
pr.var.short

PVE.short <- pr.var.short / sum(pr.var.short) * 100
PVE.short

plot(pr.out.short)
##

set.seed(17)
km.three <- kmeans(y, centers = 3, nstart = 25)
km.three$cluster

wss <- km.three$tot.withinss
bss <- km.three$betweenss
table(km.three$cluster)
# cluster 1    cluster 2    cluster 3 
#     6           31          4249 

par(mfrow = c(1, 2))
plot(y, col = (km.three$cluster + 1),
     main = "K-Means Clustering Results with K = 3", pch = 25, cex = 3)
#from the plot, we see that clustering seems much more viable for
#frequency and amount (lastMth and Months are correlated, which makes sense)

##shortened kmeans
set.seed(17)
km.three.short <- kmeans(y.short, centers = 3, nstart = 25)
km.three.short$cluster

wss <- km.three.short$tot.withinss
bss <- km.three.short$betweenss
table(km.three.short$cluster)
# cluster 1    cluster 2    cluster 3 
#     24           5            1 

par(mfrow = c(1, 2))
plot(y, col = (km.three.short$cluster + 1),
     main = "K-Means Clustering Results with K = 3", pch = 25, cex = 3)
##

res <- cbind(y, ClusterId = km.three$cluster)
res <- as.data.frame(res)
a <- ggplot(res, aes(x = ClusterId, y = eaggr.Purchases.per.Month, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2") 
a
b <- ggplot(res, aes(x = ClusterId, y = eaggr.Amount, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2")
b
c <- ggplot(res, aes(x = ClusterId, y = eaggr.LastMth, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2")
c
d <- ggplot(res, aes(x = ClusterId, y = eaggr.Months, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2")
d
e <- ggplot(res, aes(x = ClusterId, y = eaggr.Country, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2")
e

#Now we try hierarchical clustering through each linkage method
hc.complete <- hclust(dist(y), method = "complete")
hc.average <- hclust(dist(y), method = "average")
hc.single <- hclust(dist(y), method = "single")

##short
hc.complete.short <- hclust(dist(y.short), method = "complete")
hc.average.short <- hclust(dist(y.short), method = "average")
hc.single.short <- hclust(dist(y.short), method = "single")
##

par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)

##
plot(hc.complete.short, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average.short, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single.short, main = "Single Linkage", xlab = "", sub = "", cex = .9)
##

#how the data is clustered
cutree(hc.complete, 3)
cutree(hc.average, 3)
cutree(hc.single, 3)

#finally, some dendrograms
hc.comp <- as.dendrogram(hc.complete)
cd <- color_branches(hc.comp,k = 3)
plot(cd)

hc.sing <- as.dendrogram(hc.single)
cd <- color_branches(hc.sing,k = 3)
plot(cd)

hc.ave <- as.dendrogram(hc.average)
cd <- color_branches(hc.ave,k = 3)
plot(cd)
#this one appears to be the best

##short
hc.comp.short <- as.dendrogram(hc.complete.short)
cd <- color_branches(hc.comp.short,k = 3)
plot(cd)

hc.sing.short <- as.dendrogram(hc.single.short)
cd <- color_branches(hc.sing.short,k = 3)
plot(cd)

hc.ave.short <- as.dendrogram(hc.average.short)
cd <- color_branches(hc.ave.short,k = 3)
plot(cd)
