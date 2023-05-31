#Lesson 5 Assignment
#Written by Arielle Thibeault with excerpts from provided code

install.packages("GGally")
library(GGally)
install.packages("factoextra")
library(factoextra)
#First we will filter and clean the data to make it easier to
#use in PCA
SR <- read.table("~/Downloads/ABBREV.txt", header=F, row.names=1, sep="^", quote="~")
SR <- na.omit(SR) # remove rows with missing values
SR <- SR[row.names(SR) != "13352",] # remove "duplicate" entry
row.names(SR) <- SR[,1] # set more meaningful row names
SR <- SR[,-1]
names(SR) <- c("Water_(g)", "Energ_Kcal", "Protein_(g)", "Lipid_Tot_(g)", "Ash_(g)", "Carbohydrt_(g)", "Fiber_TD_(g)", "Sugar_Tot_(g)", "Calcium_(mg)", "Iron_(mg)", "Magnesium_(mg)", "Phosphorus_(mg)", "Potassium_(mg)", "Sodium_(mg)", "Zinc_(mg)", "Copper_(mg)", "Manganese_(mg)", "Selenium_(µg)", "Vit_C_(mg)", "Thiamin_(mg)", "Riboflavin_(mg)", "Niacin_(mg)", "Panto_Acid_(mg)", "Vit_B6_(mg)", "Folate_Tot_(µg)", "Folic_Acid_(µg)", "Food_Folate_(µg)", "Folate_DFE_(µg)", "Choline_Tot_(mg)", "Vit_B12_(µg)", "Vit_A_IU", "Vit_A_RAE", "Retinol_(µg)", "Alpha_Carot_(µg)", "Beta_Carot_(µg)", "Beta_Crypt_(µg)", "Lycopene_(µg)", "Lut+Zea_(µg)", "Vit_E_(mg)", "Vit_D_µg", "Vit_D_IU", "Vit_K_(µg)", "FA_Sat_(g)", "FA_Mono_(g)", "FA_Poly_(g)", "Cholestrl_(mg)", "GmWt_1", "GmWt_Desc1", "GmWt_2", "GmWt_Desc2", "Refuse_Pct")
SRp <- SR[,c(1:46)] # restrict to just the nutrient variables

dim(SR)
par(mfrow=c(2,4))
for (i in 1:46) {
  hist(SRp[,i], main = "", ylab = "Freq", xlab = names(SR)[i])
}
ggcorr(SRp, low = "springgreen4", mid = "white", high = "hotpink4") +
  ggtitle(label = "Correlation Plot")

#Perform PCA on the data in 'SRp'. Don't forget to center and scale all the 
#variables. How many principal components would you need to adequately represent 
#this data in lower dimension? Are the first few principal components capturing 
#any interpretable combinations of the nutrients?

pr.components <- prcomp(x = SRp, scale = TRUE, center = TRUE)
summary(pr.components)
biplot(pr.components)
biplot(pr.components, xlim=c(-0.01, 0.01), ylim=c(-0.01, 0.01))

pr.var <- pr.components$sdev^2
pve <- pr.var / sum(pr.var)
pve

plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot",
     ylim = c(0, 0.2), type = 'b')
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     main = "Cumulative PVE per Principal Component",
     ylim = c(0, 1), type = 'b')

res.var <- get_pca_var(pr.components)
res.var$coord          
res.var$contrib        
res.var$cos2 

res.ind <- get_pca_ind(pr.components)
res.ind$coord
res.ind$contrib
max(res.ind$contrib)
res.ind$cos2