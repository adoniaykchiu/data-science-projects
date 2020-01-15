
library(MASS)
library(ca)
library(corrplot)

setwd("~/1. DePaul/Autumn 19'/DSC 424 Advanced Data Analysis/Project/Codes")
housing = read.csv("housingsample22.csv", header=T)
head(housing)
str(housing)

# Create the Contingency Table for Type and Method
caData = table(housing$Type, housing$sublevel)
mosaicplot(caData, main="Mosaic Plot", xlab="Type", ylab="Sublevel", shade=T)

# Compute the correspondence matrix 
P = caData / sum(caData) # frequency count
print(sum(caData))
round(P, 3)

# To get the chiSquared statistics for each, we need the row and column sums
rSum = rowSums(P) #relative frequency
rSum
cSum = colSums(P)
cSum

# Then we compute all the products as a new matrix
mu = as.matrix(rSum) %*% t(as.matrix(cSum)) # measure each varlue's independence
mu

# Finally we make a matrix of the deviations and sum them
tmp = (P - mu)^2 / mu
chiSquared = sum(caData) * sum(tmp)
chiSquared #singular value decomposition (SVD)

nrow(caData) #3
ncol(caData) #5

pchisq(chiSquared, (nrow(caData) - 1) * (ncol(caData) - 1), lower.tail=F)

# The ca library has a nice correspondence analysis function
c = ca(caData)
c$N
c$rowcoord

summary(c)
plot(c, map = "symbiplot")

###############################################
###########          LDA            ###########
###############################################

head(housing)
housing$Price <- round(log(housing$Price),2)
housing$Propertycount <- round(log(housing$Propertycount),2)
housing$Landsize[housing$Landsize != 0] <- round(log(housing$Landsize),2)
housing$BuildingArea[housing$BuildingArea != 0] <- round(log(housing$BuildingArea),2)

write.table(housing, "newHousingsample22.csv", sep=",", row.names=T)
head(housing)
str(housing)

newHousing = read.csv("newHousingsample22_rmobs.csv", header=T)
newHousing = newHousing[, -c(1, 13:19)]
head(newHousing)
summary(newHousing$Type)

plot(newHousing[1:5], pch=16, col=newHousing$Type)  # Not much separation
plot(newHousing[6:10], pch=16, col=newHousing$Type)  # Not much separation
plot(newHousing[11:15], pch=16, col=newHousing$Type)  # Not much separation

str(newHousing)
corr.matrix <- cor(newHousing[,-c(14,15)])
corrplot(corr.matrix, order="AOE")

# Initial LDA on train set
newHousingLDA = lda(Type ~ ., data=newHousing)
print(newHousingLDA)

print(newHousingLDA$scaling[order(newHousingLDA$scaling[, 1]), ])
print(newHousingLDA$scaling[order(newHousingLDA$scaling[, 2]), ])

#Predicting on train set
newHousingLDA.values = predict(newHousingLDA)
par(mar=c(3, 3, 3, 3))
# Separates group 3 from the others
ldahist(data=newHousingLDA.values$x[, 1], g=newHousing$Type)
# Separates group 3 from the others but a bit of confusion on this one!
ldahist(data=newHousingLDA.values$x[, 2], g=newHousing$Type)

newHousingLDA.values$x 
plot(newHousingLDA.values$x[, 1], newHousingLDA.values$x[, 2], col=newHousing$Type, pch=16)

# Compute a confusion matrix
# not a perfect fit
table(newHousingLDA.values$class, newHousing$Type)
setwd("~/1. DePaul/Autumn 19'/DSC 424 Advanced Data Analysis/Module 7/LDA-Examples")
source("Confusion.R")
confusion(newHousingLDA.values$class, newHousing$Type) #.8373
#diag(prop.table(ct))
#sum(diag(prop.table(ct)))

set.seed(7890)
s = sample(nrow(newHousing), nrow(newHousing) * .8)
newHousingTrain = newHousing[s, ]
newHousingTest = newHousing[-s, ]

# Build the model on the training set
newHousingLDAtrain1 = lda(Type~ Price, data=newHousingTrain)
newHousingLDAtrain1.values = predict(newHousingLDAtrain1)
confusion(newHousingLDAtrain1.values$class, newHousingTrain$Type) #.768

newHousingLDAtrain2 = lda(Type~ Rooms, data=newHousingTrain)
newHousingLDAtrain2.values = predict(newHousingLDAtrain2)
confusion(newHousingLDAtrain2.values$class, newHousingTrain$Type) #.7809

newHousingLDAtrain3 = lda(Type~ Distance, data=newHousingTrain)
newHousingLDAtrain3.values = predict(newHousingLDAtrain3)
confusion(newHousingLDAtrain3.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain4 = lda(Type~ Bedroom2, data=newHousingTrain)
newHousingLDAtrain4.values = predict(newHousingLDAtrain4)
confusion(newHousingLDAtrain4.values$class, newHousingTrain$Type) #.7784

newHousingLDAtrain5 = lda(Type~ Bathroom, data=newHousingTrain)
newHousingLDAtrain5.values = predict(newHousingLDAtrain5)
confusion(newHousingLDAtrain5.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain6 = lda(Type~ Car, data=newHousingTrain)
newHousingLDAtrain6.values = predict(newHousingLDAtrain6)
confusion(newHousingLDAtrain6.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain7 = lda(Type~ Landsize, data=newHousingTrain)
newHousingLDAtrain7.values = predict(newHousingLDAtrain7)
confusion(newHousingLDAtrain7.values$class, newHousingTrain$Type) #.8093

newHousingLDAtrain8 = lda(Type~ BuildingArea, data=newHousingTrain)
newHousingLDAtrain8.values = predict(newHousingLDAtrain8)
confusion(newHousingLDAtrain8.values$class, newHousingTrain$Type) #.7088

newHousingLDAtrain9 = lda(Type~ Propertycount, data=newHousingTrain)
newHousingLDAtrain9.values = predict(newHousingLDAtrain9)
confusion(newHousingLDAtrain9.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain10 = lda(Type~ Years, data=newHousingTrain)
newHousingLDAtrain10.values = predict(newHousingLDAtrain10)
confusion(newHousingLDAtrain10.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain11 = lda(Type~ sublevel, data=newHousingTrain)
newHousingLDAtrain11.values = predict(newHousingLDAtrain11)
confusion(newHousingLDAtrain11.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain12 = lda(Type~ VendorBid, data=newHousingTrain)
newHousingLDAtrain12.values = predict(newHousingLDAtrain12)
confusion(newHousingLDAtrain12.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain13 = lda(Type~ Auction, data=newHousingTrain)
newHousingLDAtrain13.values = predict(newHousingLDAtrain13)
confusion(newHousingLDAtrain13.values$class, newHousingTrain$Type) #.7152

newHousingLDAtrain14 = lda(Type~ Method, data=newHousingTrain)
newHousingLDAtrain14.values = predict(newHousingLDAtrain14)
confusion(newHousingLDAtrain14.values$class, newHousingTrain$Type) #.7139

newHousingLDAtrain15 = lda(Type~ Landsize+Rooms
                           +Price+Distance+Bathroom, data=newHousingTrain)
newHousingLDAtrain15
newHousingLDAtrain15.values = predict(newHousingLDAtrain15)
table(newHousingLDAtrain15.values$class, newHousingTrain$Type)
confusion(newHousingLDAtrain15.values$class, newHousingTrain$Type) #.826
newHousingLDAtrain15

# The "scaling" in the result is the "loadings" of the original variables
# and can help us interpret the result
print(newHousingLDAtrain15$scaling[order(newHousingLDAtrain15$scaling[, 1]), ])
print(newHousingLDAtrain15$scaling[order(newHousingLDAtrain15$scaling[, 2]), ])

ldahist(data=newHousingLDAtrain15.values$x[, 1], g=newHousingTrain$Type)
ldahist(data=newHousingLDAtrain15.values$x[, 2], g=newHousingTrain$Type)

plot(newHousingLDAtrain15.values$x[, 1], newHousingLDAtrain15.values$x[, 2], 
     col=newHousingTrain$Type, pch=16)

# Run on Test Set
newHousingLDAtest.values = predict(newHousingLDAtrain15, newHousingTest)
table(newHousingLDAtest.values$class, newHousingTest$Type)
confusion(newHousingLDAtest.values$class, newHousingTest$Type) #.8359
newHousingLDAtest.values

ldahist(data=newHousingLDAtest.values$x[, 1], g=newHousingTest$Type)
ldahist(data=newHousingLDAtest.values$x[, 2], g=newHousingTest$Type)
plot(newHousingLDAtest.values$x[, 1], newHousingLDAtest.values$x[, 2], 
     col=newHousingTest$Type, pch=16)

################################################
###########         PREDICTION      ############
################################################

#predictions
prediction <- predict(newHousingLDAtrain15, newHousingTest)$posterior
dis.score <- with(newHousingTest, Landsize+Rooms+Price+Distance+Bathroom)
cbind(prediction, dis.score)
