library(ggplot2)
library(tidyr)
library(rpart)
library(data.table)
library(rpart.plot)



# features <- c('Price', 'Category', 'IsHOF', 'SellerAuctionCount', 'SellerAuctionSaleCount', 
#               'StartingBid', 'EndDay')

#alternative - need to check
features <- c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
              "AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")


tree.df <- fread('Data/TrainingSet.csv', select = features)
# train.data$Price=log(train.data$Price)

tree.df$IsHOF <- as.factor(tree.df$IsHOF)
# tree.df$Category <- as.factor(tree.df$Category)
# tree.df$EndDay <- as.factor(tree.df$EndDay)

# grow tree 
tree <- rpart(Price ~ ., method="anova", data=tree.df)

printcp(tree) # display the results 
plotcp(tree) # visualize cross-validation results 
summary(tree) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(tree) # visualize cross-validation results  	


# plot tree 
plot(tree, uniform=TRUE, 
     main="Classification Tree for Ebay Auction price prediction")
text(tree, use.n=TRUE, all=TRUE, cex=.8)
par(mfrow=c(1,1))
rpart.plot(tree,type=2,extra=1, cex=1)

# prune the tree 
pfit<- prune(tree,cp=0.012052) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)



# Conditional Inference Tree
# Tree growth is based on statistical stopping rules, so pruning should not be required.
# library(party)
# Don't run this one its laggy for now
# fit <- ctree(Price ~ ., data=tree.df)
# plot(fit, main="Conditional Inference Tree for Ebay Auction price prediction")

#############"

test.raw.data  <- read.csv(file='Data/TestSubset.csv', sep=',', h=T)
test.data <- test.raw.data[c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent",
                             "StartingBidPercent", "StartingBid", "AuctionHitCountAvgRatio", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")]
test.data$Price=log(test.data$Price)

# test.data$IsHOF <- as.factor(test.data$IsHOF)

# make predictions
predCart<-predict(tree, test.data, type = "vector")

# add fields to test.data
test.data[["SalePrice"]] <- exp(test.data$Price)
# test.data[["SalePrice"]] <- test.data$Price
test.data[["Prediction"]] <- exp(predCart)
# test.data[["Prediction"]] <- predCart
test.data[["Interval"]] <- (round((test.data$Prediction)/5+1)*5)

# calculate the RMSE, standard deviation, mean difference
sqrt(mean(test.data$SalePrice-test.data$Prediction)^2)
sd(test.data$Prediction)
sum(test.data$SalePrice-test.data$Prediction)/sum(test.data$SalePrice)


# create title for results plot
title <- paste('Auction Prediction Results - CART', sep='')
# create results plot
prediction.plot <- ggplot(test.data, aes(x=SalePrice, y=Prediction)) + geom_point() + geom_line() + ggtitle(title)+ stat_smooth()
print(prediction.plot)







#################################################
#################################################
#################################################

train.raw.data  <- read.csv(file='Data/TrainingSet.csv', sep=',', h=T)
# optimized set of features
train.data <- train.raw.data[c("Price", "AvgPrice")]
train.data$Price=log(train.data$Price)

tree<-rpart(Price ~.,data=train.data)

# draw decision tree
rpart.plot(tree,type=2,extra=1)


test.raw.data  <- read.csv(file='Data/TestSubset.csv', sep=',', h=T)
test.data <- test.raw.data[c("Price", "AvgPrice")]
test.data$Price=log(test.data$Price)

# make predictions
predCart<-predict(tree, test.data, type = "vector")

# add fields to test.data
test.data[["SalePrice"]] <- exp(test.data$Price)
test.data[["Prediction"]] <- exp(predCart)
test.data[["AvgPrice"]] <- test.raw.data$AvgPrice

# calculate the RMSE, standard deviation, mean difference
sqrt(mean(test.data$SalePrice-test.data$Prediction)^2)
#MSE
mean(test.data$SalePrice-test.data$Prediction)^2
sd(test.data$Prediction)
sum(test.data$SalePrice-test.data$Prediction)/sum(test.data$SalePrice)


# create title for results plot
title <- paste('Auction Prediction Results - CART', sep='')
# create results plot
prediction.plot <- ggplot(test.data, aes(x=SalePrice, y=Prediction)) + geom_point() + ggtitle(title)+ stat_smooth() #+ scale_y_continuous(limits = c(00, 125)) # + scale_x_continuous(limits = c(00, 325))
print(prediction.plot)


# 
# prediction.plot <- ggplot(test.data, aes(x=SalePrice, y=Prediction)) + geom_point() + geom_line() + ggtitle(title)+ stat_smooth() #+ scale_y_continuous(limits = c(00, 125)) # + scale_x_continuous(limits = c(00, 325))
# print(prediction.plot)
