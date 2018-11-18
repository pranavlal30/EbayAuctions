library(ggplot2)
library(tidyr)
library(rpart)
library(data.table)
library(rpart.plot)



features <- c('Price', 'Category', 'IsHOF', 'SellerAuctionCount', 'SellerAuctionSaleCount', 
              'StartingBid', 'EndDay')

#alternative - need to check
# c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
#   "AuctionHitCountAvgRatio", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")

tree.df <- fread('Data/TrainingSet.csv', select = features)
# train.data$Price=log(train.data$Price)

tree.df$IsHOF <- as.factor(tree.df$IsHOF)
tree.df$Category <- as.factor(tree.df$Category)
tree.df$EndDay <- as.factor(tree.df$EndDay)

# grow tree 
fit <- rpart(Price ~ ., method="anova", data=tree.df)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	


# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Ebay Auction price prediction")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
par(mfrow=c(1,1))
rpart.plot(fit,type=2,extra=1, cex=1)

# prune the tree 
pfit<- prune(fit,cp=0.012052) # from cptable   

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


