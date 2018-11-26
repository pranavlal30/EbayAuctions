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

df <- fread('Data/TrainingSet.csv', select = features)
dfsub <- df[1:20000]

plot(dfsub$AvgPrice, dfsub$SellerAuctionSaleCount)


#Check the "optimal" number of clusters with elbow curve
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 6
data <- dfsub[,c('AvgPrice','SellerAuctionSaleCount')]#scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kmeans3 <- kmeans(dfsub[,c('AvgPrice','SellerAuctionSaleCount')], 3)
print(kmeans3)

kw_with_cluster <- as.data.frame(cbind(dfsub, kmeans3$cluster))

library(miceadds)
library(multiwayvcov)

mod1 <- miceadds::lm.cluster( data=kw_with_cluster, formula=Price ~ IsHOF + StartingBid,
                              cluster="V2" )
coef(mod1)
vcov(mod1)
summary(mod1)


# c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
  # "AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")
#Forward selection
fullmod <- miceadds::lm.cluster( data=kw_with_cluster, 
                                 formula=Price ~ IsHOF + StartingBid 
                                 + StartingBidPercent + ItemAuctionSellPercent + AuctionHitCountAvgRatio 
                                 +AuctionCount + AuctionMedianPrice, cluster="V2" )
coef(fullmod)
vcov(fullmod)
summary(fullmod)
# nothing <- glm(formula = flushot~1, family = binomial(link="logit"), data = df1414)

backward = step(fullmod)

predict(fullmod)


pairs(dfsub)
