library(ggplot2)
library(ggplot2)
library(tidyr)

EbayAuctions <- fread('Data/TrainingSet.csv')
head(EbayAuctions)
names(EbayAuctions)
summary(EbayAuctions)

library(corrplot)

#Bunch of useless boxplots

ebay.subset <- EbayAuctions[, c("EbayID", "SellerName", "PersonID", "EndDay", "ReturnsAccepted") := NULL]
EbayAuctions[, c("Price", "StartingBid", "AvgPrice", "AuctionMedianPrice", "SellerAuctionSaleCount")]
corrplot(cor(ebay.subset))

boxplot(ebay.subset)
ggplot(stack(EbayAuctions[, c("Price", "StartingBid", "AvgPrice", "AuctionMedianPrice", "SellerAuctionSaleCount")]), aes(x = ind, y = values)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 400))


ggplot(data = stack(ebay.subset), aes(x = ind, y = values)) +
  geom_boxplot()+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y = element_blank())+ 
  coord_cartesian(ylim = c(0, 100))

#Bunch of Histograms
ggplot(data = EbayAuctions, aes(x = EbayAuctions$Price)) +
  geom_histogram(binwidth = 10)

ggplot(data = EbayAuctions[Price < 50], aes(x = Price)) +
  geom_histogram(binwidth = 1, aes(fill = as.factor(QuantitySold)))


ggplot(data = EbayAuctions, aes(x = StartingBid)) +
  geom_histogram(aes(fill = as.factor(QuantitySold)))

ggplot(data = EbayAuctions[Price < 50], aes(x = StartingBid)) +
  geom_histogram(binwidth = 1, aes(fill = as.factor(QuantitySold)))


ggplot(data = EbayAuctions, aes(x = as.factor(EndDay))) +
  geom_histogram(stat = "count", aes(fill = as.factor(QuantitySold)))

#Feature Importance

library(randomForest)

fit <- randomForest(factor(QuantitySold) ~ ., data = ebay.subset[1:10000,]) 

library(caret)
varImpPlot(fit)

