<<<<<<< HEAD
library(data.table)
=======
>>>>>>> e69be3dcd17774bb689cb3c45fb287c7c538f419
library(ggplot2)
library(tidyr)
library(corrplot)



EbayAuctions <- fread('Data/TrainingSet.csv')
head(EbayAuctions)
names(EbayAuctions)
summary(EbayAuctions)

nrow(EbayAuctions[IsHOF == 0,])

#Feature correlation


ebay.subset <- EbayAuctions[, c("EbayID", "SellerName", "PersonID", "EndDay", "ReturnsAccepted") := NULL]
corrplot(cor(ebay.subset))


#Bunch of useless boxplots

boxplot(ebay.subset)
ggplot(stack(EbayAuctions[, c("Price", "StartingBid", "AvgPrice", "AuctionMedianPrice", "SellerAuctionSaleCount")]), aes(x = ind, y = values)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 400))


ggplot(data = stack(ebay.subset[IsHOF == 1,]), aes(x = ind, y = values)) +
  geom_boxplot()+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y = element_blank())+ 
  coord_cartesian(ylim = c(0, 100))

ggplot(data = stack(ebay.subset[IsHOF == 1,]), aes(x = ind, y = values)) +
  geom_boxplot()+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y = element_blank())


#Bunch of Histograms
ggplot(data = EbayAuctions, aes(x = EbayAuctions$Price)) +
  geom_histogram(binwidth = 10)

g <- ggplot(data = EbayAuctions[Price < 50], aes(x = Price)) +
  geom_histogram(binwidth = 1, aes(fill = as.factor(QuantitySold))) +
  title(main = "Price Distribution")
ggsave(plot = g, filename = 'Results/price_distribution.png')


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

<<<<<<< HEAD
#

=======
#Scatterplots 

# pairs(~Price+StartingBid+SellerAuctionCount+HitCount,data=EbayAuctions, 
#       main="Simple Scatterplot Matrix")


# useless PCA plots 

subset <- EbayAuctions[,c('StartingBid', 'SellerAuctionCount', 'HitCount')]

ea.pca <- prcomp(susbet, center = TRUE,scale. = TRUE)
summary(ea.pca)
plot(ea.pca$x[,1:2], EbayAuctions$Price)
 
# library(devtools)
# install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(ea.pca)
>>>>>>> e69be3dcd17774bb689cb3c45fb287c7c538f419
