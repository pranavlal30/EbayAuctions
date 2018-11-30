library(data.table)
library(ggplot2)
library(tidyr)
library(corrplot)



EbayAuctions <- fread('Data/TrainingSet.csv')
head(EbayAuctions)
names(EbayAuctions)
summary(EbayAuctions)

nrow(EbayAuctions[IsHOF == 0,])

#Interesting plot - Difference between starting bid and final price
plot(ebay.subset$StartingBid, (ebay.subset$Price-ebay.subset$StartingBid))

#Same with only the ones that has been sold
#We don't want to be confused with the ones that have not been sold (Price = Start Price)
ebay.sold <- ebay.subset[QuantitySold == 1]
plot(ebay.sold$StartingBid, (ebay.sold$Price-ebay.sold$StartingBid))


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


corr <- cor(ebay.subset)
library(spatstat)
# corRaw[nrow(corRaw):1,]
plot(im(corr), main="Correlation Matrix Map")
dissimilarity <- 1 - corr
distance <- as.dist(dissimilarity)
plot(hclust(distance), 
     main="Dissimilarity = 1 - Correlation", xlab="")


library(cluster)
plot(agnes(distance))

library(Hmisc)
plot( varclus(as.matrix(ebay.subset), similarity="spearman") )

library(pvclust)
cluster.bootstrap <- pvclust(ebay.subset, nboot=100, 
                             method.dist="abscor", parallel = TRUE)
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)
