library(ggplot2)
library(tidyr)
library(rpart)
library(data.table)
library(rpart.plot)
library(dplyr)

# 
# features <- c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
# "AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")

features <- c('Price', 'StartingBid', 'AuctionMedianPrice', 'AuctionCount',
              'AuctionHitCountAvgRatio', 'SellerItemAvg', 'AuctionSaleCount', 
              'SellerAvg', 'ItemListedCount', 'AvgPrice', 'SellerSaleAvgPriceRatio')



df <- fread('Data/TrainingSet.csv', select = features)


test <- fread('Data/TestSet.csv')


df$Price <- log(df$Price)
# df$AvgPrice <- log(df$AvgPrice)
# df$AuctionMedianPrice <- log(df$AuctionMedianPrice)


# test$AvgPrice <- log(test$AvgPrice)
# test$AuctionMedianPrice <- log(test$AuctionMedianPrice)
# 
# plot(dfsub$AvgPrice, dfsub$SellerAuctionSaleCount)
# df2 <- fread('Data/TrainingSet.csv')
# 
# av <- df2[, .(avg = mean(Price)), by='Category']
# av <- av[order(avg),]
# 
# barplot(av$avg, av$Category, main = "Price frequency by category")
# barplot(log(av$avg), av$Category)

# set.seed(124)
# k.max <- 6
# data <- df[,c('AvgPrice','AuctionHitCountAvgRatio')]
# 
# wss <- sapply(1:k.max, 
#               function(k){kmeans(data, k,iter.max = 15 )$tot.withinss})
# wss
# plot(1:k.max, wss,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")

# kmeans <- kmeans(dfsub[,c('AvgPrice','SellerAuctionSaleCount')], 2)
kmeans <- kmeans(df[,c('AvgPrice','AuctionHitCountAvgRatio')], 2)
# df.c <- as.data.table(cbind(dfsub, cluster = kmeans$cluster))
df.c <- as.data.table(cbind(df, cluster = kmeans$cluster))



# full.fmla <- as.formula(Price ~  StartingBid 
#                         + StartingBidPercent + ItemAuctionSellPercent +  SellerAuctionSaleCount
#                         +AuctionCount + AuctionMedianPrice)

full.fmla <- as.formula(Price ~ .)

num.clust <- length(unique(df.c$cluster))

for(i in 1:num.clust){
  df.c[cluster == i]
  full <- lm(formula = full.fmla, data = df.c[cluster == i, -c('cluster')])
  empty <- lm(formula = Price~-1, data = df.c[cluster == i, -c('cluster')])
  # assign(paste('backward', i, sep = '.'),step(full, trace = 0))
  assign(paste('forward', i, sep = '.'),step(empty, scope=list(lower=formula(empty),
                                                               upper=formula(full)),
                                             direction = 'forward', trace = 0))
  # assign(paste('both', i, sep = '.'),step(empty, scope=list(lower=formula(empty),
                                                               # upper=formula(full)),
                                             # direction = 'both', trace = 0))
}

summary(forward.1)
summary(forward.2)

total <- step(lm(formula = full.fmla, data = df.c), trace = 0)


#function to classify a new object into a cluster
closest.cluster <- function(x, km) {
  cluster.dist <- apply(km$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}

test.clust <- apply(test[,c('AvgPrice', 'AuctionHitCountAvgRatio')], 
                    1, function(x) closest.cluster(x, kmeans))


test.c <- as.data.table(cbind(test, cluster = test.clust))

test_pred.1 <- predict(backward.1, test.c[cluster == 1])
test_pred.2 <- predict(backward.2, test.c[cluster == 2])
test_pred.total <- predict(total, test.c)

rmse.1 <- caret::RMSE(test.c[cluster == 1, Price],exp(test_pred.1))
rmse.2 <- caret::RMSE(test.c[cluster == 2, Price], exp(test_pred.2))
trmse <- caret::RMSE(test.c[, Price], exp(test_pred.total))



fitest <- function(model, n){
  lan <- anova(model)
  # n <- nrow(df)
  len <- length(lan$Df)
  # c = n - lan$Df[len]
  p = len -1
  c = p+1
  # p = len - 1
  
  SSPE <- lan$`Sum Sq`[len]
  SSLF <- sum(lan$`Sum Sq`[1:p])
  
  Fhat <- (SSLF/(c-p))/(SSPE/(n-c))
  Fval <- qf(0.99,df1=(c-p),df2=(n-c))
  print(paste('Fhat:',Fhat))
  print(paste('Fval:',Fval))
}
