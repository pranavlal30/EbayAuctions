library(ggplot2)
library(tidyr)
library(rpart)
library(data.table)
library(rpart.plot)
library(dplyr)


features <- c('Price', 'StartingBid', 'AuctionMedianPrice', 'AuctionCount',
              'AuctionHitCountAvgRatio', 'SellerItemAvg', 'AuctionSaleCount', 
              'SellerAvg', 'ItemListedCount', 'AvgPrice', 'SellerSaleAvgPriceRatio')


df <- fread('Data/TrainingSet.csv', select = features)

test <- fread('Data/TestSet.csv')


df$Price <- log(df$Price)
df$AvgPrice <- log(df$AvgPrice)
df$AuctionMedianPrice <- log(df$AuctionMedianPrice)

test$AvgPrice <- log(test$AvgPrice)
test$AuctionMedianPrice <- log(test$AuctionMedianPrice)

### Elbow plot
set.seed(124)
k.max <- 6
data <- df[,c('AvgPrice','AuctionHitCountAvgRatio')]

wss <- sapply(1:k.max,
              function(k){kmeans(data, k,iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#####
##### Clustering 
kmeans <- kmeans(df[,c('AvgPrice','AuctionHitCountAvgRatio')], 2)

df.c <- as.data.table(cbind(df, cluster = kmeans$cluster))

##Plot the clusters
df.c$cluster <- as.factor(df.c$cluster)
ggplot(data = df.c, aes(y = AvgPrice, x = AuctionHitCountAvgRatio))+
  geom_point(data = df.c, aes(y = AvgPrice, x = AuctionHitCountAvgRatio, col = cluster))


full.fmla <- as.formula(Price ~ .)

num.clust <- length(unique(df.c$cluster))
#Create forward models for each cluster
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

#summarize the results
summary(forward.1)
summary(forward.2)


empty <- lm(formula = Price~-1, data = df.c[,-c('cluster')])
total <- step(empty, scope=list(lower=Price ~ -1, upper=full.fmla),
              direction = 'forward', trace = 0)


#function to classify a new object into a cluster
closest.cluster <- function(x, km) {
  cluster.dist <- apply(km$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}

# Creating clusters for the test set
test.clust <- apply(test[,c('AvgPrice', 'AuctionHitCountAvgRatio')], 
                    1, function(x) closest.cluster(x, kmeans))


test.c <- as.data.table(cbind(test, cluster = test.clust))

test_pred.1 <- predict(forward.1, test.c[cluster == 1])
test_pred.2 <- predict(forward.2, test.c[cluster == 2])
test_pred.total <- predict(total, test.c)

rmse.1 <- caret::RMSE(log(test.c[cluster == 1, Price]),(test_pred.1))
rmse.2 <- caret::RMSE(log(test.c[cluster == 2, Price]), (test_pred.2))
trmse <- caret::RMSE(log(test.c[, Price]), exp(test_pred.total))

caret::RMSE(log(test.c[cluster == 1, Price]),(test_pred.1))
caret::RMSE(log(test.c[cluster == 2, Price]), (test_pred.2))
caret::RMSE(log(test.c[, Price]), (test_pred.total))

caret::RMSE((test.c[cluster == 1, Price]),exp(test_pred.1))
caret::RMSE((test.c[cluster == 2, Price]), exp(test_pred.2))
caret::RMSE((test.c[, Price]), exp(test_pred.total))

cust.rmse((test.c[cluster == 1, Price]),exp(test_pred.1))
cust.rmse((test.c[cluster == 2, Price]), exp(test_pred.2))
cust.rmse((test.c[, Price]), exp(test_pred.total))

cust.rmse <- function(actual, predicted){
  
  diff <- (actual - predicted)^2
  # print(length(diff))
  n <- length(diff)
  # print(length(which(diff < 50^2)))
  SSE <- sum(diff[which(diff < 50^2)])
  RMSE <- sqrt(1/n*SSE)
  print(RMSE)
}

