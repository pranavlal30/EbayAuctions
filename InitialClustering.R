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
df2 <- fread('Data/TrainingSet.csv')
dfsub <- df[sample(nrow(df), 10000)]
dfsub2 <- df2[sample(nrow(df), 1000)]

plot(dfsub$AvgPrice, dfsub$SellerAuctionSaleCount)

av <- dfsub2[, .(avg = mean(Price)), by='Category']
av <- av[order(avg),]

barplot(av$avg, av$Category)
barplot(log(av$avg), av$Category)
#Check the "optimal" number of clusters with elbow curve
set.seed(123)
# Compute and plot wss for k = 2 to k = 6.
k.max <- 6
data <- dfsub[,c('AvgPrice','SellerAuctionSaleCount')]#scaled_data
# data <- dfsub
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kmeans <- kmeans(dfsub[,c('AvgPrice','SellerAuctionSaleCount')], 2)
# kmeans <- kmeans(dfsub, 2)
print(kmeans3)
# dfsub_c <- as.data.table(cbind(dfsub, cluster = kmeans$cluster))
dfsub_c <- as.data.table(cbind(dfsub, cluster = kmeans$cluster))

##Plot the clusters
dfsub_c$cluster <- as.factor(dfsub_c$cluster)
ggplot(data = dfsub_c, aes(x = AvgPrice, y = SellerAuctionSaleCount))+
  geom_point(data = dfsub_c, aes(x = AvgPrice, y = SellerAuctionSaleCount, col = cluster))

ggplot(data = dfsub_c, aes(x = AvgPrice, y = SellerAuctionSaleCount))+
  geom_point(data = dfsub_c, aes(x = AvgPrice, y = SellerAuctionSaleCount, col = as.factor()))+
  scale_color_discrete(dfsub_c$cluster)

library(miceadds)
library(multiwayvcov)

mod1 <- miceadds::lm.cluster( data=kw_with_cluster, formula=Price ~ IsHOF + StartingBid,
                              cluster="cluster" )
coef(mod1)
vcov(mod1)
summary(mod1)



#Forward selection
fullmod <- miceadds::lm.cluster( data=kw_with_cluster, 
                                 formula=Price ~ IsHOF + StartingBid 
                                 + StartingBidPercent + ItemAuctionSellPercent + AuctionHitCountAvgRatio 
                                 +AuctionCount + AuctionMedianPrice, cluster="cluster" )
coef(fullmod)
vcov(fullmod)
summary(fullmod)
# nothing <- glm(formula = flushot~1, family = binomial(link="logit"), data = df1414)

backward = step(fullmod)

predict(fullmod)


pairs(dfsub)

full.fmla <- as.formula(Price ~ IsHOF + StartingBid 
+ StartingBidPercent + ItemAuctionSellPercent + AuctionHitCountAvgRatio 
+AuctionCount + AuctionMedianPrice)

num.clust <- length(unique(dfsub_c$cluster))
# Normalize data
varscale <- c("Price","AuctionMedianPrice", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
              "AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")

dfsub_c <- as.data.table(dfsub_c %>% mutate_each_(funs(scale(.) %>% as.vector),
                             vars=features))

dfsub_c$Price <- log(dfsub_c$Price)


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
