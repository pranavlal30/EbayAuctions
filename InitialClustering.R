library(ggplot2)
library(tidyr)
library(rpart)
library(data.table)
library(rpart.plot)
library(dplyr)


features <- c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
"AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")

df <- fread('Data/TrainingSet.csv', select = features)
test <- fread('Data/TestSet.csv')


df$AvgPrice <- log(df$AvgPrice)
df$Price <- log(df$Price)
df$AuctionMedianPrice <- log(df$AuctionMedianPrice)

test$AvgPrice <- log(test$AvgPrice)
test$AuctionMedianPrice <- log(test$AuctionMedianPrice)



plot(dfsub$AvgPrice, dfsub$SellerAuctionSaleCount)

av <- dfsub2[, .(avg = mean(Price)), by='Category']
av <- av[order(avg),]

barplot(av$avg, av$Category)
barplot(log(av$avg), av$Category)

set.seed(123)
k.max <- 6
data <- df[,c('AvgPrice','AuctionHitCountAvgRatio')]

wss <- sapply(1:k.max, 
              function(k){kmeans(data, k,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# kmeans <- kmeans(dfsub[,c('AvgPrice','SellerAuctionSaleCount')], 2)
kmeans <- kmeans(df[,c('AvgPrice','AuctionHitCountAvgRatio')], 2)
# df.c <- as.data.table(cbind(dfsub, cluster = kmeans$cluster))
df.c <- as.data.table(cbind(df, cluster = kmeans$cluster))




par(mfrow=c(1,2))

ggplot(data = df.c[cluster == 1], 
       aes(x = StartingBid, y = AvgPrice, color = Price)) + geom_point()
ggplot(data = df.c[cluster == 2], 
       aes(x = StartingBid, y = AvgPrice, color = Price)) + geom_point()



full.fmla <- as.formula(Price ~ IsHOF + StartingBid 
                        + StartingBidPercent + ItemAuctionSellPercent +  SellerAuctionSaleCount
                        +AuctionCount + AuctionMedianPrice)

num.clust <- length(unique(df.c$cluster))

for(i in 1:num.clust){
  df.c[cluster == i]
  full <- lm(formula = full.fmla, data = df.c[cluster == i])
  empty <- lm(formula = Price~-1, data = df.c[cluster == i])
  assign(paste('backward', i, sep = '.'),step(full, trace = 0))
  assign(paste('forward', i, sep = '.'),step(empty, scope=list(lower=formula(empty),
                                                               upper=formula(full)),
                                             direction = 'forward', trace = 0))
  assign(paste('both', i, sep = '.'),step(empty, scope=list(lower=formula(empty),
                                                               upper=formula(full)),
                                             direction = 'both', trace = 0))
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

full.nocluster <- step(lm(formula = full.fmla, data = dfsub), trace =0)
# summary(step(lm(formula = full.fmla, data = df.c), trace =0))
summary(full.nocluster)
sum(full.nocluster$residuals^2/length(full.nocluster$residuals))


### Tests with a PCA transformation

coeffs1 <- names(both.1$coefficients)
coeffs1 <- c(coeffs1[2:length(coeffs1)])

dfsub.1 <- df.c[cluster == 1]
prin_comp <- as.data.table(prcomp(dfsub.1[,.(AuctionMedianPrice,AuctionCount,ItemAuctionSellPercent)], scale. = T))
prin_comp <- prcomp(dfsub.1[,-c('Price')])
biplot(prin_comp, scale = 0)
#proportion of variance explained
pr_var <- prin_comp$sdev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

comp <- as.data.table(prin_comp$x)
df.pca <- cbind(dfsub.1[,'Price'], comp[,c('PC1', 'PC2')])
reg.pca <- lm(formula = Price ~ ., data = df.pca)
summary(reg.pca)

sum(reg.pca$residuals^2/length(reg.pca$residuals))



# clusters2 <- apply(df2, 1, closest.cluster)


## Revert PCA 
# Xpca = prcomp(X)
# 
# nComp = 2
# Xhat = Xpca$x[,1:nComp] %*% t(Xpca$rotation[,1:nComp])
# Xhat = scale(Xhat, center = -mu, scale = FALSE)



##########################
##########################
##########################
##########################
### Useless 

library(miceadds)
library(multiwayvcov)



mod1 <- miceadds::lm.cluster( data=kw_with_cluster, formula=Price ~ IsHOF + StartingBid,
                              cluster="cluster" )
coef(mod1)
vcov(mod1)
summary(mod1)



# Normalize data
varscale <- c("Price","AuctionMedianPrice", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
              "AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")

df.c <- as.data.table(df.c %>% mutate_each_(funs(scale(.) %>% as.vector),
                                                  vars=features))

df.c$Price <- log(df.c$Price)


