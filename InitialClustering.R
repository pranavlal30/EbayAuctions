library(ggplot2)
library(tidyr)
library(rpart)
library(data.table)
library(rpart.plot)
library(dplyr)


### TODO : - ACP on those features to find 2 variables with the highest contribution to the variance
###        - Use those 2 variables for clustering
###        - plot ? Do the CART method & Clustering regression, compare results


# features <- c('Price', 'Category', 'IsHOF', 'SellerAuctionCount', 'SellerAuctionSaleCount',
               # 'StartingBid', 'EndDay')

#alternative - need to check
features <- c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid",
"AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")

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
dfsub_c <- as.data.table(cbind(df, cluster = kmeans$cluster))
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

for(i in 1:num.clust){
  dfsub_c[cluster == i]
  full <- lm(formula = full.fmla, data = dfsub_c[cluster == i])
  empty <- lm(formula = Price~-1, data = dfsub_c[cluster == i])
  assign(paste('backward', i, sep = '.'),step(full, trace = 1))
  assign(paste('forward', i, sep = '.'),step(empty, scope=list(lower=formula(empty),
                                                               upper=formula(full)),
                                             direction = 'forward', trace = 0))
  assign(paste('both', i, sep = '.'),step(empty, scope=list(lower=formula(empty),
                                                               upper=formula(full)),
                                             direction = 'both', trace = 0))
}

summary(forward.1)
summary(forward.2)
summary(forward.3)
summary(forward.4)

summary(both.1)
summary(both.2)

full.nocluster <- step(lm(formula = full.fmla, data = dfsub), trace =0)
# summary(step(lm(formula = full.fmla, data = dfsub_c), trace =0))
summary(full.nocluster)
sum(full.nocluster$residuals^2/length(full.nocluster$residuals))


### Tests with a PCA transformation

coeffs1 <- names(both.1$coefficients)
coeffs1 <- c(coeffs1[2:length(coeffs1)])

dfsub.1 <- dfsub_c[cluster == 1]
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


#function to classify a new object into a cluster
closest.cluster <- function(x) {
  cluster.dist <- apply(km$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
# clusters2 <- apply(df2, 1, closest.cluster)


## Revert PCA 
# Xpca = prcomp(X)
# 
# nComp = 2
# Xhat = Xpca$x[,1:nComp] %*% t(Xpca$rotation[,1:nComp])
# Xhat = scale(Xhat, center = -mu, scale = FALSE)

