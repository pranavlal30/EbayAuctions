
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


