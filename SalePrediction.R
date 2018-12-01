library(tidyr)

EbayAuctions <- fread('Data/TrainingSet.csv')
EbayAuctionsTest <- fread('Data/TestSet.csv')
head(EbayAuctions)
names(EbayAuctions)
summary(EbayAuctions)
View(EbayAuctions)
# 
# subsetNo <- nrow(EbayAuctions)*(0.2)
# EbayAuctions <- EbayAuctions[1:subsetNo,]
## Plotting to check the skewness of the target variable
d <- density(EbayAuctions$Price)
plot(d, main="Auction Price")
polygon(d, col="red", border="blue")

d2 <- density(log(EbayAuctions$Price))
plot(d2, main="Log Auction Price")
polygon(d, col="red", border="blue")

features <- c("AuctionMedianPrice", "Price", "AvgPrice", "ItemListedCount", "StartingBid",
             "AuctionHitCountAvgRatio", "SellerSaleAvgPriceRatio", "SellerItemAvg", "AuctionCount", "AuctionSaleCount", "SellerAvg")

EbayAuctions <- fread('Data/TrainingSet.csv', select = features)
EbayAuctionsTest <- fread('Data/TestSet.csv', select = features)
# EbayAuctions <- EbayAuctions[,features]
# EbayAuctionsTest <- EbayAuctionsTest[,features]



###old data 

EbayAuctions$LogPrice <- log(EbayAuctions$Price)
EbayAuctionsTest$LogPrice <- log(EbayAuctionsTest$Price)
EbayAuctions <- EbayAuctions[,-c(1,3,9,6,21)]
EbayAuctionsTest <- EbayAuctionsTest[,-c(1,3,9,6,21)]
#subsetNo <- nrow(EbayAuctions) * (0.5)


### Generating full linear regression model 
linear.model <- lm(LogPrice ~ ., data = EbayAuctions[,-c('Price')])
summary(linear.model)

predictions <- predict(linear.model, EbayAuctionsTest)

actualsPred <- data.frame(cbind(actuals=EbayAuctionsTest$LogPrice, predicteds=predictions))
correlation_accuracy <- cor(actualsPred)
correlation_accuracy

### Lasso Regression


predictions <- predict(linear.model.log, EbayAuctionsTest)

actualsPred <- data.frame(cbind(actuals=EbayAuctionsTest$LogPrice, predicteds=predictions))
correlation_accuracy <- cor(actualsPred)
correlation_accuracy

library(glmnet)
x <- model.matrix(LogPrice~.,EbayAuctions)[,-1]
testx <- model.matrix(LogPrice~.,EbayAuctionsTest)[,-1]
y <- as.matrix(EbayAuctions$LogPrice)
testy <- as.matrix(EbayAuctionsTest$LogPrice)
lambda <- 10^seq(10, -2, length = 100)



lan <- anova(linear.model.log)
n <- nrow(EbayAuctions)
len <- length(lan$Df)
p = len - 1
c = p + 1

SSPE <- lan$`Sum Sq`[len]
SSLF <- sum(lan$`Sum Sq`[1:p])

Fhat <- (SSLF/(c-p))/(SSPE/(n-c))
Fval <- qf(0.99,df1=(c-p),df2=(n-c))

lasso.mod <- glmnet(x , y, alpha=1,lambda = lambda)
plot(lasso.mod, xvar="lambda", label=TRUE)


pr.lasso = cv.glmnet(x,y,type.measure='mse', keep=TRUE, alpha=1)
lambda.lasso = pr.lasso$lambda.min
lambda.id <- which(pr.lasso$lambda == pr.lasso$lambda.min)
plot(pr.lasso)

### Forward Selection Model
library(MASS)
null.model <- glm(LogPrice ~ 1, data = EbayAuctions)
xVars <- colnames(EbayAuctions)[1:23]
modelForm <- as.formula(paste( "~", paste(xVars, collapse = '+ ')))

step.model.forward <- stepAIC(null.model, direction = "forward",
                              scope = modelForm)



### Backward Selection Model
modelForm <- as.formula(paste("LogPrice", "~", paste(xVars, collapse = '+ ')))
full.model <- glm(modelForm, data = EbayAuctions)

step.model.backward <- stepAIC(full.model, direction = "backward")
 
### FOWARD AND BACKWARD RESULT in same model by eliminating no variables

#### Checking the correlation matrix
Ebay <- EbayAuctions[,-c(8)]
cor(Ebay)

### For log(Price), high correlations exist with AuctionMedianPrice, AuctionSaleCount, 
## AuctionCount, ItemListedCount, AuctionHitCountAvgRatio, StartingBid, AvgPrice,
### HitCount, AuctionAvgHitCount

### checking to see if second order terms and interaction terms make a difference
## in the model performance.


### Generating full linear regression model with 2nd order + interaction terms
linear.model.2 <- lm(LogPrice ~ . + I(PricePercent*2) + I(HitCount*2) +
                     I(StartingBid*2) + I(SellerClosePercent*2) +
                     (PricePercent*StartingBid) + (PricePercent*HitCount)+
                     (StartingBid*SellerClosePercent) + (HitCount*StartingBid) +
                     (SellerClosePercent*PricePercent), data = EbayAuctions)
summary(linear.model.2)

predictions <- predict(linear.model.2, EbayAuctionsTest)

actualsPred <- data.frame(cbind(actuals=EbayAuctionsTest$LogPrice, predicteds=predictions))
correlation_accuracy <- cor(actualsPred)
correlation_accuracy

### Since no signifcant difference can be seen by adding second order terms and
### interaction terms, we use the simple first order multiple linear model

### Lack of fit test

an <- anova(linear.model)
n <- nrow(EbayAuctions)
c = n - an$Df[24]
p = 23

SSPE <- an$`Sum Sq`[24]
SSLF <- sum(an$`Sum Sq`[1:23])

Fhat <- (SSLF/(c-p))/(SSPE/(n-c))
Fval <- qf(0.99,df1=(c-p),df2=(n-c))


## Since the lack of fit test results in the alternate hypothesis being accepted
## i.e the model fit is not good. Trying to see if standardizing the model makes
## any difference

Ymean = mean(EbayAuctions$LogPrice)
X1mean = mean(EbayAuctions$PricePercent)
X2mean = mean(EbayAuctions$StartingBidPercent)
X3mean = mean(EbayAuctions$SellerClosePercent)
X4mean = mean(EbayAuctions$StartingBid)
X5mean = mean(EbayAuctions$AvgPrice)
X6mean = mean(EbayAuctions$HitCount)
X7mean = mean(EbayAuctions$AuctionAvgHitCount)
X8mean = mean(EbayAuctions$SellerSaleAvgPriceRatio)
X9mean = mean(EbayAuctions$SellerAvg)
X10mean = mean(EbayAuctions$SellerItemAvg)
X11mean = mean(EbayAuctions$AuctionHitCountAvgRatio)
X12mean = mean(EbayAuctions$BestOffer)
X13mean = mean(EbayAuctions$AuctionCount)
X14mean = mean(EbayAuctions$AuctionSaleCount)
X15mean = mean(EbayAuctions$SellerAuctionCount)
X16mean = mean(EbayAuctions$SellerAuctionSaleCount)
X17mean = mean(EbayAuctions$AuctionMedianPrice)

n = nrow(EbayAuctions)

sY = sqrt((sum((EbayAuctions$LogPrice - Ymean)^2))/(n-1))
sX1 = sqrt((sum((EbayAuctions$PricePercent - X1mean)^2))/(n-1))
sX2 = sqrt((sum((EbayAuctions$StartingBidPercent - X2mean)^2))/(n-1))
sX3 = sqrt((sum((EbayAuctions$SellerClosePercent - X3mean)^2))/(n-1))
sX4 = sqrt((sum((EbayAuctions$StartingBid - X4mean)^2))/(n-1))
sX5 = sqrt((sum((EbayAuctions$AvgPrice - X5mean)^2))/(n-1))
sX6 = sqrt((sum((EbayAuctions$HitCount - X6mean)^2))/(n-1))
sX7 = sqrt((sum((EbayAuctions$AuctionAvgHitCount - X7mean)^2))/(n-1))
sX8 = sqrt((sum((EbayAuctions$SellerSaleAvgPriceRatio - X8mean)^2))/(n-1))
sX9 = sqrt((sum((EbayAuctions$SellerAvg - X9mean)^2))/(n-1))
sX10 = sqrt((sum((EbayAuctions$SellerItemAvg - X10mean)^2))/(n-1))
sX11 = sqrt((sum((EbayAuctions$AuctionHitCountAvgRatio - X11mean)^2))/(n-1))
sX12 = sqrt((sum((EbayAuctions$BestOffer - X12mean)^2))/(n-1))
sX13 = sqrt((sum((EbayAuctions$AuctionCount - X13mean)^2))/(n-1))
sX14 = sqrt((sum((EbayAuctions$AuctionSaleCount - X14mean)^2))/(n-1))
sX15 = sqrt((sum((EbayAuctions$SellerAuctionCount - X15mean)^2))/(n-1))
sX16 = sqrt((sum((EbayAuctions$SellerAuctionSaleCount - X16mean)^2))/(n-1))
sX17 = sqrt((sum((EbayAuctions$AuctionMedianPrice - X17mean)^2))/(n-1))

Yhat = (1/sqrt(n-1))*((EbayAuctions$LogPrice-Ymean)/sY)
X1hat = (1/sqrt(n-1))*((EbayAuctions$PricePercent-X1mean)/sX1)
X2hat = (1/sqrt(n-1))*((EbayAuctions$StartingBidPercent-X2mean)/sX2)
X3hat = (1/sqrt(n-1))*((EbayAuctions$SellerClosePercent-X3mean)/sX3)
X4hat = (1/sqrt(n-1))*((EbayAuctions$StartingBid-X4mean)/sX4)
X5hat = (1/sqrt(n-1))*((EbayAuctions$AvgPrice-X5mean)/sX5)
X6hat = (1/sqrt(n-1))*((EbayAuctions$HitCount-X6mean)/sX6)
X7hat = (1/sqrt(n-1))*((EbayAuctions$AuctionAvgHitCount-X7mean)/sX7)
X8hat = (1/sqrt(n-1))*((EbayAuctions$SellerSaleAvgPriceRatio-X8mean)/sX8)
X9hat = (1/sqrt(n-1))*((EbayAuctions$SellerAvg-X9mean)/sX9)
X10hat = (1/sqrt(n-1))*((EbayAuctions$SellerItemAvg-X10mean)/sX10)
X11hat = (1/sqrt(n-1))*((EbayAuctions$AuctionHitCountAvgRatio-X11mean)/sX11)
X12hat = (1/sqrt(n-1))*((EbayAuctions$BestOffer-X12mean)/sX12)
X13hat = (1/sqrt(n-1))*((EbayAuctions$AuctionCount-X13mean)/sX13)
X14hat = (1/sqrt(n-1))*((EbayAuctions$AuctionSaleCount-X14mean)/sX14)
X15hat = (1/sqrt(n-1))*((EbayAuctions$SellerAuctionCount-X15mean)/sX15)
X16hat = (1/sqrt(n-1))*((EbayAuctions$SellerAuctionSaleCount-X16mean)/sX16)
X17hat = (1/sqrt(n-1))*((EbayAuctions$AuctionMedianPrice-X17mean)/sX17)

### Building the standardized regression model 
std.lm <- lm(LogPrice ~ X1hat+X2hat+X3hat+X4hat+
               X5hat+X6hat+X7hat+X8hat+
               X9hat+X10hat+X11hat+X12hat+
               X13hat+X14hat+X15hat+X16hat+X17hat, data = EbayAuctions)


summary(std.lm)

predictions <- predict(std.lm, EbayAuctionsTest)

actualsPred <- data.frame(cbind(actuals=EbayAuctionsTest$LogPrice, predicteds=predictions))
correlation_accuracy <- cor(actualsPred)
correlation_accuracy

### Results are worse than the original :-/









