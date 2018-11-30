library(tidyr)

EbayAuctions <- read.csv('Data/TrainingSubset.csv')
EbayAuctionsTest <- read.csv('Data/TestSubset.csv')
head(EbayAuctions)
names(EbayAuctions)
summary(EbayAuctions)
View(EbayAuctions)

## Plotting to check the skewness of the target variable
d <- density(EbayAuctions$Price)
plot(d, main="Auction Price")
polygon(d, col="red", border="blue")

d2 <- density(log(EbayAuctions$Price))
plot(d2, main="Log Auction Price")
polygon(d, col="red", border="blue")

EbayAuctions$LogPrice <- log(EbayAuctions$Price)
EbayAuctionsTest$LogPrice <- log(EbayAuctionsTest$Price)
EbayAuctions <- EbayAuctions[,-c(1,2,5,8,11)]
EbayAuctionsTest <- EbayAuctionsTest[,-c(1,2,5,8,11)]
#subsetNo <- nrow(EbayAuctions) * (0.5)


### Generating full linear regression model 
linear.model <- lm(LogPrice ~ ., data = EbayAuctions)
summary(linear.model)

predictions <- predict(linear.model, EbayAuctionsTest)

actualsPred <- data.frame(cbind(actuals=EbayAuctionsTest$LogPrice, predicteds=predictions))
correlation_accuracy <- cor(actualsPred)


### Lasso Regression

library(glmnet)
x <- model.matrix(LogPrice~.,EbayAuctions)[,-1]
testx <- model.matrix(LogPrice~.,EbayAuctionsTest)[,-1]
y <- as.matrix(EbayAuctions$LogPrice)
testy <- as.matrix(EbayAuctionsTest$LogPrice)
lambda <- 10^seq(10, -2, length = 100)


lasso.mod <- glmnet(x , y, alpha=1,lambda = lambda)
plot(lasso.mod, xvar="lambda", label=TRUE)

pr.lasso = cv.glmnet(xtrain,ytrain,type.measure='mse', keep=TRUE, alpha=1)
lambda.lasso = pr.lasso$lambda.min
lambda.id <- which(pr.lasso$lambda == pr.lasso$lambda.min)
plot(pr.lasso)

### Forward Selection Model
library(MASS)
null.model <- glm(LogPrice ~ 1, data = EbayAuctions)
xVars <- colnames(EbayAuctions)[1:28]
modelForm <- as.formula(paste( "~", paste(xVars, collapse = '+ ')))

step.model.forward <- stepAIC(null.model, direction = "forward",
                              scope = modelForm)



### Backward Selection Model
modelForm <- as.formula(paste("LogPrice", "~", paste(xVars, collapse = '+ ')))
full.model <- glm(modelForm, data = EbayAuctions)

step.model.backward <- stepAIC(full.model, direction = "backward")
 

