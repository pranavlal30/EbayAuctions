library(data.table)
library(MASS)
library(caret)

EbayAuctions <- fread('Data/TrainingSet.csv')
TestData <- fread('Data/TestSet.csv')

head(EbayAuctions)
names(EbayAuctions)

#Drop unnecessary columns

dropCols = c("EbayID", "PersonID", "SellerName", "ReturnsAccepted")

EbayAuctions[, (dropCols) := NULL]
TestData[, (dropCols) := NULL]

#Qualitative Variables
EbayAuctions[, QuantitySold := as.factor(QuantitySold)]
EbayAuctions[, Category := as.factor(Category)]
EbayAuctions[, EndDay := as.factor(EndDay)]
EbayAuctions[, IsHOF := as.factor(IsHOF)]
EbayAuctions[, ItemListedCount := as.factor(ItemListedCount)]

TestData[, QuantitySold := as.factor(QuantitySold)]
TestData[, Category := as.factor(Category)]
TestData[, EndDay := as.factor(EndDay)]
TestData[, IsHOF := as.factor(IsHOF)]
TestData[, ItemListedCount := as.factor(ItemListedCount)]

summary(EbayAuctions)


#Fit a full base model
fit <- glm(as.factor(QuantitySold) ~ ., data = EbayAuctions, family = binomial(link = "logit"))
predictions_base <- predict(fit, TestData, type = "response")
predictions_base <- ifelse(predictions_base > 0.5, 1, 0)
##errorRate
mean(predictions_base != TestData$QuantitySold)

confusionMatrix(as.factor(predictions_base), as.factor(TestData$QuantitySold))


#Ignoring a few columns that don't make sense

IgnoreCols <- c('Price', 'PricePercent', 'SellerItemAvg', 'AuctionHitCountAvgRatio', 'BestOffer')
EbayAuctions[, (IgnoreCols) := NULL]
TestData[, (IgnoreCols) := NULL]

####New Base model####

testModel <- glm(QuantitySold ~ ., data = EbayAuctions, family = binomial(link = "logit"))
#Train accuracy
threshold = 0.5
trainPredictions <- factor( ifelse(testModel$fitted.values > threshold, 1, 0))
confusionMatrix(trainPredictions, EbayAuctions$QuantitySold)

#Test Prediction
test_pred <- predict(testModel, TestData, type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0)
confusionMatrix(as.factor(test_pred), as.factor(TestData$QuantitySold))

#K-fold cross validation
ctrl <- trainControl(method = "cv",
                     number = 10)
kfoldFit <- train(as.factor(QuantitySold) ~ ., data = EbayAuctions, family = binomial(link = "logit"), trControl = ctrl)


#Backward step feature selection

backward <- stepAIC(testModel, direction = "backward")
backward$anova
backward$formula
plot(backward$anova$AIC, xlab = "Step", type = 'l')
threshold = 0.5
trainPredictions <- factor( ifelse(fit$fitted.values > threshold, 2, 1) )
conf1 <- confusionMatrix(trainPredictions, dfTrain[, V25])
backwardModel <- glm(backward$formula, data = EbayAuctions, family = binomial(link = "logit"))
back_pred <- predict(backwardModel, TestData, type = "response")
back_pred <- ifelse(back_pred > 0.5, 1, 0)
confusionMatrix(as.factor(back_pred), as.factor(TestData$QuantitySold))




#Forward Step Selection
emptyModel <- glm(as.factor(QuantitySold) ~ 1, data = EbayAuctions, family = binomial(link = "logit"))
summary(fit2)


forward2 <- stepAIC(emptyModel, direction = "forward", scope = list(upper = as.factor(QuantitySold) ~ StartingBidPercent + 
                                                                     SellerClosePercent + Category + 
                                                                     StartingBid + AvgPrice + 
                                                                     EndDay + HitCount + AuctionAvgHitCount +
                                                                     ItemAuctionSellPercent + 
                                                                     SellerSaleAvgPriceRatio + SellerAvg + ReturnsAccepted +
                                                                     AuctionAvgHitCount + IsHOF +
                                                                     ItemListedCount + AuctionCount + 
                                                                     AuctionSaleCount + SellerAuctionSaleCount + 
                                                                     AuctionMedianPrice))

forward2$anova
plot(forward2$anova$AIC, xlab = "Number of features", ylab = "AIC", type = 'l')

forwardModel <- glm(as.factor(QuantitySold) ~ HitCount + SellerClosePercent + StartingBid + 
                      SellerSaleAvgPriceRatio + StartingBidPercent + AvgPrice + 
                      SellerAuctionSaleCount + AuctionSaleCount + Category + AuctionCount + 
                      IsHOF + SellerAvg + ItemListedCount + EndDay + AuctionMedianPrice + 
                      AuctionAvgHitCount + ItemAuctionSellPercent, 
                    data = EbayAuctions, family = binomial(link = "logit"))
summary(forwardModel)
forward_pred <- predict(forwardModel, TestData, type = "response")
forward_pred <- ifelse(forward_pred > 0.5, 1, 0)
errorRate <- mean(forward_pred != TestData$QuantitySold)
conf <- confusionMatrix(as.factor(forward_pred), as.factor(TestData$QuantitySold))
conf
precision <- array(numeric())
for(i in seq(0,1,by = 0.05)){
  pred <- ifelse(forwardModel$fitted.values > i, 1, 0)
  conf <- confusionMatrix(as.factor(pred), EbayAuctions$QuantitySold)
  precision <- rbind(precision, conf$table[4]/(conf$table[4] + conf$table[3]))
}
plot(seq(0,1,by = 0.05), precision)

res <- residuals(forwardModel, "pearson")
plot(res ~ forwardModel$fitted.values)

plot(forwardModel$fitted.values[res < 1000 & res > -1000], res[res < 1000 & res > -1000])



#Scaled features
scaledData <- EbayAuctions
scaledData$SellerClosePercent <- scaledData$SellerClosePercent * 100
scaledData$StartingBidPercent <- scaledData$StartingBidPercent * 100
scaledData$ItemAuctionSellPercent <- scaledData$ItemAuctionSellPercent * 100


scaledModel <- glm(as.factor(QuantitySold) ~ HitCount + SellerClosePercent + StartingBid + 
                      SellerSaleAvgPriceRatio + StartingBidPercent + AvgPrice + 
                      SellerAuctionSaleCount + AuctionSaleCount + Category + AuctionCount + 
                      IsHOF + SellerAvg + ItemListedCount + EndDay + AuctionMedianPrice + 
                      AuctionAvgHitCount + ItemAuctionSellPercent, 
                    data = EbayAuctions, family = binomial(link = "logit"))
summary(scaledModel)
scaled_pred <- predict(scaledModel, TestData, type = "response")
scaled_pred <- ifelse(scaled_pred > 0.5, 1, 0)
errorRate <- mean(scaled_pred != TestData$QuantitySold)
conf <- confusionMatrix(as.factor(scaled_pred), as.factor(TestData$QuantitySold))
conf

library(corrplot)
corrplot(cor(EbayAuctions[,-7]))

#Plotting the best linear model:

emptyModel <- glm(QuantitySold ~ 1, data = EbayAuctions, family = binomial(link = "logit"))
fullModel <- glm(QuantitySold ~ ., data = EbayAuctions, family = binomial(link = "logit"))

full_pred <- predict(fullModel, TestData, type = "response")
full_pred <- ifelse(full_pred > 0.5, 1, 0)
confusionMatrix(as.factor(full_pred), as.factor(TestData$QuantitySold))
parameters <- data.frame(NoOfParameters = c(),
                         Accuracy = c(),
                         specificity = c(),
                         sensitivity = c())


for(x in 1:ncol(EbayAuctions)){
  stepmod <- step(emptyModel, scope=list(lower=formula(emptyModel), upper=formula(fullModel)),
                  direction="forward", data=EbayAuctions, steps=x, trace=F)
  step_predict <- predict(stepmod, TestData, type = "response")
  step_predict <- ifelse(step_predict > 0.5, 1, 0)
  conf <- confusionMatrix(as.factor(step_predict), as.factor(TestData$QuantitySold))
  t <- data.frame(NoOfParameter = x,
                  Accuracy = conf$overall[1],
                  sensitivity = conf$byClass[1],
                  specificity = conf$byClass[2])
  parameters <- rbind(parameters, t)
  #cv.err  <- cv.glm(data=final_model, glmfit=stepmod, K=nrow(final_model))$delta[1]
  if (x == 1){
    final_features <- stepmod
  }else{
    if (conf$byClass[1] > max(parameters$specificity)){ 
      final_features <- stepmod 
    }
  }
}

ggplot(data = parameters, aes(x = NoOfParameter, y = seq(0.5,1,length.out = nrow(parameters)))) +
  geom_line(data = parameters, aes(y = specificity, colour = specificity)) + 
  geom_line(data = parameters, aes(y = sensitivity, colour = sensitivity), col = "red") + 
  geom_line(data = parameters, aes(y = Accuracy, colour = Accuracy), col = "green2") +
  theme(legend.position="right") +
  theme_bw()

library(reshape2)
mod <-melt(parameters, id = "NoOfParameter")
mod

ggplot(data = mod) + 
  geom_line(aes(x = NoOfParameter, y = value, colour = variable)) +
  geom_hline(yintercept = max(parameters$sensitivity)) 

####Adding weights####

weights <- as.numeric(EbayAuctions$QuantitySold) * 2
summary(weights)

weightedModel <- glm(QuantitySold ~ ., data = EbayAuctions, family = binomial(link = "logit"), weights = weights)
#Train accuracy
threshold = 0.5
trainPredictions <- factor( ifelse(weightedModel$fitted.values > threshold, 1, 0))
confusionMatrix(trainPredictions, EbayAuctions$QuantitySold)

#Test Prediction
test_pred <- predict(weightedModel, TestData, type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0)
confusionMatrix(as.factor(test_pred), as.factor(TestData$QuantitySold))

weights2 <- rep(3, nrow(EbayAuctions))
for(i in 1:nrow(EbayAuctions)){
  if(EbayAuctions$QuantitySold[i] == 0){
    weights2[i] <- 1
  }
}
summary(weights2)
weightedModel2 <- glm(QuantitySold ~ ., data = EbayAuctions, family = binomial(link = "logit"), weights = weights2)
#Train accuracy
threshold = 0.5
trainPredictions <- factor( ifelse(weightedModel2$fitted.values > threshold, 1, 0))
confusionMatrix(trainPredictions, EbayAuctions$QuantitySold)

#Test Prediction
test_pred <- predict(weightedModel2, TestData, type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0)
confusionMatrix(as.factor(test_pred), as.factor(TestData$QuantitySold))


####Higher Order Terms####

formula(weightedModel2)

# newdat <- data.frame(StartingBidPercent = seq(min(EbayAuctions$StartingBidPercent), max(EbayAuctions$StartingBidPercent),len=nrow(EbayAuctions)))
# newdat$Y <- EbayAuctions$QuantitySold
# newdat$Y_logit = weightedModel2$fitted.values
# plot(Y ~ StartingBidPercent, data = newdat)
# lines(weightedModel2$fitted.values ~ EbayAuctions$StartingBidPercent, col="green4", lwd=2)
# lines(lowess(df$Y ~ df$X, f = 0.8), col='blue', lwd=2)
# legend(x = 400, y = 0.9, legend = c('logit', 'lowess'), lty = c(1,1), col = c('green4', 'blue'))


higherOrder <- glm(QuantitySold ~ . + poly(HitCount,2) + poly(SellerClosePercent,2), data = EbayAuctions, family = binomial(link = "logit"), weights = weights)
#Train accuracy
threshold = 0.5
trainPredictions <- factor( ifelse(higherOrder$fitted.values > threshold, 1, 0))
confusionMatrix(trainPredictions, EbayAuctions$QuantitySold)

#Test Prediction
test_pred <- predict(higherOrder, TestData, type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0)
confusionMatrix(as.factor(test_pred), as.factor(TestData$QuantitySold))

##Best subset
library(glmulti)
BestSubset <-glmulti(formula(testModel), data = EbayAuctions,
                       level = 1,               # No interaction considered
                       method = "h",            # Exhaustive approach
                       crit = "aic",            # AIC as criteria
                       confsetsize = 5,         # Keep 5 best models
                       plotty = F, report = F,  # No plot or interim reports
                       fitfunction = "glm",     # glm function
                       family = binomial(link = "logit"))  

##Diagnostics and tests on Weighted model 2:

r_O <- residuals(weightedModel2, type="response") # ordinary residual
r_P <- residuals(weightedModel2, type="pearson") # Pearson residual
X <- model.matrix(weightedModel2)
W <- diag(weightedModel2$weights)
W2 <- diag(sqrt(weightedModel2$weights))
H <- W2%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%W2
r_SP <- residuals(weightedModel2, type="pearson")/sqrt(1-diag(H))
r_D <- residuals(weightedModel2, type="deviance") # Deviance residual

plot(weightedModel2$fitted.values,r_O,main="ordinary residuals vs probabilities",
     xlab="Estimated Probability",ylab="Ordinary Residuals")
lines(lowess(weightedModel2$fitted.values,r_O, f = 0.8), col='red', lwd=2)

plot(weightedModel2$fitted.values,r_P,main="Pearson residuals vs probabilities",
     xlab="Estimated Probability",ylab="Pearson Residuals")
lines(lowess(weightedModel2$fitted.values,r_P, f = 0.8), col='red', lwd=2)

####MAJOR OUTLIERS IN ABOVE PLOT

#Restricting y limits to ignore outliers

plot(weightedModel2$fitted.values,r_P,main="Pearson residuals vs probabilities",
     xlab="Estimated Probability",ylab="Pearson Residuals", ylim = c(-5,5), xlim = c(0,1))
lines(lowess(weightedModel2$fitted.values,r_P, f = 0.8), col='red', lwd=2)

#Plot the pearson residuals
length(r_P[abs(r_P) > 100])
length(r_P[abs(r_P) > 5])
hist(r_P)
range(r_P)
hist(r_P[abs(r_P) < 5])

#Restricting y limits to ignore outliers. Ignoring only 2420 of total dataset.

plot(weightedModel2$fitted.values,r_P,main="Pearson residuals vs probabilities",
     xlab="Estimated Probability",ylab="Pearson Residuals", ylim = c(-5,5), xlim = c(0,1))
lines(lowess(weightedModel2$fitted.values,r_P, f = 0.8), col='red', lwd=2)

# plot(weightedModel2$fitted.values,r_SP,main="Studentized Pearson residuals vs probabilities",
#      xlab="Estimated Probability",ylab="Studentized Pearson Residuals")
# plot(weightedModel2$fitted.values,r_D,main="Deviance residuals vs probabilities",
#      xlab="Estimated Probability",ylab="Deviance Pearson Residuals")

# #Residuals vs fitted probabilities
# 
# 
# plot(fitted(weightedModel2), rstandard(weightedModel2),
#      xlab = "Fitted values")
# lines(lowess(fitted(weightedModel2), rstandard(weightedModel2), f = 0.8), col='red', lwd=2)
# 
# #Deviance residuals vs fitted probabilities
# 
# plot(fitted(weightedModel2), rstandard(weightedModel2, type = "deviance"),
#      xlab = "Fitted values",
#      ylab = "Deviance residuals")
# lines(lowess(fitted(weightedModel2), rstandard(weightedModel2, type = "deviance"), f = 0.8), col='red', lwd=2)

##Both above plots point to a good fit.

##Checking for overdispersion

summary(weightedModel2)$deviance / summary(weightedModel2)$df.residual


##Tests

##WALD Test

predictors <- c("StartingBidPercent", "SellerClosePercent", "Category", "StartingBid",
                "AvgPrice", "EndDay", "HitCount", "AuctionAvgHitCount", "ItemAuctionSellPercent",
                  "SellerSaleAvgPriceRatio", "SellerAvg", 'IsHOF', 'ItemListedCount' ,
                  "AuctionCount", "AuctionSaleCount", "SellerAuctionCount", "SellerAuctionSaleCount", "AuctionMedianPrice")

library(survey)
fcrit <- qf(0.99, 1, 258521)
for(predictor in predictors){
  waldTest <- regTermTest(weightedModel2, predictor, method = "Wald")
  if(waldTest$Ftest <= fcrit){
    print(paste(paste("Predictor", predictor, sep = " "), " can be removed from the model with an alpha value of 0.01", sep = " "))
  } else{
    print(paste(paste("Predictor", predictor, sep = " "), " can remain in the model with an alpha value of 0.01", sep = " "))
  }
}


