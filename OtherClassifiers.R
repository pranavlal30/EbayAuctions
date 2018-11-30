library(data.table)
library(MASS)
library(caret)

EbayAuctions <- fread('Data/TrainingSet.csv')
TestData <- fread('Data/TestSet.csv')

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


#Ignoring a few columns that don't make sense

IgnoreCols <- c('Price', 'PricePercent', 'SellerItemAvg', 'AuctionHitCountAvgRatio', 'BestOffer')
EbayAuctions[, (IgnoreCols) := NULL]
TestData[, (IgnoreCols) := NULL]

testModel <- glm(QuantitySold ~ ., data = EbayAuctions, family = binomial(link = "logit"))

library(glmulti)
BestSubset <-glmulti(formula(testModel), data = EbayAuctions,
                     level = 1,               # No interaction considered
                     method = "h",            # Exhaustive approach
                     crit = "aic",            # AIC as criteria
                     confsetsize = 5,         # Keep 5 best models
                     plotty = F, report = F,  # No plot or interim reports
                     fitfunction = "glm",     # glm function
                     family = binomial(link = "logit"))  


##Random Forest
library(randomForest)

rfModel <- randomForest(QuantitySold ~ ., data = EbayAuctions, ntree = 10)
test_pred <- predict(rfModel, TestData, type = "response")
confusionMatrix(test_pred, TestData$QuantitySold)$byClass
library(foreach)
results <- c()
foreach(p =  1:10) %do%{
  tree <- p
  rfModel <- randomForest(QuantitySold ~ ., data = EbayAuctions, ntree = tree)
  test_pred <- predict(rfModel, TestData, type = "response")
  conf <- confusionMatrix(test_pred, TestData$QuantitySold)
  results <- rbind(results, conf$byClass)
}

library(ggplot2)
ggplot(data = as.data.frame(results), aes(x = seq(1,100,10), y = Sensitivity)) + 
  geom_line(data = as.data.frame(results), aes(y = Sensitivity, colour = "red")) + 
  geom_line(data = as.data.frame(results), aes(y = Specificity, colour = "green")) + 
  geom_line(data = as.data.frame(results), aes(y = F1, colour = "blue")) +
  scale_color_discrete(name = "Metric", labels = c("Sensitivity", "Specificity", "F1-Score")) +
  xlab("No. of Trees") + ylab("Metrics Value")
  title(main = "Comparing performance of different no. of trees")
  
##LDA

ldaModel <- lda(QuantitySold ~ ., data = EbayAuctions)

#Test Prediction
test_pred <- predict(ldaModel, TestData, type = "class")
confusionMatrix(as.factor(test_pred$class), as.factor(TestData$QuantitySold))

levels(EbayAuctions$QuantitySold)
results <- c()
for(p in seq(0.1,0.9,0.1)){
  ldaModel2 <- lda(QuantitySold ~ ., data = EbayAuctions, prior = c(p,(1-p)))
  test_pred <- predict(ldaModel2, TestData, type = "class")
  conf <- confusionMatrix(as.factor(test_pred$class), as.factor(TestData$QuantitySold))
  results <- rbind(results, conf$byClass)
}

plot(results[,1] ~ seq(0.1,0.9,0.1), type = "l", col = "red")
lines(results[,2] ~ seq(0.1,0.9,0.1), col = "blue")

library(ggplot2)
ggplot(data = as.data.frame(results), aes(x = seq(0.1,0.9,0.1), y = Sensitivity)) + 
  geom_line(data = as.data.frame(results), aes(y = Sensitivity, colour = "red")) + 
  geom_line(data = as.data.frame(results), aes(y = Specificity, colour = "green")) + 
  geom_line(data = as.data.frame(results), aes(y = F1, colour = "blue")) +
  geom_vline(xintercept = 0.182) +
  scale_color_discrete(name = "Metric", labels = c("Sensitivity", "Specificity", "F1-Score")) +
  xlab("Prior Value") + ylab("Metrics Value")

pOpt <- 0.22
ldaModel2 <- lda(QuantitySold ~ ., data = EbayAuctions, prior = c(pOpt,(1-pOpt)))
test_pred <- predict(ldaModel2, TestData, type = "class")
confusionMatrix(as.factor(test_pred$class), as.factor(TestData$QuantitySold))
