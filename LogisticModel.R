library(data.table)

EbayAuctions <- fread('Data/TrainingSet.csv')
head(EbayAuctions)

EbayAuctions[, Sold := Price > StartingBid]
EbayAuctions[, Sold := as.factor(as.integer(Sold))]
summary(EbayAuctions$Sold)
summary(as.factor(EbayAuctions$QuantitySold))


dropCols = c("EbayID", "PersonID", "SellerName")
EbayAuctions[, (dropCols) := NULL]


head(EbayAuctions)


fit <- glm(QuantitySold ~ ., data = EbayAuctions, family = binomial(link = "logit"))
TestData <- fread('Data/TestSet.csv')
TestData[, (dropCols) := NULL]


predictions <- predict(fit, TestData, type = "response")
predictions <- ifelse(predictions > 0.1, 1, 0)
errorRate <- mean(predictions != TestData$QuantitySold)



library(caret)
confusionMatrix(as.factor(predictions), as.factor(TestData$QuantitySold))

head(fit$fitted.values)



