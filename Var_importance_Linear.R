# load the library
library(mlbench)
library(caret)


EbayAuctions <- fread('Data/TrainingSet.csv')
TestData <- fread('Data/TestSet.csv')

dropCols = c("EbayID", "PersonID", "SellerName", "ReturnsAccepted",
             "QuantitySold", "Category", "EndDay")

EbayAuctions[, (dropCols) := NULL]
TestData[, (dropCols) := NULL]

EbayAuctions$Price <- log(EbayAuctions$Price, base = exp(1))


cM <- cor(EbayAuctions)

highlyCorrelated <- findCorrelation(cM, cutoff=0.5)
print(highlyCorrelated)

# ensure results are repeatable
set.seed(7)
# prepare training scheme
# control <- trainControl(method="repeatedcv", number=3, repeats=3)
control <- trainControl(method="cv", number=3)
# # train the model
# model <- train(Price~., data=EbayAuctions, method='bridge', 
#                preProcess="scale", trControl=control, metric = 'RMSE')

# model <- train(Price~., data=EbayAuctions[1:10000], method='bridge', 
#                trControl=control, metric = 'RMSE')
model <- train(Price~., data=EbayAuctions, method='foba', 
               trControl=control, metric = 'RMSE')


# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance, cex= 1.2, cex.main = 1.5, 
     main = 'Var importance with random forest',cex.lab=1.5, cex.axis=2,
     cex.names=2)


# 
# test.data[["Interval"]] <- (round((test.data$Prediction)/5+1)*5)
# 
# # define the control using a random forest selection function
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
# results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))
