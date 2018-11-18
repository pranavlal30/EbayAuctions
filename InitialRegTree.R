library(ggplot2)
library(tidyr)
library(rpart)

features <- c('Price', 'Category', 'isHOF', 'SellerAuctionCount', 'SellerAuctionSaleCount', 
              'StartingBid', 'EndDay')

tree.df <- fread('Data/TrainingSet.csv', select = features,
                 colClasses = c('numeric', 'factor', 'factor',
                                'integer', 'integer', 'numeric', 'factor'))



# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for Kyphosis")