library(dplyr)
library(tidyverse)
library(caret)
library(Rcpp)
library(pROC)
set.seed(1234)
df <- read.csv('/Users/2t/Downloads/output/RiskyDealer/caseCL.csv')


df1 <- df %>%
  group_by(BuyerID) %>%
  summarise_at(vars(Mileage:JDPowersCatVAN), mean)

df2 <- df %>%
  group_by(BuyerID) %>%
  summarise(AVGReturned = mean(Returned, na.rm = TRUE),
            Risky = ifelse(AVGReturned>0.1,1,0))

df_summary <- reduce(list(df1, df2), 
                     left_join, by = "BuyerID")

drops <- c('BuyerPerLoc','NumBuyerDate','NumLocationDate','NumBuyerLocDate',
           'CompetativeSize','GlobalRank','AdjustedSalePrice','EffectivePrice',
           'NumVisitDate','NumReturendMonth','NumReturendLoc','NumPurchaseBuyerMonth',
           'NumPurchaseBuyer','NumPurchaseBuyerZ','AVGReturned')

case <- df_summary[,!names(df_summary) %in% drops]

case <- case %>%
  rename( AVGSalePrice = SalePrice,
         CarAge = Age)

write.csv(case,'/Users/2t/Downloads/output/case.csv')

na_count <- sapply(case, function(y) sum(length(which(is.na(y)))))
(na_county <- data.frame(na_count))

case2 <- case[complete.cases(case$Returned),]
splitIndex <- createDataPartition(case2$Risky, p = .50,
                                  list = FALSE,
                                  times = 1)

trainSplit <- case2[ splitIndex,!names(case2) %in% c("BuyerID","Returned")]
testSplit <- case2[-splitIndex,!names(case2) %in% c("BuyerID","Returned")]

ctrl <- trainControl(method = "cv", number = 5)

tbmodel <- train(Risky ~ ., data = trainSplit, method = "treebag",trControl = ctrl)
predictors <- names(trainSplit)[names(trainSplit) != 'Risky']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])
auc <- roc(testSplit$Risky, pred)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))





trainSplit <- case[ splitIndex,!names(case) %in% c("BuyerID","Returned")]
testSplit <- case[-splitIndex,!names(case) %in% c("BuyerID","Returned")]

ctrl <- trainControl(method = "cv", number = 5)

tbmodel <- train(Risky ~ ., data = trainSplit, method = "rf", trControl = ctrl)
predictors <- names(trainSplit)[names(trainSplit) != 'Risky']

pred <- predict(tbmodel$finalModel, testSplit[,predictors])

auc <- roc(testSplit$Risky, pred)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)
