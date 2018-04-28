
library(mice)
library(VIM)
library(Amelia)
library(missForest)
library(lubridate)
library(caret)
library(zoo)
library(strptime)
library(date)
library(dplyr)
library(Boruta)
library(randomForest)
library(mlbench)
library(ggplot2)
library(GGally)
Train <- read.csv("train.csv",header = TRUE)
#Train
Test_data <- read.csv('test.csv',header = TRUE)


Test_data[Test_data == 'MB'] <- 'DT'



Train$revenue <- log (Train$revenue)

#plot(density(Train$revenue))
#plot(density(log(Train$revenue)))

set.seed(123)
boruta <- Boruta(revenue~., data = Train, doTrace = 2)
print(boruta)
plot(boruta, xlab = "", xaxt = "n")

final.boruta <- TentativeRoughFix(boruta)
print(final.boruta)

confirmed.var <- getSelectedAttributes(boruta, withTentative = F)
confirmed.var

Train$City.Group <- factor(Train$City.Group)
Train$Type <- factor(Train$Type)
Train$City<-factor(Train$City)
Train$Open.Date <- strptime(as.character(Train$Open.Date), "%m/%d/%Y")
Train$Open.Date <- wday(as.Date(Train$Open.Date,'%d-%m-%Y'), label=TRUE,abbr = FALSE)
Train$Open.Date <- factor(Train$Open.Date)
#Train


#index <- sample(1:nrow(Train),size = 0.8*nrow(Train))
#Train <- Train[index,]
#Train
#Test <- Train[-index,]
#Test
#summary(Train)
#Test <- Test[,-43]
#Test




#index <- sample(2, nrow(Train), replace = TRUE, prob=c(0.8, 0.2))
#index


revenue.rf <- randomForest(revenue ~ City.Group *P6 *P23 *P28*P21* P26  , data=Train,importance = T)
#revenue.pred <- predict(revenue.rf,Test)
#Train$revenue.pred <- revenue.pred
#Rmse <- RMSE(Test$revenue,Test$revenue.pred)
#Rmse


Prediction <- predict(revenue.rf,Test_data)
#Prediciton <- antilog.pred(results = Prediction, base = 10)
Test_data$Prediction <- exp(Prediction)
Test_data <- Test_data[,-c(2:42)]
write.csv(Test_data,"Testdata_kaggle.csv")

