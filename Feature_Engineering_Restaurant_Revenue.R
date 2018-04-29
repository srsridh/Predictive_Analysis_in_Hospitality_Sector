
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
Test_data

Test_data[Test_data == 'MB'] <- 'DT'

"""
Train$P1 <- sqrt(Train$P1)
Train$P6 <- sqrt(Train$P6)
Train$P7 <- sqrt(Train$P7)
Train$P8 <- sqrt(Train$P8)
Train$P9 <- sqrt(Train$P9)
Train$P10 <- sqrt(Train$P10)
Train$P11 <- sqrt(Train$P11)
Train$P12 <- sqrt(Train$P12)
Train$P14 <- sqrt(Train$P14)
Train$P15 <- sqrt(Train$P15)
Train$P16 <- sqrt(Train$P16)
Train$P17 <- sqrt(Train$P17)
Train$P18 <- sqrt(Train$P18)
Train$P19 <- sqrt(Train$P19)
Train$P20 <- sqrt(Train$P20)
Train$P21 <- sqrt(Train$P21)
Train$P23 <- sqrt(Train$P23)
Train$P24 <- sqrt(Train$P24)
Train$P25 <- sqrt(Train$P25)
Train$P26 <- sqrt(Train$P26)
Train$P27 <- sqrt(Train$P27)
Train$P28 <- sqrt(Train$P28)
Train$P30 <- sqrt(Train$P30)
Train$P31 <- sqrt(Train$P31)
Train$P32 <- sqrt(Train$P32)
Train$P34 <- sqrt(Train$P34)
Train$P35 <- sqrt(Train$P35)
Train$P36 <- sqrt(Train$P36)
"""

Train$revenue <- log (Train$revenue)

#plot(density(Train$revenue))
#plot(density(log(Train$revenue)))

"""
library(mice)

md.pattern(Train)
library(VIM)

aggr_plot <- aggr(Train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(Train,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)


Train <- complete(tempData,1)
Train
summary(Train)

"""

Train$City.Group <- factor(Train$City.Group)
Train$Type <- factor(Train$Type)
Train$City<-factor(Train$City)
Train$Open.Date <- strptime(as.character(Train$Open.Date), "%m/%d/%Y")
Train$Open.Date <- wday(as.Date(Train$Open.Date,'%d-%m-%Y'), label=TRUE,abbr = FALSE)
Train$Open.Date <- factor(Train$Open.Date)
#Train



index <- sample(1:nrow(Train),size = 0.7*nrow(Train))
Train <- Train[index,]
#Train
Test <- Train[-index,]
#Test
summary(Train)
#Test <- Test[,-43]
#Test




index <- sample(2, nrow(Train), replace = TRUE, prob=c(0.7, 0.3))
index

#for(i in 400:600) {
#set.seed(i)

revenue.rf <- randomForest(revenue ~ City.Group + P1 + P6 + P17 + P23 + P28, data=Train[index == 1,],importance = T)
#varImpPlot(revenue.rf)

"""
revenue.pred <- predict(revenue.rf,Test)
Test$revenue.pred <- revenue.pred
Rmse <- RMSE(Test$revenue,Test$revenue.pred)
print(c(i,Rmse))
#}
"""

revenue.pred <- predict(revenue.rf,Test_data)
Test_data$revenue.pred <- revenue.pred
Test_data <- Test_data[,-c(2:42)]

write.csv(Test_data,"Testdata_kaggle.csv")
