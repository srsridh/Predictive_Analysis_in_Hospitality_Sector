
train =read.table("train.csv",sep = ",",header = TRUE)
train

library(ggplot2)
library(dplyr)


ggplot(train,aes(Type)) + geom_bar(aes(fill = City.Group), position ="dodge" )+ggtitle("Types of restaurant per city group")


