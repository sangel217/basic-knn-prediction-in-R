setwd("~/Desktop/011422PA in R")
bank.df<-read.csv("UniversalBank.csv")
bank.df<-bank.df[,-c(1,5)]
bank.df<-bank.df[,c(1:7,9:12,8)]
library(FNN)
library(caret)#already installed packages
#set random seed
set.seed(12345)
#partitioning into training(60%) and validation(40%) sets
#validation
train.index<-sample(row.names(bank.df), 0.6*dim(bank.df)[1])
valid.index<-setdiff(row.names(bank.df),train.index)
#all columns collected with training row ID into training set:
train.df<-bank.df[train.index, ]
valid.df<-bank.df[valid.index, ]
train.norm.df<-train.df
valid.norm.df<-valid.df
bank.norm.df<-bank.df
#normalize all variables
norm.values<-preProcess(train.df[,1:11], method=c("center","scale"))
train.norm.df[,1:11]<-predict(norm.values, train.df[,1:11])
valid.norm.df[,1:11]<-predict(norm.values,valid.df[,1:11])
bank.norm.df[,1:11]<-predict(norm.values,bank.df[,1:11])
new.df<-data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education = 2, Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1)
new.norm.df<-predict(norm.values,new.df)
#create data frame with two columns: k and accurracy
accuracy.df<-data.frame(k=seq(1, 19, 2), accuracy=rep(0,10))
for (i in 1:10) {
  +     knn.pred<-knn(train.norm.df[,1:11], valid.norm.df[,1:11], cl=train.norm.df[,12],k=i)
  + accuracy.df[i,2]<-confusionMatrix(knn.pred,valid.norm.df[,12])$overall[1]
}
#got error "Error: `data` and `reference` should be factors with the same levels." but checked that they had the same rows of data so i checked manually k=9
nn<-knn(train.norm.df[,1:11], new.norm.df,cl=train.norm.df[,12], k=4)
nn
#[1] 0
#Therefore the new customer would be classified as a not accepting a loan
knn.pred.new<-knn(bank.norm.df[,1:11], new.norm.df,cl=bank.norm.df[,12], k=4)