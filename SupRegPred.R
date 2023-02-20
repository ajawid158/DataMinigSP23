#Chapter 1
#Supervised Learning methods
#1.Regression and Prediction############################

getwd()
setwd("G:/My Drive/Spring23/ITC360/Datasets/DMSP23/DataMinigSP23")
x=read.csv("employee.csv")
View(x)
names(x)
##Probem: we want to predict Employee's Spending using a vector of 
#relevant variables such as Salary, Gender, Working Hrs

#Step 1: check their correlation 
x.sub=data.frame(x$Spending, x$Salary, x$WrH, x$GenCode)
names(x.sub)=c("Spending", "Salary", "WH", "Gender")
View(x.sub)
x.cor=cor(x.sub)
x.cor

library(corrplot)
corrplot(x.cor, method = "pie", 
         type="lower")
##Step 2: split the dataset into trainig and test 
library(dplyr)
nrow(x)
0.8*30

s=sample(nrow(x), 24)
s
x.train=x[s,]
x.test=x[-s,]

##Step 3: Construct the prediction models using x.train 
##Step 3.1. Model 0
###Regression and Prediction
names(x.train)
lm0=lm(Spending~Salary, data = x.train)
y.pred=lm0$fitted.values

all.y=data.frame(x.train$Spending, y.pred)
View(all.y)

lm0.test=predict(lm0, x.test)
y.test=data.frame(x.test$Spending, lm0.test)
View(y.test)
###########################
SSE0=sum((x.test$Spending- lm0.test)^2)
SSE0

#Step 4:
lm1=lm(Spending~Salary+WrH, data = x.train)
lm1.test=predict(lm1, x.test)
SSE1=sum((x.test$Spending- lm1.test)^2)
SSE1

##Model 2

lm2=lm(Spending~Salary+WrH+GenCode, data = x.train)
lm2.test=predict(lm2, x.test)
SSE2=sum((x.test$Spending- lm2.test)^2)
SSE2

#Step 5: choose the best Model  Model 0 entailed the lowest error
##Make prediction with the model
View(x)
# a New employee comes with salary of 34 T
new.emp=data.frame(Salary = c(34, 50, 30))

names(new.emp)="Salary"
predict(lm0,new.emp)
##the new epm spends about 21600




