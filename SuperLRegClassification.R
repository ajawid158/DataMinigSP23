#+Supervised Learning: Classification

setwd("G:/My Drive/Spring23/ITC360/Datasets/DMSP23/DataMinigSP23")
x=read.csv("employee.csv")
names(x)
dim(x)

s=sample(nrow(x), 24)
x.train=x[s,]
x.test=x[-s,]
##Predict the MS of the employees let say using their Age, Gender, Salary
#Model 0 
m0=lm(Gymcode~Age, data=x.train)
m0_pr=predict(m0, x.test)
m0_pr

gym_pred=ifelse(m0_pr>0.5, 1, 0)   ##to create a dummy variable
gym_pred
x.test$Gymcode

View(x.test)
table(x.test$Gymcode, gym_pred)   #confussion matrix
##wrog classifications /6=50%

names(x)
###Model 1
m1=lm(Gymcode~Weight, data=x.train)
m1_pr=predict(m1, x.test)
gym_pred1=ifelse(m1_pr>0.5, 1, 0)
#ms_pred1
##plot(x.test$Age, m1_pr)
table(x.test$Gymcode, gym_pred1)
###Error rate=1/6 ~ 17%
x.test$Weight
###Suppose we choose model 1 how we use this for the new data
Weight.new=data.frame(Weight=c(60, 90, 85))
pr_new_data=predict(m1, Weight.new)
pr_new_data
cl_new_data=ifelse(pr_new_data>0.5, 1,0)
cl_new_data

x.fit=fitted(m1, x.train)
summary(x.fit)
plot(x.train$Weight, x.fit, col="red", pch=10)


