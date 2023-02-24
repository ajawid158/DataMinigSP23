#+Supervised Learning: Classification

setwd("G:/My Drive/Spring23/ITC360/Datasets/DMSP23/DataMinigSP23")
x=read.csv("employee.csv")
names(x)
dim(x)


#######----------------------------###

#Second classification-------------###

##Logistic classification----------###


names(x)
###Model 0
L.m0=glm(Gymcode~Age, family = "binomial", data = x.train)
#x.fit=fitted(L.m0, x.train)
#summary(m0.pr.train)
#plot(x.train$Age, x.fit, col="blue", pch=12)

###Make predication with the model 
m0_pr=predict(L.m0, x.test)
cl0_pr=ifelse(m0_pr>0.5, 1, 0)   #dummy variable
table(x.test$Gymcode, cl0_pr)   #confussion matrix

#error rate=0/6 = 0%

###Model 1
names(x)
L.m1=glm(Gymcode~Weight, family = "binomial", data = x.train)

#m0.pr.train=fitted(L.m0, x.train)
#summary(m0.pr.train)
#plot(x.train$Age, m0.pr.train, col="blue", pch=12)

###Make predication with the model 
m1_pr=predict(L.m1, x.test)
cl1_pr=ifelse(m1_pr>0.5, 1, 0)
table(x.test$Gymcode, cl1_pr)

##error rate=2/6=1/3=0.33=33%
## >> choose model 0 over 1
##New data 
Age.new=data.frame(Age=c(20, 26, 60))
cl_pred_new=predict(L.m0, Age.new)
cl_pred_new
class_new=ifelse(cl_pred_new>0.5, 1, 0)
class_new