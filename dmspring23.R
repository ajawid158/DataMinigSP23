#++++++++++++++++Data Mining SP23++++++++++++#
#setwd("G:/My Drive/Spring23/ITC360/Datasets")
###Upload the dataset to R 
x=read.csv("datapreprocessing.csv")

View(x)
dim(x)

##name of attributes
names(x)


#Chapter 2: Data pre processing
#Inconsistent values

#Gender

class(x$Gender)
table(x$Gender)

#child is an inconsistent value for Gender
x$Gender=gsub("Child", NA, x$Gender)
table(x$Gender)
x$Gender

#Height  ##nature numerical 
summary(x$Height)
class(x$Height)

x$Height=gsub("164 cm ", 164, x$Height)
x$Height=gsub("186cm", 186, x$Height)
x$Height
class(x$Height)
x$Height=as.numeric(x$Height)
summary(x$Height)

#Weight
summary(x$Weight)
x$Weight=gsub("Male", NA, x$Weight)
x$Weight
class(x$Weight)

x$Weight=as.numeric(x$Weight)
summary(x$Weight)

##
x=read.csv("datapreprocessing.csv")
view(x)
x_height=x$Height
class(x_height)
x_height=as.character(x_height)
x_height
x_height=replace(x_height, x_height %in% c("186cm", "164 cm "), c(186, 164))
x_height
x_height=as.numeric(x_height)
summary(x_height)

#Chapter 2: Data pre processing
#Duplicated rows 
x=read.csv("datapreprocessing.csv")
view(x)
##package tidyverse
library(tidyverse)
dp_value=duplicated(x)
table(dp_value)


##Lets introduce some duplcated objects (rows)
dp_rows=x[c(1,2), ]
view(dp_rows)

y=rbind(x, dp_rows)
View(y)

dp_object=duplicated(y)
table(dp_object)

y[duplicated(y), ]   #this gives the duplicated rows with all details

###how to remove duplicated Rows/Objects
y_unique=unique(y)
y_dup=duplicated(y_unique)
table(y_dup)

#Chapter 2: Data pre processing
#Missing Values

#install.packages("mice")
library(mice)

x=read.csv("navalues.csv")
View(x)   ##missing values present

##spotting the missing values
is.na(x)
colSums(is.na(x))   #it gives the # of NAs in each col.
sum(is.na(x$Gender))  #for indiv col.
sum(is.na(x$Weight))

library(mice)
md.pattern(x)

##to take care of the missing values mitigation
y=x
View(y)

##omit the rows with missing values>>> the easiest way to deal NAs
y_new=na.omit(y)

View(y_new)
colSums(is.na(y_new))
md.pattern(y_new)
dim(y_new)
dim(y)
8/20   ##if you remove about 5% of your objects---you are still fine
##for our analysis  we work only with Gender and Height
y_gen_height=data.frame(y$Gender, y$Height)
names(y_gen_height)=c("Gender", "Height")
View(y_gen_height)

colSums(is.na(y_gen_height))
y_gen_height=na.omit(y_gen_height)
dim(y_gen_height)
colSums(is.na(y_gen_height))

###calculations in the presence of NAs
mean(x$Height, na.rm=T)
sum(is.na(x$Weight))
mean(x$Weight, na.rm = T)



####Very very very very important discussion here
##systematic missing values vs Random missing values 
View(x)

##test for random/syst missing values 
##Weight 
sum(is.na(x$Weight))

dum_na=is.na(x$Weight)  #binary variable recall from ITC 255
dum_na 

#Variable Gender: 

#Gender is QL  and dum_na is QL(binary)
#which test of association we use here>>>Chi-square test 
chisq.test(x$Gender, dum_na)  #No association, i.e. random missing values
##Height (ANT) and dum_na (Binary) >>>t-test 
t.test(x$Height~dum_na)   ##Hence no association, i.e. random missing values

###if the missing values are at random then we can impute them>>estimate them
##mice package

x.fill=mice(x)
x.cmpl=complete(x.fill)
colSums(is.na(x.cmpl))
View(x.cmpl)

write.csv(x.cmpl, file = "no_na_values.csv")

#Chapter 2: Data pre processing
#out-liers

x=read.csv("no_na_values.csv")
View(x)
names(x)
str(x)
###suppose we wanna work with the variable Age
summary(x$Age)
plot(density(x$Age), col="blue") #may be out-liers on the righthandside
##check for the outliers
boxplot(x$Age, horizontal = T)
boxplot.stats(x$Age)

###We remove the outliers only from Age
age_new=x$Age[x$Age<24]
boxplot(age_new, horizontal = T)

##you want to have a new dataset 
x_new=x[x$Age<24, ]
View(x_new)


###height
boxplot(x$Height, horizontal = T)
boxplot.stats(x$Height)

x_new_height=x[x$Height>58, ]
boxplot(x_new_height$Height, horizontal = T)

#Chapter 2: Data pre processing
#generating new attributes

names(x)
##Height in cm>>>generate a new var height in foot/feet
#1 cm=0.0328084 feet

height_in_feet=x$Height*0.0328084
height_in_feet
x=cbind(x, height_in_feet)
View(x)

##Decoding of the non-numerical var
##Gender
factor(x$Gender)
levels(x$Gender)
##0: Female, 1: Male
Gender_new=c()    ##empty vector 

for (i in 1:length(x$Gender)){
  if (x$Gender[i]=="Female"){
    Gender_new[i]=0
  } else {
    Gender_new[i]=1
  }
}


x=cbind(x, Gender_new)
View(x)
