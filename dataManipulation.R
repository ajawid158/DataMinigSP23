
#data manipulation with dplyr package
library(dplyr)

#The dataset smoker
dfTips=read.csv(url('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv'))
names(dfTips)
head(dfTips)
dim(dfTips)
#filter
fCustomers=filter(dfTips, sex=="Female", smoker=='No')  #Females who do not smoke
head(fCustomers)
View(fCustomers)

#logical operators &, |, !
unique(dfTips$day)
weekend=filter(dfTips, day=='Sun'| day=='Sat')
head(weekend)
View(weekend)
#Females visited on the weekend

weekendFemale=filter(dfTips, (day=='Sun'|day=='Sat') & sex=='Female')
View(weekendFemale)

#Weekdays 
weekdayCs=filter(dfTips, day !='Sun'& day!='Sat')
View(weekdayCs)

#use the function %in%
unique(dfTips$size)

filter(dfTips, size %in% c(5,6,4))
filter(dfTips, day %in% c('Sun','Sat'))  

#for numerical >, <, ==
filter(dfTips, size==4)
filter(dfTips, tip>=6)

#select certain variables 
names(dfTips)
head(dfTips)
num_vars=select(dfTips, c(total_bill, tip, size))
head(num_vars)

select(dfTips, size, everything())

select(dfTips, tip:smoker)

select(dfTips, -(tip:smoker))

#+++++++++++++++++#
#data manipulation with dplyr package
#mutate
head(dfTips)
#total spending=total_bill+tip
head(mutate(dfTips, total_cost=total_bill+tip))

#+++++++++++++++++#
#data manipulation with dplyr package
#summarize
summarise(dfTips, mean(total_bill), mean(tip))
summarise(dfTips, mean(tip), median(tip), sd(tip))

genderdf=group_by(dfTips, sex)
summarise(genderdf, mean(total_bill), sd(total_bill))

#+++++++++++++++++#
#data manipulation with dplyr package
#rename columns
names(dfTips)
df1=rename(dfTips, bill=total_bill)
head(df1)
#+++++++++++++++++#
#data manipulation with dplyr package
#pull a column as a vector 
gender = pull(dfTips, sex)

head(gender)

#+++++++++++++++++#
#data manipulation with dplyr package
#sample_n
dim(dfTips)
dim(dfTips)
sampledfTips=sample_n(dfTips, 100)
head(sampledfTips)
View(sampledfTips)
