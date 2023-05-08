dt=read.csv('ts.csv')
View(dt)
head(dt)

##Statistical analysis
summary(dt$Visits)

#Q1=14.25 On 25% of time we had 14.25 visits or less per day. 
#Q2=16 
#Mean=18.6 on average per day we had 18.6 visits

hist(dt$Visits, 
     ylab = 'Day', 
     xlab = 'Visits', 
     ylim = c(0,13))

View(dt)
#the number of days in which we had between [30, 35) visits is 2
summary(dt$Sales)

#mean=USD 74.4:::per day we had on average USD 74.5 of sale
#Q1=USD 57::::

plot(density(dt$Sales), 
     xlab = 'Daily Sale')
abline(v=60, col='red', lty=3)

mean(dt$Visits[dt$Date<16])
sd(dt$Visits[dt$Date<16])


mean(dt$Visits[dt$Date>15])
sd(dt$Visits[dt$Date>15])

plot(ts(dt$Visits), 
     xlab='Date', 
     col='blue', 
     ylab='Daily visit')
abline(h=mean(dt$Visits), lty=1, col='red')
abline(v=15, col='green')

names(dt)
#plots
library(ggplot2)
p=ggplot(data = dt, aes(x=Date))
p+geom_point(aes(y=Visits))+
  geom_line(aes(y=Visits, col='Visits'))+
  geom_point(aes(y=Sales))+
  geom_line(aes(y=Sales, col='Sales'))+
  labs(x='Time', y='Daily Sale/Visit')
  
#by adding the title
#Assignment:
#Define a time depending var: Y=Y(t) where Y is the daily tem in Kabul
#collect data for at least 30 (period)
#Do some statistical analysis 
#graph it using ggplot 2
#submit everything as a single pdf file. 







