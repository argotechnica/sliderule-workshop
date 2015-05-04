setwd("C:/Client/Classes/Data Science Workshop (Sliderule)/UD651 Data Analysis with R")

library(ggplot2)

names(pf)

pf <- read.csv("pseudo_facebook.tsv", sep = '\t')

names(pf)

qplot(x = dob_day, data = pf) + scale_x_discrete(breaks = 1:31) + facet_wrap(~dob_month, ncol = 3)

names(pf)

qplot(x = friend_count, data = pf, xlim = c(0, 1000))

summary(pf$friend_count)

qplot(x = friend_count, data = pf) + scale_x_continuous(limits = c(0,1000))

qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 25) + scale_x_continuous(limits = c(0,1000), breaks = seq(0, 1000, 50)) + facet_wrap(~gender)

names(pf)

by(pf$friend_count, pf$gender, summary)
96-74


qplot(x = tenure/365, data = pf, binwidth = 1, xlab = 'Number of years using Facebook', ylab = 'Number of users in sample') + scale_x_continuous(breaks = seq(1,10,1),lim=c(0,10))

names(pf)

qplot(x = age, data = subset(pf, age<330), binwidth = 10, xlab = 'Age of users in sample', ylab = 'Number of years using Facebook')
+ scale_x_continuous(breaks = seq(min(pf$age),max(pf$age),1),lim=c(min(pf$age),max(pf$age)))
range(pf$age)
min(pf$age)







qplot(x = friend_count, data = pf)

summary(pf$friend_count)

summary(log10(pf$friend_count + 1))
library(gridExtra)


?scales




p1 = qplot(x = friend_count, data = pf, xlab = 'Number of friends', ylab = 'Number of users on Facebook') + scale_x_continuous(limits = c(0,1000))
p2 = qplot(x = friend_count, data = pf, xlab = 'Number of friends (log10)', ylab = 'Number of users on Facebook') + scale_x_log10("friend_count" + 1)
p3 = qplot(x = friend_count, data = pf, xlab = 'Number of friends (sqrt)', ylab = 'Number of users on Facebook') + scale_x_sqrt("friend_count")
grid.arrange(p1, p3, ncol=1)




qplot(x = www_likes, y = ..count../sum(..count..), data = subset(pf, !is.na(gender)), xlab = 'Number of likes', ylab = 'Proportion of Users with that number of likes', geom = 'freqpoly', color = gender) + scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 500, 10))

qplot(x = www_likes, data = subset(pf, !is.na(gender)), geom = 'freqpoly', color = gender) + scale_x_continuous() + scale_x_log10()

mean(subset(pf$www_likes, gender = 'male'))

sum(subset(pf$www_likes, pf$gender == 'male'))
sum(subset(pf$www_likes, pf$gender == 'female'))

qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geom = 'boxplot') + coord_cartesian(ylim = c(0,250))

by(pf$friendships_initiated, pf$gender, summary)

names(pf)

qplot(x = gender, y = friendships_initiated, data = subset(pf, !is.na(gender)), geom = 'boxplot') + coord_cartesian(ylim = c(0, 150))

summary(pf$mobile_likes > 0)

pf$mobile_check_in <- NA

pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

length(pf$mobile_check_in == 1)
length(pf$mobile_check_in)



qplot(x = friend_count, data = pf, binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000),
                     breaks = seq(0, 1000, 50))
