Lesson 4
========================================================

***

### Scatterplots and Perceived Audience Size
Notes:

***

### Scatterplots
Notes:

```{r Scatterplots}
setwd("C:/Client/GitHub/sliderule-workshop/3.3_dive-into-eda")
library(ggplot2)
library(GGally)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

qplot(age, friend_count, data = pf)

ggplot(aes(x = age, y = friend_count), data = na.omit(pf)) +
  geom_jitter(alpha = 1/20) +
  xlim(13, 90)

ggplot(aes(x = age, y = friend_count), data = na.omit(pf)) +
  geom_jitter(alpha = 1/20, position = position_jitter(h=0)) +
  xlim(13, 90) +
  coord_trans(y = "sqrt")



ggplot(aes(x = age, y = friendships_initiated), data = pf, binwidth = 1) + geom_jitter(alpha = 1/10, position=position_jitter(width=0.4, height=0))+ coord_cartesian(ylim = c(0, 2000), xlim=c(13,99)) + coord_trans(y = "sqrt") + geom_smooth()






library(dplyr)


age_groups <- group_by(pf, age)

pf.fc_by_age <- summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())



head(pf.fc_by_age)
pf.fc_by_age



pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)

pf.fc_by_age

ggplot(aes(x = age,y = friend_count_mean), data = pf.fc_by_age)) + geom_point()

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) + geom_line() 
+ geom_smooth()

ggplot(aes(age,friend_count), data=pf) +
  geom_jitter(alpha=.05,
             position=position_jitter(h=0),
             color = "orange") +
  coord_trans(y = "sqrt") +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = median, color = "blue") +
  geom_line(stat = 'summary', fun.y = quantile, prob = 0.1, linetype = 2, color = "blue") +
  geom_line(stat = 'summary', fun.y = quantile, prob = 0.9, linetype = 2, color = "blue") +
  coord_cartesian(ylim = c(0, 1000), xlim=c(13,99))





cor(pf$age,pf$friend_count)
with(subset(pf, age < 70), cor.test(age, friend_count))


ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +
  geom_point() +
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95)) +
  geom_smooth(method = 'lm', color = 'red')

cor.test(pf$www_likes_received,pf$likes_received)







install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell
names(Mitchell)

ggplot(aes(Month,Temp), data = Mitchell) +
  geom_point() +
  geom_line() +
  scale_x_discrete(breaks = seq(0,203,12))


cor.test(Mitchell$Month,Mitchell$Temp)




names(pf)
head(pf$dob_month)
head

pf$age_with_months <- (pf$age + ((12-pf$dob_month)/12))


age_months <- group_by(pf, age_with_months)
pf.fc_by_age_months <- summarise(age_months,
                                 friend_count_mean = mean(friend_count),
                                 friend_count_median = median(friend_count),
                                 n = n())
head(pf.fc_by_age_months)




library(dplyr)
age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())
pf.fc_by_age <- arrange(pf.fc_by_age, age)

head(pf.fc_by_age)

names(pf.fc_by_age)
names(pf)

ggplot(aes(x=friend_count_mean,))


