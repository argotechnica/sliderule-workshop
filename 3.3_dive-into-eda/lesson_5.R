# Write code to create a new data frame,
# called 'pf.fc_by_age_gender', that contains
# information on each age AND gender group.

# The data frame should contain the following variables:

#    mean_friend_count,
#    median_friend_count,
#    n (the number of users in each age and gender grouping)

# Here is an example of the structure of your data frame. Your
# data values will be different. Note that if you are grouping by
# more than one variable, you will probably need to call the
# ungroup() function. 

#   age gender mean_friend_count median_friend_count    n
# 1  13 female          247.2953                 150  207
# 2  13   male          184.2342                  61  265
# 3  14 female          329.1938                 245  834
# 4  14   male          157.1204                  88 1201

# See the Instructor Note for two hints.

# DO NOT DELETE THESE NEXT TWO LINES OF CODE
# ==============================================================
pf <- read.delim('/datasets/ud651/pseudo_facebook.tsv')
suppressMessages(library(dplyr))

# ENTER YOUR CODE BELOW THIS LINE.
# ==============================================================
suppressMessages(library(ggplot2))
suppressMessages(library(GGally))
suppressMessages(library(dplyr))

setwd("C:/Client/GitHub/sliderule-workshop/3.3_dive-into-eda")
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
  
#example from lesson 4
pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)

#new stuff for lesson 5... group by multiple
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age,gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(as.numeric(friend_count)),
            n = n()) %>%
  ungroup() %>%
  arrange(age)

names(pf.fc_by_age_gender)

ggplot(pf.fc_by_age_gender, aes(age,median_friend_count)) +
  geom_line(aes(color = gender))

#look at reshaping data
#library(reshape2) instead of using reshape2, use tidyr
library(tidyr)
library(dplyr)

pf.fc_by_age_gender.wide <- pf.fc_by_age_gender[c('age', 'gender', 'median_friend_count')] %>%
  filter(!is.na(gender)) %>%
  spread(gender, median_friend_count) %>%
  mutate(ratio = male / female)
head(pf.fc_by_age_gender.wide)

ggplot(pf.fc_by_age_gender.wide, aes(age,female / male)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.5)

#use cut to create a new numerical variable from an existing one, then plot it
pf$year_joined <- 2014-(pf$tenure/365.25)
pf$year_joined.bucket <- cut(pf$year_joined,
                             c(2004, 2009, 2011, 2012, 2014))

ggplot(subset(pf, !is.na(year_joined.bucket)), aes(age, friend_count)) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = median)

ggplot(subset(pf, !is.na(year_joined.bucket)), aes(age, friend_count)) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = mean) +
  geom_line(linetype = 2,
            stat = 'summary',
            fun.y= mean)

# look at friending rate - v1, I do it with a var
friending_rate <- subset(pf$friend_count/pf$tenure, pf$tenure >= 1)
median(friending_rate)
max(friending_rate)

# look at friending rate - v2, tut does it with "with"
with(subset(pf, tenure >=1), summary(friend_count/tenure))

ggplot(subset(pf, tenure>=1), aes(tenure, friendships_initiated/tenure)) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = mean)

ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_smooth(aes(color = year_joined.bucket))                                                    

# and now for something completely different... :)
suppressMessages(library(ggplot2))
suppressMessages(library(GGally))
suppressMessages(library(dplyr))

setwd("C:/Client/GitHub/sliderule-workshop/3.3_dive-into-eda")

yo <- read.csv('yogurt.csv')

yo$id <- factor(yo$id)
str(yo)
summary(yo)

ggplot(yo, aes(x=price)) + geom_histogram()

# use transform to create a summed column
yo <- transform(yo,all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)

#scatterplot the data
ggplot(yo, aes(time,price)) +
  geom_jitter(alpha = 1/4, shape = 21)

#use a set seed to sample and then plo the data
#to get a feel for 16 specific households
set.seed(4320)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x = time, y = price),
       data = subset(yo, id %in% sample.ids)) +
  facet_wrap( ~ id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)

#now use sampling and GGally to look at the pf dataset again
suppressMessages(library(GGally))

theme_set(theme_minimal(20))
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])
