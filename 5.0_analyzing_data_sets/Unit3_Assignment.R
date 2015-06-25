setwd("C:\\Client\\Classes\\Data Science Workshop (Sliderule)\\5 Analyzing Datasets\\Analytics Edge, The (MITx)\\03 Logistic Regression")

library(caTools)
library(ROCR)
library(mice)

################################################################
# POPULARITY OF MUSIC RECORDS
################################################################
rm(list = ls())

songs <- read.csv("songs.csv")

# How many from 2010?
table(songs$year)

# How many from Michael Jackson?
nrow(subset(songs, artistname == "Michael Jackson"))

# Which Michael Jackson songs made it to Top 10?
songs$songtitle[songs$artistname == "Michael Jackson" & songs$Top10 == "1"]

# What values of "timesignature" are used in the dataset?
sort(unique(songs$timesignature))

# What value of "timesignature" is most common in dataset?
table(songs$timesignature)

# What song has the highest tempo?
which.max(songs$tempo)
songs$songtitle[6206]

# Split into train/test datasets
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year > 2009)

# Train a glm model, using a trick to remove non-numerical
# values... What is the AIC for it?
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# What is the correlation between "loudness" and "energy"?
cor(SongsTrain$loudness, SongsTrain$energy)

# It's high -- so create new models, each dropping one of the two
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# SongsLog3 looks to be the better of the two... Validate by predicting
# on the test set and measuring its accuracy at a threshold of 0.45
predictTrain = predict(SongsLog3, newdata = SongsTest, type="response")
table(SongsTest$Top10, predictTrain >= 0.45)
(309+19)/(309+5+40+19)

# To find out how this compares to the baseline model... What's the
# accuracy of the baseline? Assume this is the most common occurance,
# which is that it's NOT  a top 10 song
table(SongsTest$Top10)
314/(314+59)

# sensitivity of model3
19/(19+40)

# specificity of model3
309/(309+5)

################################################################
# PREDICTING PAROLE VIOLATORS
################################################################
rm(list = ls())

parole <- read.csv("parole.csv")

table(parole$violator)

str(parole)

# Convert unordered factors to factors
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)

# Split into training & test...
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Train a log reg model on the training set; dep = violator; use all
mod <- glm(violator ~ ., data = train, family="binomial")
summary(mod)

# What are the log(odds) that this person will is a violator?
-4.2411574 + 0.3869904*1 + 0.8867192*1 - 0.0001756*50 + 0.4433007*0 + 0.8349797*0 - 3.3967878*0 - 0.1238867*3 + 0.0802954*12 + 1.6119919*0 + 0.6837143*1 - 0.2781054*0 - 0.0117627*0

# What, therefore, are the odds?
exp(-1.700629)

# What, therefore, is the probability?
1/(1+exp(-(-1.700629)))

# Build predictions on test set
predictTest = predict(mod, newdata = test, type="response")
table(test$violator, predictTest >= 0.5)
12/(12+11)
167/(167+12)
(167+12)/(167+12+11+12)

table(test$violator)
179/(179+23)

library(ROCR)
ROCRpred = prediction(predictTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

################################################################
# PREDICTING LOAN REPAYMENT
################################################################
rm(list = ls())

loans <- read.csv("loans.csv")

# How many did not fully pay?
table(loans$not.fully.paid)
1533/(8045+1533)

# Which fields have missing observations?
summary(loans)

# How do we feel about those fields with missing data?
missing = subset(loans, is.na(log.annual.inc) |
                   is.na(days.with.cr.line) |
                   is.na(revol.util) |
                   is.na(inq.last.6mths) |
                   is.na(delinq.2yrs) |
                   is.na(pub.rec)) # Find rows with missing data...

nrow(missing) # Only 62 loans have missing data...

table(missing$not.fully.paid)
12/(50+12) # Similar rate of loans not paid back in missing...

# Ultimately, we still want to predict based on ALL rows,
# so let's impute values
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# Now randomly split into training and test...
set.seed(144) # Again...
split <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)

# Split up the data using subset
train <- subset(loans, split==TRUE)
test <- subset(loans, split==FALSE)

# Train a model...
payback <- glm(not.fully.paid ~ ., data = train, family=binomial)
summary(payback)

# What are the log odds of a given loan not being paid back with
# FICO score 700 vs 710? Logit(A) = 700, Logit(B) = 710...
# First, what's Logit(A) - Logit(B)?
(-9.406e-03*700)-(-9.406e-03*710)

# Difference in Odds, AKA O(A)/O(B)
exp(-9.406e-03*700)/exp(-9.406e-03*710)

# Predict probability of test set loans not being paid back in full
predictTest = predict(payback, newdata = test, type="response")

# Add predictions as a new variable on the test set
test$predicted.risk <- predictTest

# Confusion matrix with threshold of 0.5
table(test$not.fully.paid, test$predicted.risk > 0.5)

# Calculate accuracy of the predictions
# (true falses + true trues / all observations)
(2400+3)/(2400+3+13+457)

# Calculate accuracy of the baseline model
table(test$not.fully.paid)
2413/(2413+460)

# Use ROCR to compute the test set AUC
ROCRpred = prediction(predictTest, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Interest Rate is a big deal... make a new, bivariate model
# using only that. How does it look?
paybackIntOnly <- glm(not.fully.paid ~ int.rate, data = train, family=binomial)
predictTestIntOnly = predict(paybackIntOnly, newdata = test, type="response")
max(predictTestIntOnly)
table(predictTestIntOnly > 0.5)
ROCRpredIntOnly = prediction(predictTestIntOnly, test$not.fully.paid)
as.numeric(performance(ROCRpredIntOnly, "auc")@y.values)

# Start trying to analyze the profitability / value of a given investment
# using pert
10*exp(0.06*3)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
min(test$profit)*10

highInterest <- subset(test, int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)

# Determine the 100th smallest predicted probability of not paying in full
# by sorting the predicted risks in increasing order and selecting the
# 100th element of this sorted list
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans <- subset(highInterest, highInterest$predicted.risk <= cutoff)

sum(selectedLoans$profit)
sum(selectedLoans$not.fully.paid)
selectedLoans$profit
