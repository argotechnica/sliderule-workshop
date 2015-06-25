setwd("C:\\Client\\Classes\\Data Science Workshop (Sliderule)\\5 Analyzing Datasets\\Analytics Edge, The (MITx)\\02 Linear Regression")

################################################################
# CLIMATE CHANGE
################################################################
rm(list = ls())

climate.full <- read.csv("climate_change.csv")

str(climate.full)

climate.train <- subset(climate.full, Year <= 2006)

climate.test <- subset(climate.full, Year > 2006)

climate.model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate.train)

# explore model to see, based on coefficiency and significance of variables,
# what we should prune the model to

summary(climate.model)

cor(climate.train)

climate.model = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climate.train)

summary(climate.model)

# use step to do the same thing faster

climate.model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate.train)

climate.model.step = step(climate.model)

summary(climate.model.step)

# evaluate quality of stepped model on test dataset

climate.predictions <- predict(climate.model.step, climate.test)

SSE = sum((climate.predictions - climate.test$Temp)^2)

SST = sum((mean(climate.train$Temp) - climate.test$Temp)^2)

R_Squared = 1 - SSE/SST

R_Squared

################################################################
# READING TEST SCORES
################################################################
rm(list = ls())

pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")

str(pisaTrain)

# find average reading test scores of Ms & Fs
tapply(pisaTrain$readingScore, pisaTrain$male == 1, mean)

# find cols that have missing data (at least one NA value)
colSums(is.na(pisaTrain))[colSums(is.na(pisaTrain)) > 0]

# remove rows with NAs
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# Because the race variable takes on text values, it was loaded
# as a factor variable when we read in the dataset with read.csv
# () -- you can see this when you run str(pisaTrain) or str
# (pisaTest). However, by default R selects the first level
# alphabetically ("American Indian/Alaska Native") as the
# reference level of our factor instead of the most common level
# ("White"). Set the reference level of the factor
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)

# Sum of Squared Errors
SSE = sum(lmScore$residuals^2)

# Root mean squared error
RMSE = sqrt(SSE/nrow(pisaTrain))

RMSE

summary(lmScore)

lmPredict <- predict(lmScore, newdata = pisaTest)

summary(lmPredict)

SSE.test = sum((lmPredict - pisaTest$readingScore)^2)
SST.test = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2.test = 1 - SSE.test/SST.test
RMSE.test = sqrt(SSE.test/nrow(pisaTest))

SSE.test
SST.test
R2.test
RMSE.test

# "The baseline model predicts the average value
# of the dependent variable regardless of the value
# of the independent variable.
mean(pisaTrain$readingScore)

################################################################
# DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA
################################################################
rm(list = ls())

FluTrain <- read.csv("FluTrain.csv")

FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]

hist(FluTrain$ILI)

plot(FluTrain$Queries, log(FluTrain$ILI))

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)

summary(FluTrend1)

Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))

Correlation^2
log(1/Correlation)
exp(-0.5*Correlation)

FluTest <- read.csv("FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

PredTest1[FluTest$Week == "2012-03-11 - 2012-03-17"]

(FluTest$ILI[FluTest$Week == "2012-03-11 - 2012-03-17"]-PredTest1[FluTest$Week == "2012-03-11 - 2012-03-17"])/FluTest$ILI[FluTest$Week == "2012-03-11 - 2012-03-17"]

SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

# TRAINING A TIME SERIES MODEL
library(zoo) # helpful for time series models

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

# How many values are missing in the new ILILag2 variable?
sum(is.na(FluTrain$ILILag2))

plot(log(FluTrain$ILI), log(FluTrain$ILILag2))

# Train a linear regression model on the FluTrain dataset
# to predict the log of the ILI variable using the Queries
# variable as well as the log of the ILILag2 variable
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)

summary(FluTrend2)

sum(is.na(FluTest$ILILag2))

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)

FluTest$ILILag2 = coredata(ILILag2)

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
