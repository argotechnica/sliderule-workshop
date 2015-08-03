###############################################################################
# Using a snapshot of data before, after, or during one of the ASAP conferences
# in year X, predict whether a given user will attend each specific conference
# in year X + 1.
#
# In this dataset, the following values in the "Attended..." factors mean:
#   0 = Attended neither ASAP 
#   1 = Attended first ASAP
#   2 = Attended second ASAP
#   3 = Attended both ASAPs
#
###############################################################################
# Set up environment
library(caTools)
library(dplyr)
library(party)
library(randomForest)
library(e1071)
setwd("C:\\Users\\E052457\\Desktop\\Conference Attendance Prediction")

###############################################################################
# PRE-WORK - Improving the source data set
###############################################################################
# Load data and convert "Attended..." to factors; these will be used as
# dependent variables in different models
rm(list = ls())
data <- read.csv("conference_behaviors.csv", colClasses="factor", fileEncoding = "UTF-8-BOM")

# Cluster on company size since random forest (see below) doesn't support
# features with > 53 levels, THEN, update SQL query to cluster by size before
# importing. Finally, re-import data before continuing.
data.cluster_me <- select(data,Company_Size)
data.km <- kmeans(data.cluster_me, 5)
data$Company_Size_Cluster <- data.km$cluster
table(data$Company_Size_Cluster)
table(data$Company_Size, data$Company_Size_Cluster)

###############################################################################
# MAIN SECTION
###############################################################################
# Update SQL, input data file, outside of R... Then re-read
rm(list = ls())
data <- read.csv("conference_behaviors.csv", colClasses="factor", fileEncoding = "UTF-8-BOM")

####################################
# Split into training and test sets
set.seed(1)
spl = sample.split(data$Attended_ASAP_This_Year, SplitRatio = 0.85)
train = subset(data, spl==TRUE)
test = subset(data, spl==FALSE)

####################################
# Random Forest model

# Train rf model on all factors with <= 53 levels
set.seed(1)
rf <- randomForest(Attended_ASAP_This_Year ~ . - Company_Code, data = train, ntree = 2000)
rf
varImpPlot(rf)

# Analyze random forest model; compare accuracy on training dataset
# against baseline model
round(importance(rf), 2)
rf
(1288+114+140+12)/nrow(train)
table(train$Attended_ASAP_This_Year)
1802/nrow(train)

# Evaluate on training set
rf.predict <- predict(rf, type = "class")
table(train$Attended_ASAP_This_Year, rf.predict)
(1742+123+144+14)/nrow(train)

# Get final conference attendance counts based on rf model
table(rf.predict)
ASAP1.rf <- sum(rf.predict == 1 | rf.predict == 3)
ASAP2.rf <- sum(rf.predict == 2 | rf.predict == 3)


##############
# SVM model

# Train model on all factors with > 1 levels
train_svm <- train[, sapply(train, nlevels) != 1]
svm <- svm(Attended_ASAP_This_Year ~ ., data = train_svm)

# Analyze random forest model; compare accuracy on training dataset
# against baseline model
svm.predict <- predict(svm)
table(train$Attended_ASAP_This_Year, svm.predict)
1802/nrow(train)

# Get final attendance counts based on the SVM model
table(svm.predict)
ASAP1.svm <- sum(svm.predict == 2 | svm.predict == 4)
ASAP2.svm <- sum(svm.predict == 3 | svm.predict == 4)

####################################
# Test out some ways of collecting final attendance #s
# in preparation for functionalizaing

ASAP.predictions <- data.frame(ASAP1 = integer(0), ASAP2 = integer(0))

ASAP.predictions <- bind_rows(ASAP.predictions, data_frame(ASAP1 = sum(rf.predict == 1 | rf.predict == 3),
                                       ASAP2 = sum(rf.predict == 2 | rf.predict == 3)))
ASAP.predictions <- bind_rows(ASAP.predictions, data_frame(ASAP1 = sum(svm.predict == 2 | svm.predict == 4),
                                                           ASAP2 = sum(svm.predict == 3 | svm.predict == 4)))
ASAP.predictions
summarise_each(ASAP.predictions, funs(mean))


####################################
# Functionalize the models

# Make functions that return final conference number predictions
rf_counts <- function()
{
  rf <- randomForest(Attended_ASAP_This_Year ~ . - Company_Code, data = train, ntree = 2500)
  rf.predict <- predict(rf, type = "class")
  ASAP.predictions.rf <- data_frame(Type = "randomForest",
                                    ASAP1 = sum(rf.predict == 1 | rf.predict == 3),
                                    ASAP2 = sum(rf.predict == 2 | rf.predict == 3))
}

svm_counts <- function()
{
  train_svm <- train[, sapply(train, nlevels) != 1]
  svm <- svm(Attended_ASAP_This_Year ~ ., data = train_svm)
  svm.predict <- predict(svm)
  ASAP.predictions.svm <- data_frame(Type = "svm",
                                     ASAP1 = sum(svm.predict == 2 | svm.predict == 4),
                                     ASAP2 = sum(svm.predict == 3 | svm.predict == 4))
}

# Run the randomForest predictions 100 times, SVM once (since it seems to produce
# the same numbers each time)
rm(ASAP.predictions)
ASAP.predictions <- data.frame(Type = character(0), ASAP1 = integer(0), ASAP2 = integer(0))

for (i in 1:100)
{
  ASAP.predictions <- bind_rows(ASAP.predictions, rf_counts())
}

ASAP.predictions <- bind_rows(ASAP.predictions, svm_counts())

# Remove the "Type" column because, after all, I don't want to analyze
# based on model type at the moment
ASAP.predictions <- select(ASAP.predictions, - Type)

# Get final average of all 101 models
final_guess <- summarise_each(ASAP.predictions, funs(mean))

# Look at what the training set says really happened
actual <- data_frame(ASAP1 = sum(train$Attended_ASAP_This_Year == 1 | train$Attended_ASAP_This_Year == 3),
  ASAP2 = sum(train$Attended_ASAP_This_Year == 2 | train$Attended_ASAP_This_Year == 3))

# Measure difference
actual-final_guess # Off by 48 and 64, respectively - both low
final_guess/actual # About 15% and 20% lower, respectively

####################################
# Make some conclusions

# What I've seen in the previous section is that SVM produces the same results
# each time when run multiple times. This might be worth further exploration.
# Meanwhile, randomForest produces OK results. But overall, the two models,
# even when running the rf case as many as 100 times, consistently under-
# predicts the true counts per the training data. This suggests that there's
# something that I'm not capturing in the data that might do a better job
# at explaining the dependent variable. Some ideas about what else to try,
# in terms of data:
#  * What score they gave the conference last year
#  * How much of what's offered at this year's conference have they already
#    taken
#  * Reinvestigate the quality of the source dataset with respect to knowledge
#    of users' levels and active statuses at given points in time, viz. the
#    model relies on knowing what level people are at in the past, but I still
#    may not be uncovering this adequately under the current data pull.
#
# In terms of approach:
#  * Try again to think about some kind of linear regression model
#    to incorporate into the ensemble
#  * Try making single CART trees with rpart to explore different decisions
#    to see if this uncovers anything interesting (viz., useful) for modeling
#  * Dig deeper in investigating ways to either add consideration of feature
#    levels to randomForest or find an alternate package that does this; create
#    a function that experiments with different settings, e.g. different
#    parameters included, different tree levels
#  * Further functionalize collection of stats like accuracy, etc., to try and
#    identify understanding of what the best model within each model type
#    might be.
#
# In particular, I think Stanford's free "Statistical Learning" class
# (http://statlearning.class.stanford.edu/) might be a good resource, as I've
# already found that it goes into some more detail about tuning random forest
# models. I think it would also support some of the conceptual underpinning
# a bit.