setwd("C:\\Client\\Classes\\Data Science Workshop (Sliderule)\\5 Analyzing Datasets\\Analytics Edge, The (MITx)\\04 Trees")

library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)

#######################################################
# understanding why people vote
#######################################################
rm(list = ls())

# 1.1 Exploration and Logistic Regression
gerber <- read.csv("gerber.csv")
table(gerber$voting)
108696/nrow(gerber)

# 1.2 Exploration and Logistic Regression

# 1.2.1 First, here's how I did it
table(gerber$voting[gerber$civicduty == 1])
12021/(12021+26197)
table(gerber$voting[gerber$hawthorne == 1])
12316/(25888+12316)
table(gerber$voting[gerber$self == 1])
13191/(25027+13191)
table(gerber$voting[gerber$neighbors == 1])
14438/(23763+14438)

# 1.2.2 Here's how MITx did it
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# 1.3 Exploration and Logistic Regression
voting.glm <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
summary(voting.glm)

# 1.4 - Exploration and Logistic Regression
voting.glm.predict <- predict(voting.glm, type="response")
table(gerber$voting, voting.glm.predict > 0.3)
(134513+51966)/(134513+100875+56730+51966)

# 1.5 - Exploration and Logistic Regression
table(gerber$voting, voting.glm.predict > 0.5)
(235388)/(235388+108696)

# 1.6 - Exploration and Logistic Regression
table(gerber$voting)
235388/nrow(gerber)

# 2.1 - Trees
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# 2.2 - Trees
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# 2.4 - Trees
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

# 3.1 Interaction Terms
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.296638-0.34)

# 3.2 Interaction Terms
prp(CARTmodel5, digits = 5)
abs(0.290456-0.334176)
abs(0.302795-0.345818)
abs(0.04372-0.043023)

# 3.3 - Interaction Terms
LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(LogModelSex)

# 3.4 - Interaction Terms
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
abs(0.29046-0.2908065)

# 3.5 - Interaction Terms
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

# 3.6 - Interaction Terms
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.29046-0.2904558)

#######################################################
# Letter recognition
######################################################
rm(list = ls())

# 1.1 Predicting B or not B
# Load data, create factor for if the letter is B
letters <- read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

# Split into train and test sets, 50% each, on isB
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
letters.train = subset(letters, spl==TRUE)
letters.test = subset(letters, spl==FALSE)

# Calculate accuracy of base model on test set
table(letters.test$isB)
1175/(1175+383)

# 1.2 - Predicting B or not B
# Build a classification tree on training data to predict if B
CARTb = rpart(isB ~ . - letter, data=letters.train, method="class")

# Calculate accuracy of CARTb
letters.predict.test <- predict(CARTb, newdata = letters.test, type = "class")
table(letters.test$isB, letters.predict.test)
(1118 + 340)/nrow(letters.test)

# 1.3 - Predicting B or Not B
# Build a random forest model
set.seed(1000)
rf.B <- randomForest(isB ~ . - letter, data=letters.train)

# Calculate accuracy of rf.B
letters.predict.test <- predict(rf.B, newdata = letters.test, type = "class")
table(letters.test$isB, letters.predict.test)
(1165+374)/nrow(letters.test)

# 2.1 - Predicting the letters A, B, P, R
# Convert "letter" to a factor because we want to do multiclass
# classification
letters$letter = as.factor( letters$letter )

# Split into train and test sets again, 50% each, on isB
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
letters.train = subset(letters, spl==TRUE)
letters.test = subset(letters, spl==FALSE)

# Calculate accuracy of base model on new test set
table(letters.test$letter)
401/nrow(letters.test)

# 2.2 - Predicting the letters A, B, P, R
# Build a classification tree on training data to predict letter
CARTletter = rpart(letter ~ . - isB, data=letters.train, method="class")

# Calculate accuracy of CARTletter
letters.predict.test <- predict(CARTletter, newdata = letters.test, type = "class")
table(letters.test$letter, letters.predict.test)
(348+318+363+340)/nrow(letters.test)

# 2.3 - Predicting the letters A, B, P, R 
# Build a random forest model
set.seed(1000)
rf.letter <- randomForest(letter ~ . - isB, data=letters.train)

# Calculate accuracy of rf.B
letters.predict.test <- predict(rf.letter, newdata = letters.test, type = "class")
table(letters.test$letter, letters.predict.test)
(390+380+393+364)/nrow(letters.test)

#######################################################
# Predicting Earnings from census data
######################################################
rm(list = ls())

# 1.1 - A Logistic Regression Model
# Read data, split into train and test
census <- read.csv("census.csv")
set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, spl==TRUE)
test <- subset(census, spl==FALSE)

# Build glm to predict "over50k"
model.glm <- glm(over50k ~ ., data = train, family = binomial)
summary(model.glm)

# 1.2 - A Logistic Regression Model
predict.test <- predict(model.glm, newdata = test, method = "test")
table(test$over50k, predict.test > 0.5)
(9351+1515)/nrow(test)

# 1.3 - A Logistic Regression Model
table(test$over50k)
9713/nrow(test)

# 1.4 - A Logistic Regression Model
pred = prediction(predict.test, test$over50k)
perf = performance(pred, "tpr", "fpr")
as.numeric(performance(pred, "auc")@y.values)

# 2.1 - A CART Model
model.cart <- rpart(over50k ~ ., data = train, method = "class")

# 2.2/2.3 - A CART Model
prp(model.cart)

# 2.4 - A CART Model
predict.test <- predict(model.cart, newdata = test, type = "class")
table(test$over50k, predict.test)
(9243+1596)/nrow(test)

# 2.5 - A CART Model
predict.test <- predict(model.cart, newdata = test)
pred = prediction(predict.test, test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# 2.6 - A CART Model
as.numeric(performance(pred, "auc")@y.values)

# 3.1 - A Random Forest Model
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
model.rf <- randomForest(over50k ~ ., data = trainSmall)

predict.test <- predict(model.rf, newdata = test)
table(predict.test, test$over50k)
(9586+1093)/nrow(test)

# 3.2 - A Random Forest Model
vu = varUsed(model.rf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(model.rf$forest$xlevels[vusorted$ix]))

# 3.3 - A Random Forest Model
varImpPlot(model.rf)

# 4.1 - Selecting cp by Cross-Validation
numFolds <- trainControl(method = "cv", number = 10)
cartGrid <- expand.grid(.cp = seq(0.002,0.1,0.002))
set.seed(2)
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

# 4.2 - Selecting cp by Cross-Validation
model.cart <- rpart(over50k ~ ., data = train, method = "class", cp = 0.002)
predict.test <- predict(model.cart, newdata = test, type = "class")
table(test$over50k, predict.test)
(9178+1838)/nrow(test)

# 4.3 - Selecting cp by Cross-Validation
prp(model.cart)
