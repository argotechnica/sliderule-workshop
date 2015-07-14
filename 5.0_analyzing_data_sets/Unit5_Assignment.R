setwd("C:\\Client\\Classes\\Data Science Workshop (Sliderule)\\5 Analyzing Datasets\\Analytics Edge, The (MITx)\\05 Text Analytics")

library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(tm)
library(SnowballC)
########################################
# detecting vandalism on wikipedia
########################################
rm(list = ls())

wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)

wiki$Vandal = as.factor(wiki$Vandal)

# 1.1 - Bags of Words
table(wiki$Vandal)

# 1.2 - Bag of Words
# Create and prep corpus
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
corpusAdded <- tm_map(corpusAdded, PlainTextDocument)

# Create document term matrix, count # of terms
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

# 1.3 - Bag of Words
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# 1.4 - Bags of Words
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
wordsAdded

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)

dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

ncol(wordsRemoved)

# 1.5 - Bags of Words
wikiWords = cbind(wordsAdded, wordsRemoved) 
wikiWords$Vandal = wiki$Vandal

set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

train = subset(wikiWords, split==TRUE)
test = subset(wikiWords, split==FALSE)

table(test$Vandal)
618/nrow(test)

# 1.6 - Bags of Words
model.CART <- rpart(Vandal ~ ., data = train)
test.predict <- predict(model.CART, newdata = test, type = "class")
table(test$Vandal, test.predict)
(618+12)/nrow(test)

# 1.7 - Bags of Words
prp(model.CART)

# 1.8 - Bags of Words
table(test$Vandal)
618/nrow(test)

# 2.1 - Problem-specific Knowledge
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

# 2.2 - Problem-Specific Knowledge
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
model2.CART <- rpart(Vandal ~ ., data = wikiTrain2)
wikiTest2.predict <- predict(model2.CART, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, wikiTest2.predict)
(609+57)/nrow(wikiTest2)

# 2.3 - Problem-Specific Knowledge
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# 2.4 - Problem-Specific Knowledge
wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
model3.CART <- rpart(Vandal ~ ., data = wikiTrain3)
wikiTest3.predict <- predict(model3.CART, newdata = wikiTest3, type = "class")
table(wikiTest3$Vandal, wikiTest3.predict)
(514+248)/nrow(wikiTest3)

# 3.1 - Using Non-Textual Data 
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
model3.CART <- rpart(Vandal ~ ., data = wikiTrain3)
wikiTest3.predict <- predict(model3.CART, newdata = wikiTest3, type = "class")
table(wikiTest3$Vandal, wikiTest3.predict)
(595+241)/nrow(wikiTest3)

# 3.2 - Using Non-Textual Data
prp(model3.CART)

########################################
# automating reviews in medicine
########################################
rm(list = ls())

# 1.1 - Loading the Data
trials <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
max(nchar(trials$abstract))

# 1.2 - Loading the Data
sum(nchar(trials$abstract) == 0)

# 1.3 - Loading the Data
trials$title[which.min(nchar(trials$title))]

# 2.1 - Preparing the Corpus
# 2.1.1 - Convert the title variable to corpusTitle and the
# abstract variable to corpusAbstract.
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

# 2.1.2 - Convert corpusTitle and corpusAbstract to lowercase
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)

# 2.1.3 - Remove the punctuation in corpusTitle and corpusAbstract
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# 2.1.4 - Remove the English language stop words from corpusTitle
# and corpusAbstract
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

# 2.1.5 - Stem the words in corpusTitle and corpusAbstract
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# 2.1.6 - Build a document term matrix called dtmTitle from
# corpusTitle and dtmAbstract from corpusAbstract
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

# 2.1.7 - Limit dtmTitle and dtmAbstract to terms with sparseness
# of at most 95% (aka terms that appear in at least 5% of documents)
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

# 2.1.8 - Create data frame
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

# How many terms remain in the dtms after removing sparse terms?
length(dtmTitle)
length(dtmAbstract)

# 2.3 - Preparing the Corpus
which.max(colSums(dtmAbstract))

# 3.1 - Building a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# 3.2 - Building a Model
dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
length(dtm)

# 3.3 - Building a Model
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(train$trial)
730/nrow(train)

# 3.4 - Building a Model
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

# 3.5 - Building a Model
predTrain = predict(trialCART)[,2]
max(predTrain)

# 3.6 - Building a Model
# the CART tree assigns the same predicted 
# probability to each leaf node and there are
# a small number of leaf nodes compared to 
# data points, we expect exactly the same 
# maximum predicted probability

# 3.7 - Building a Model 
table(train$trial, predTrain >= 0.5)
(631 + 441)/nrow(train)
441/(441+131)
631/(631+99)

# 4.1 - Evaluating the model on the testing set
predTest <- predict(trialCART, newdata = test)[,2]
table(test$trial, predTest >= 0.5)
(261+162)/nrow(test)

# 4.2 - Evaluating the Model on the Testing Set 
pred = prediction(predTest, test$trial)
perf = performance(pred, "tpr", "fpr")
as.numeric(performance(pred, "auc")@y.values)

########################################
# separating spam from ham (Part 1)
########################################
rm(list = ls())

# 1.1 - Loading the Dataset
emails <- read.csv("emails.csv", stringsAsFactors = FALSE)

# 1.2 - Loading the Dataset
table(emails$spam)

# 1.3 - Loading the Dataset
emails$text[1]

# 1.5 - Loading the Dataset
max(nchar(emails$text))

# 1.6 - Loading the Dataset
which.min(nchar(emails$text))

# 2.1 - Preparing the Corpus
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
dtm

# 2.2 - Preparing the Corpus
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

# 2.3 - Preparing the Corpus 
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

# 2.4 - Preparing the Corpus
emailsSparse$spam <- emails$spam
sum(colSums(subset(emailsSparse, spam == 0)) >= 5000) # different from what was recommended - works

# 2.5 - Preparing the Corpus
# sum(colSums(subset(emailsSparse, spam == 1)) >= 1000) # different from what was recommended - doesn't work b/c "spam" col gets counted; sort and look manually instead
sort(colSums(subset(emailsSparse, spam == 1)))

# 3.1 - Building Machine Learning Models
# Note - I ended up getting this whole section wrong
# because I wrote "split = TRUE/FALSE" instead of
# "split == TRUE/FALSE"... I need to pay bette rattention
# to syntax!
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
split <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, split == TRUE)
test <- subset(emailsSparse, split == FALSE)

spamLog <- glm(spam ~ ., data = train, family = "binomial")
spamCART <- rpart(spam ~ ., data = train, method = "class")
set.seed(123)
spamRF <- randomForest(spam ~ ., data = train)

predictLog <- predict(spamLog, type = "response")
predictCART <- predict(spamCART)[,2]
predictRF <- predict(spamRF, type = "prob")[,2]

table(predictLog < 0.00001)
table(predictLog > 0.99999)
table(predictLog >= 0.00001 & predictLog <= 0.99999)

# 3.2 - Building Machine Learning Models
length(spamLog)

# 3.3 - Building Machine Learning Models
prp(spamCART)

# 3.4 - Building Machine Learning Models
table(train$spam, predictLog > 0.5)
(3052+954)/nrow(train)

# 3.5 - Building Machine Learning Models
ROCRpred = prediction(predictLog, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 3.6 - Building Machine Learning Models
table(train$spam, predictCART > 0.5)
(2885+894)/nrow(train)

# 3.7 - Building Machine Learning Models
ROCRpred = prediction(predictCART, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 3.8 - Building Machine Learning Models
table(train$spam, predictRF > 0.5)
(3013+914)/nrow(train)

# 3.9 - Building Machine Learning Models
ROCRpred = prediction(predictRF, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 4.1 - Evaluating on the Test Set
predictLog <- predict(spamLog, newdata = test, type = "response")
predictCART <- predict(spamCART, newdata = test)[,2]
predictRF <- predict(spamRF, newdata = test, type = "prob")[,2]
table(test$spam, predictLog > 0.5)
(1257+376)/nrow(test)

# 3.5 - Building Machine Learning Models
ROCRpred = prediction(predictLog, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 3.6 - Building Machine Learning Models
table(test$spam, predictCART > 0.5)
(1228+386)/nrow(test)

# 3.7 - Building Machine Learning Models
ROCRpred = prediction(predictCART, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 3.8 - Building Machine Learning Models
table(test$spam, predictRF > 0.5)
(1290+385)/nrow(test)

# 3.9 - Building Machine Learning Models
ROCRpred = prediction(predictRF, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)
