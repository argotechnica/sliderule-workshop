setwd("C:\\Client\\Classes\\Data Science Workshop (Sliderule)\\5 Analyzing Datasets\\Analytics Edge, The (MITx)\\06 Clustering")

## I think I like how your libraries go at the top.  Also how you do an absolute path as above -- very repeatable.
library(caret)
library(caTools)
library(flexclust)

#######################################################
# document clustering with daily kos
#######################################################
rm(list = ls())
kos <- read.csv("dailykos.csv")

# 1.1 - Hierarchical Clustering
str(kos)
distances = dist(kos, method = "euclidean")
clusters = hclust(distances, method="ward.D")

# 1.2 - Hierarchical Clustering
plot(clusters)

# 1.4 - Hierarchical Clustering
kos.clusters = cutree(clusters, k = 7)

# 3 approaches to finding the number of obs
# in each cluster... approach 1
kos.1 <- subset(kos, kos.clusters==1)
kos.2 <- subset(kos, kos.clusters==2)
kos.3 <- subset(kos, kos.clusters==3)
kos.4 <- subset(kos, kos.clusters==4)
kos.5 <- subset(kos, kos.clusters==5)
kos.6 <- subset(kos, kos.clusters==6)
kos.7 <- subset(kos, kos.clusters==7)
# approach 2
#nice!
table(kos.clusters)

# 1.5 - Hierarchical Clustering
tail(sort(colMeans(kos.1)))
tail(sort(colMeans(kos.2)))
tail(sort(colMeans(kos.3)))
tail(sort(colMeans(kos.4)))
tail(sort(colMeans(kos.5)))
tail(sort(colMeans(kos.6)))
tail(sort(colMeans(kos.7)))

# 2.1 - K-Means Clustering
set.seed(1000)
KMC = kmeans(kos, centers = 7)
# I had to get at the cluster 1 
# kmc[1]$cluster.  It looks like this might be a similar idea with different syntax?
kos.k1 <- subset(kos, KMC$cluster==1)
kos.k2 <- subset(kos, KMC$cluster==2)
kos.k3 <- subset(kos, KMC$cluster==3)
kos.k4 <- subset(kos, KMC$cluster==4)
kos.k5 <- subset(kos, KMC$cluster==5)
kos.k6 <- subset(kos, KMC$cluster==6)
kos.k7 <- subset(kos, KMC$cluster==7)

# 2.2 - K-Means Clustering
# Interesting to me that tail does 6 most frequent words since
# there's no parameter specifying size of the tail we want to see?
tail(sort(colMeans(kos.1)))
tail(sort(colMeans(kos.k1)))
tail(sort(colMeans(kos.2)))
tail(sort(colMeans(kos.k2)))
tail(sort(colMeans(kos.3)))
tail(sort(colMeans(kos.k3)))
tail(sort(colMeans(kos.4)))
tail(sort(colMeans(kos.k4)))
tail(sort(colMeans(kos.5)))
tail(sort(colMeans(kos.k5)))
tail(sort(colMeans(kos.6)))
tail(sort(colMeans(kos.k6)))
tail(sort(colMeans(kos.7)))
tail(sort(colMeans(kos.k7)))

#######################################################
# market Segmentation For Airlines
#######################################################
rm(list = ls())
airlines <- read.csv("AirlinesCluster.csv")

# 1.1 - Normalizing the Data 
# Hope you might remind me about sapply also.  I think
#  I got a little carried away with lapply when sapply or tapply 
#  may have been more appropriate.
sort(sapply(airlines, mean, na.rm=TRUE))

# 1.3 - Normalizing the Data
# Preprocess is MAGIC!
preproc = preProcess(airlines)
# Is there a time when we wouldn't want to normalize our data first?
#  I think I'd have to dig more into the normalization equation but 
#  intuitively it shouldn't alter the data much if it's all pretty close together, right?
airlinesNorm = predict(preproc, airlines)
# much nicer and more reusable than my kludged lapply call.
sort(sapply(airlinesNorm, max, na.rm=TRUE))
sort(sapply(airlinesNorm, min, na.rm=TRUE))

# 2.1 - Hierarchical Clustering
distances = dist(airlinesNorm, method = "euclidean")
clusters = hclust(distances, method="ward.D")
plot(clusters)

# 2.2 - Hierarchical Clustering
airlines.hc = cutree(clusters, k = 5)
table(airlines.hc)

# 2.3-2.7 - Hierarchical Clustering
# let's review the tapply function if that's ok with you, I
# think I've kind of forgotten how it works. And I overcomplicated my response to these questions.
tapply(airlines$Balance, airlines.hc, mean)
tapply(airlines$QualMiles, airlines.hc, mean)
tapply(airlines$BonusMiles, airlines.hc, mean)
tapply(airlines$BonusTrans, airlines.hc, mean)
tapply(airlines$FlightMiles, airlines.hc, mean)
tapply(airlines$FlightTrans, airlines.hc, mean)
tapply(airlines$DaysSinceEnroll, airlines.hc, mean)
#or
lapply(split(airlines, airlines.hc), colMeans)

# 3.1 - K-Means Clustering
set.seed(88)
airlines.kmc = kmeans(airlines, centers = 5, iter.max=1000)
table(airlines.kmc$cluster)

# 3.2 - K-Means Clustering
airlines.kmc$centers

#######################################################
# Predicting Stock Returns with Cluster-Then-Predict
#######################################################
rm(list = ls())
stocks <- read.csv("StocksCluster.csv")

# 1.1 - Exploring the Dataset 
nrow(stocks)

# 1.2 - Exploring the Dataset 
mean(stocks$PositiveDec)

# 1.3 - Exploring the Dataset
sort(cor(stocks[1:11]))
# Again, way nicer use of this function than what I did.
#  Didn't recall that you could use it across a list and thought
#  you could only do pairwise.  Thanks for showing me the light!
cor(stocks[1:11])

# 1.4 - Exploring the Dataset
sort(colMeans(stocks[1:11]))

# 2.1 - Initial Logistic Regression Model
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel <- glm(PositiveDec ~ ., stocksTrain, family = binomial)
predict.StocksModel <- predict(StocksModel, type = "response")
#WELCOME TO THE CONFUSION MATRIX
table(stocksTrain$PositiveDec, predict.StocksModel > 0.5)
(990+3640)/nrow(stocksTrain)

# 2.2 - Initial Logistic Regression Model
predict.StocksModel <- predict(StocksModel, type="response", newdata = stocksTest)
table(stocksTest$PositiveDec, predict.StocksModel > 0.5)
(417+1553)/nrow(stocksTest)

# 2.3 - Initial Logistic Regression Model
table(stocksTest$PositiveDec)
1897/nrow(stocksTest)

# 3.1 - Clustering Stocks 
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# 3.2 - Clustering Stocks
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

# 3.4 - Clustering Stocks
set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster) #or: km$size

# 3.5 - Clustering Stocks 
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# 4.1 - Cluster-Specific Predictions
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)
stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# 4.2 - Cluster-Specific Predictions
StocksModel1 <- glm(PositiveDec ~ ., stocksTrain1, family = binomial)
StocksModel2 <- glm(PositiveDec ~ ., stocksTrain2, family = binomial)
StocksModel3 <- glm(PositiveDec ~ ., stocksTrain3, family = binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

# 4.3 - Cluster-Specific Predictions
PredictTest1 <- predict(StocksModel1, type = "response", newdata = stocksTest1)
PredictTest2 <- predict(StocksModel2, type = "response", newdata = stocksTest2)
PredictTest3 <- predict(StocksModel3, type = "response", newdata = stocksTest3)
table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30+774)/nrow(stocksTest1)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388+757)/nrow(stocksTest2)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13)/nrow(stocksTest3)

# 4.4 - Cluster-Specific Predictions
#  I have an answer to this question, but I think I must have overwritten my file.
#  Hope you don't mind if I swipe these lines for my file (not sure I'll ever come back to it but just in case.)
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467+1544)/(467+1110+353+1544)
