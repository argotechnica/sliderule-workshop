'''
Dataset:

  https://d396qusza40orc.cloudfront.net/
    getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

This R script:

  1. Merges the training and the test sets to create one data set.
  2. Extracts only the measurements on the mean and standard deviation for each
     measurement.
  3. Uses descriptive activity names to name the activities in the data set
  4. Appropriately labels the data set with descriptive variable names.
  5. From the data set in step 4, creates a second, independent tidy data set
     with the average of each variable for each activity and each subject.  
'''
library("dplyr")
library("readr")

setwd("C:\\Client\\Classes\\Data Science Workshop (Sliderule)\\4 Data Wrangling\\4.1 Dplyr")

# load activity and feature labels, using make.names to create syntactically
# valid, unique column names--a best practice anyway, but required for select()

activityLabels <- read_delim(".\\UCI HAR Dataset\\activity_labels.txt",
                             delim = " ",
                             col_names = c("ActivityID", "ActivityName"))
features <- read_delim(".\\UCI HAR Dataset\\features.txt",
                       delim = " ",
                       col_names = c("FeatureID", "FeatureName"))
features$FeatureName <- make.names(features$FeatureName, unique = TRUE)

# load in test and train data separately, getting "mean" and "std" columns
# beforehand, then merge into one dataset

testSubjects <- read_table(".\\UCI HAR Dataset\\test\\subject_test.txt",
                           col_names = "SubjectID")
testActivityLabels <- read_table(".\\UCI HAR Dataset\\test\\Y_test.txt",
                                 col_names = "ActivityID")
testActivityData <- read_table(".\\UCI HAR Dataset\\test\\X_test.txt",
                               col_names = features$FeatureName)
testData <- bind_cols(testSubjects,
                      testActivityLabels,
                      select(testActivityData, contains("mean")),
                      select(testActivityData, contains("std")))
trainSubjects <- read_table(".\\UCI HAR Dataset\\train\\subject_train.txt",
                            col_names = "SubjectID")
trainActivityLabels <- read_table(".\\UCI HAR Dataset\\train\\Y_train.txt",
                                  col_names = "ActivityID")
trainActivityData <- read_table(".\\UCI HAR Dataset\\train\\X_train.txt",
                                col_names = features$FeatureName)
trainData <- bind_cols(trainSubjects,
                       trainActivityLabels,
                       select(trainActivityData, contains("mean")),
                       select(trainActivityData, contains("std")))

# for readability's sake, first combine, then left_join labels
combiData <- rbind(trainData, testData)
combiData <- left_join(combiData, activityLabels)

# use "summarise_each" to run mean fun on all columns besides grouped columns
summaryData <- combiData %>%
  group_by(SubjectID, ActivityID, ActivityName) %>%
  summarise_each(funs(mean))