#Codebook for SlideRule Data Wrangling Project
Codebook describing the variables, data, and any transformations or work
performed to clean up the data varaibles, for the "Data Wrangling Project"
assignment in Lesson 4.1 - Dplyr of SlideRule's Intro to Data Science
workshop, using the "Human Activity Recognition Using Smartphones" data set
 
##Dataset Notes
Dataset created as an activity for learning more about R's dplyr package
by manipulating a dataset created for this publication:
 
 Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-
 Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-
 Friendly Support Vector Machine. International Workshop of Ambient Assisted
 Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
 [link] (https://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
 
 The original data measure accelerometer and gyroscope data from a smartphone
 as subjects perform one of six activities, e.g. sitting or walking. A total of
 7352 observations are provided in the source dataset, which for this activity,
 were combined from 8 unique data files, filtered to include only means and
 standard deviations of measurements, then averaged (summarised by mean) across
 all observations for each user and user activity.
 
 To perform the transformations, a number of dplyr as well as readr functions are
 used, including:
   * base
     * make.names - To get unqiue, compliant column names--particularly for use by
	 dplyr's select() function later on.
	 * gsub - To clean 
   * readr
     * read_table - Original datasets included 560+ variables (columns) in a fixed-
   width format, so this seemed to be the best fastest way to pull all in at once.
   * dplyr
     * bind_cols - To combine datasets. (Labels were provided separately
	 from observations and so needed to be combined.)
	 * left_join - To pull in activity labels.
	 * select - To selectively grab data.
	 * summarise_each - To get means of multiple columns simultaneously.
 
##Variables and Values
```
 Variable Name                       Type  Value Description
 SubjectID                           int   
 ActivityID                          int   
 ActivityName                        chr   
 tBodyAcc.mean.X                     num   Mean of means of observations
 tBodyAcc.mean.Y                     num   Mean of means of observations
 tBodyAcc.mean.Z                     num   Mean of means of observations
 tGravityAcc.mean.X                  num   Mean of means of observations
 tGravityAcc.mean.Y                  num   Mean of means of observations
 tGravityAcc.mean.Z                  num   Mean of means of observations
 tBodyAccJerk.mean.X                 num   Mean of means of observations
 tBodyAccJerk.mean.Y                 num   Mean of means of observations
 tBodyAccJerk.mean.Z                 num   Mean of means of observations
 tBodyGyro.mean.X                    num   Mean of means of observations
 tBodyGyro.mean.Y                    num   Mean of means of observations
 tBodyGyro.mean.Z                    num   Mean of means of observations
 tBodyGyroJerk.mean.X                num   Mean of means of observations
 tBodyGyroJerk.mean.Y                num   Mean of means of observations
 tBodyGyroJerk.mean.Z                num   Mean of means of observations
 tBodyAccMag.mean                    num   Mean of means of observations
 tGravityAccMag.mean                 num   Mean of means of observations
 tBodyAccJerkMag.mean                num   Mean of means of observations
 tBodyGyroMag.mean                   num   Mean of means of observations
 tBodyGyroJerkMag.mean               num   Mean of means of observations
 fBodyAcc.mean.X                     num   Mean of means of observations
 fBodyAcc.mean.Y                     num   Mean of means of observations
 fBodyAcc.mean.Z                     num   Mean of means of observations
 fBodyAcc.meanFreq.X                 num   Mean of means of observations
 fBodyAcc.meanFreq.Y                 num   Mean of means of observations
 fBodyAcc.meanFreq.Z                 num   Mean of means of observations
 fBodyAccJerk.mean.X                 num   Mean of means of observations
 fBodyAccJerk.mean.Y                 num   Mean of means of observations
 fBodyAccJerk.mean.Z                 num   Mean of means of observations
 fBodyAccJerk.meanFreq.X             num   Mean of means of observations
 fBodyAccJerk.meanFreq.Y             num   Mean of means of observations
 fBodyAccJerk.meanFreq.Z             num   Mean of means of observations
 fBodyGyro.mean.X                    num   Mean of means of observations
 fBodyGyro.mean.Y                    num   Mean of means of observations
 fBodyGyro.mean.Z                    num   Mean of means of observations
 fBodyGyro.meanFreq.X                num   Mean of means of observations
 fBodyGyro.meanFreq.Y                num   Mean of means of observations
 fBodyGyro.meanFreq.Z                num   Mean of means of observations
 fBodyAccMag.mean                    num   Mean of means of observations
 fBodyAccMag.meanFreq                num   Mean of means of observations
 fBodyBodyAccJerkMag.mean            num   Mean of means of observations
 fBodyBodyAccJerkMag.meanFreq        num   Mean of means of observations
 fBodyBodyGyroMag.mean               num   Mean of means of observations
 fBodyBodyGyroMag.meanFreq           num   Mean of means of observations
 fBodyBodyGyroJerkMag.mean           num   Mean of means of observations
 fBodyBodyGyroJerkMag.meanFreq       num   Mean of means of observations
 angle.tBodyAccMean.gravity          num   Mean of means of observations
 angle.tBodyAccJerkMean.gravityMean  num   Mean of means of observations
 angle.tBodyGyroMean.gravityMean     num   Mean of means of observations
 angle.tBodyGyroJerkMean.gravityMean num   Mean of means of observations
 angle.X.gravityMean                 num   Mean of means of observations
 angle.Y.gravityMean                 num   Mean of means of observations
 angle.Z.gravityMean                 num   Mean of means of observations
 tBodyAcc.std.X                      num   Mean of std dev. of observations
 tBodyAcc.std.Y                      num   Mean of std dev. of observations
 tBodyAcc.std.Z                      num   Mean of std dev. of observations
 tGravityAcc.std.X                   num   Mean of std dev. of observations
 tGravityAcc.std.Y                   num   Mean of std dev. of observations
 tGravityAcc.std.Z                   num   Mean of std dev. of observations
 tBodyAccJerk.std.X                  num   Mean of std dev. of observations
 tBodyAccJerk.std.Y                  num   Mean of std dev. of observations
 tBodyAccJerk.std.Z                  num   Mean of std dev. of observations
 tBodyGyro.std.X                     num   Mean of std dev. of observations
 tBodyGyro.std.Y                     num   Mean of std dev. of observations
 tBodyGyro.std.Z                     num   Mean of std dev. of observations
 tBodyGyroJerk.std.X                 num   Mean of std dev. of observations
 tBodyGyroJerk.std.Y                 num   Mean of std dev. of observations
 tBodyGyroJerk.std.Z                 num   Mean of std dev. of observations
 tBodyAccMag.std                     num   Mean of std dev. of observations
 tGravityAccMag.std                  num   Mean of std dev. of observations
 tBodyAccJerkMag.std                 num   Mean of std dev. of observations
 tBodyGyroMag.std                    num   Mean of std dev. of observations
 tBodyGyroJerkMag.std                num   Mean of std dev. of observations
 fBodyAcc.std.X                      num   Mean of std dev. of observations
 fBodyAcc.std.Y                      num   Mean of std dev. of observations
 fBodyAcc.std.Z                      num   Mean of std dev. of observations
 fBodyAccJerk.std.X                  num   Mean of std dev. of observations
 fBodyAccJerk.std.Y                  num   Mean of std dev. of observations
 fBodyAccJerk.std.Z                  num   Mean of std dev. of observations
 fBodyGyro.std.X                     num   Mean of std dev. of observations
 fBodyGyro.std.Y                     num   Mean of std dev. of observations
 fBodyGyro.std.Z                     num   Mean of std dev. of observations
 fBodyAccMag.std                     num   Mean of std dev. of observations
 fBodyBodyAccJerkMag.std             num   Mean of std dev. of observations
 fBodyBodyGyroMag.std                num   Mean of std dev. of observations
 fBodyBodyGyroJerkMag.std            num   Mean of std dev. of observations
```
