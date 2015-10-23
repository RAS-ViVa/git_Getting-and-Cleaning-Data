## Course Project "The goal is to prepare tidy data that can be used for later analysis."
## The goal is to prepare tidy data that can be used for later analysis.

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## Merges the training and the test sets to create one data set.

## prepare work envoriment
##library(dplyr)

## 1.Create the directory for local store date sets and fit it
if(!file.exists("./myproject")) {dir.create("./myproject")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./myproject/Dataset.zip", method = "curl")
unzip("./myproject/Dataset.zip", exdir= "./myproject")
##create constante with path to primarary data sets
datasetsPath <- file.path("./myproject" , "UCI HAR Dataset")


##read files and create table with test data and rename required columns
featuresNames_Set <- read.table(file.path(datasetsPath, "features.txt" ),header = FALSE, stringsAsFactors = FALSE)
subject_testSet <- read.table(file.path(datasetsPath, "test" , "subject_test.txt" ),header = FALSE)
setnames(subject_testSet, "id_subject")
label_testSet <- read.table(file.path(datasetsPath, "test" , "y_test.txt" ),header = FALSE)
setnames(label_testSet, "id_label")
raw_testSet <- read.table(file.path(datasetsPath, "test" , "X_test.txt" ),header = FALSE)
names(raw_testSet)<- featuresNames_Set$V2
testSet_1 <- cbind(subject_testSet, label_testSet)
testSet <- cbind(testSet_1, raw_testSet)

##read files and create table with train train
subject_trainSet <- read.table(file.path(datasetsPath, "train" , "subject_train.txt" ),header = FALSE)
label_trainSet <- read.table(file.path(datasetsPath, "train" , "y_train.txt" ),header = FALSE)
raw_trainSet <- read.table(file.path(datasetsPath, "train" , "X_train.txt" ),header = FALSE)
trainSet_1 <- cbind(subject_trainSet, label_trainSet)
trainSet <- cbind(trainSet_1, raw_trainSet)

##merge test and train sets
finishSet <- rbind(testSet, trainSet)

##2. Extracts only the measurements on the mean and standard deviation for each measurement
setMeanStd <- finishSet[ ,grep("id|mean|std", names(finishSet))]

## 4. Appropriately labels the data set with descriptive variable names.
activity_testSet <- read.table(file.path(datasetsPath, "activity_labels.txt" ),header = FALSE)
setnames(activity_testSet, c("id_label", "activity"))
activitySetMeanStd <- merge(setMeanStd, activity_testSet,  by="id_label", all.x=TRUE)

##4. Appropriately labels the data set with descriptive variable names.
names(activitySetMeanStd) <- gsub("std()", "Standard deviation", names(activitySetMeanStd))
names(activitySetMeanStd) <- gsub("mean()", "Mean value", names(activitySetMeanStd))
names(activitySetMeanStd) <- gsub("^t", "time", names(activitySetMeanStd))
names(activitySetMeanStd) <- gsub("^f", "frequency", names(activitySetMeanStd))
names(activitySetMeanStd) <- gsub("Acc", "Accelerometer", names(activitySetMeanStd))
names(activitySetMeanStd) <- gsub("Gyro", "Gyroscope", names(activitySetMeanStd))
names(activitySetMeanStd) <- gsub("Mag", "Magnitude", names(activitySetMeanStd))

##5. From the data set in step 4, creates a second, independent tidy data set with the average
##of each variable for each activity and each subject.

newTidySet_1 <- aggregate(. ~activity + id_subject, activitySetMeanStd, mean)
newTidySet_0 <- arrange(newTidySet_1, id_label, id_subject)
newTidySet <- select (newTidySet_0, -(id_label))

write.table(newTidySet, file = "./newTidySet.txt")
