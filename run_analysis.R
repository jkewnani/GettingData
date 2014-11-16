# Getting and Cleaning Data - Project

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the
# average of each variable for each activity and each subject

setwd("~/Documents/R/GettingData")
library(dplyr)
library(data.table)
library(reshape2)

### Load data
# test data
test_X <- read.table("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
# train data
train_X <- read.table("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
# labels
features <- read.table("./UCI HAR Dataset/features.txt", sep="", header=FALSE,colClasses="character")
activityLabels <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE,colClasses="character")

### Merge data
# merge y test and y training with activity labels
train_y_labels <- merge(train_y,activityLabels,by="V1")
test_y_labels <- merge(test_y,activityLabels,by="V1")
# add headers
names(train_X) <- features$V2
names(test_X) <- features$V2

### Extract mean and standard deviation
features_meanstd_col <- grepl("mean|std", features$V2)
train_X <- train_X[,features_meanstd_col]
test_X <- test_X[,features_meanstd_col]
# add cctivity labels
names(train_y_labels) = c("Activity_ID", "Activity")
names(test_y_labels) = c("Activity_ID", "Activity")
names(test_subject) = "Subject"
names(train_subject) = "Subject"
# merge test and training data and labels
train_data <- cbind(train_subject, train_y_labels, train_X)
test_data <- cbind(test_subject, test_y_labels, test_X)
# merge test and training data
all_data <- rbind(train_data,test_data)

### Get average of each variable for each activity and each subject
id_labels   <- c("Subject", "Activity_ID", "Activity")
data_labels <- setdiff(colnames(all_data), id_labels)
all_data_melted <- melt(all_data, id = id_labels, measure.vars=data_labels)
tidy_data <- dcast(all_data_melted, formula = Subject + Activity ~ variable, mean)

### Create second data set
write.table(tidy_data, file = "./tidy_data.txt", sep="\t")
