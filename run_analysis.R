run_analysis <- function(){

  
## Please set the working directory to parent director of the data set unziped at UCI HAR Dataset
  
  
# Directory and filename (.txt) of the average clean/tidy data
tidyDataFileAVGtxt <- "./tidy_dataset_AVG.txt"

## 1. Merges the training and the test sets to create one data set:
## Read each data set using read table 
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# Combines data table (train & test) by rows
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
s <- rbind(subject_train, subject_test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement:
# Read features labels
features <- read.table("./UCI HAR Dataset/features.txt")

# Friendly names to features column
names(features) <- c('feature_id', 'feature_name')

# Search for matches to argument mean or standard deviation (sd)  within each element of character vector
index_features <- grep("-mean\\(\\)|-std\\(\\)", features$feature_name) 
x <- x[, index_features] 

# Replaces all matches of a string features 
names(x) <- gsub("\\(|\\)", "", (features[index_features, 2]))

## 3. Uses descriptive activity names to name the activities in the data set:
## 4. Appropriately labels the data set with descriptive activity names:
# Read activity labels
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Friendly names to activities column
names(activities) <- c('activity_id', 'activity_name')

y[, 1] = activities[y[, 1], 2]

names(y) <- "Activity"
names(s) <- "Subject"

# Combines data table by columns
mergedDataSet <- cbind(s, y, x)

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject:

tmp_tidy <- mergedDataSet[, 3:dim(mergedDataSet)[2]] 
avgDataSet <- aggregate(tmp_tidy,list(mergedDataSet$Subject, mergedDataSet$Activity), mean)

# Meaningful names for columns 
names(avgDataSet)[1] <- "Subject"
names(avgDataSet)[2] <- "Activity"
names(avgDataSet)<-gsub("^t", "time", names(avgDataSet))
names(avgDataSet)<-gsub("^f", "frequency", names(avgDataSet))
names(avgDataSet)<-gsub("Acc", "Accelerometer", names(avgDataSet))
names(avgDataSet)<-gsub("Gyro", "Gyroscope", names(avgDataSet))
names(avgDataSet)<-gsub("Mag", "Magnitude", names(avgDataSet))
names(avgDataSet)<-gsub("BodyBody", "Body", names(avgDataSet))

print(names(avgDataSet))

# Created txt (tidy data set AVG) in directory
write.table(avgDataSet, tidyDataFileAVGtxt, row.name = FALSE)


}