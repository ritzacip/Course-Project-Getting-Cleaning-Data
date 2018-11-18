library(dplyr)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

file <- "UCI HAR Dataset.zip"

if (!file.exists(file)) 
{
  download.file(url, file, mode = "wb")
}
  
file2 <- "UCI HAR Dataset"
if (!file.exists(file2)) {
  unzip(file)
}

#1.Merges the training and the test sets to create one data set.

trainSub <- read.table(file.path(file2, "train", "subject_train.txt"))

trainVal <- read.table(file.path(file2, "train", "X_train.txt"))

trainAct <- read.table(file.path(file2, "train", "y_train.txt"))

testSub <- read.table(file.path(file2, "test", "subject_test.txt"))

testVal <- read.table(file.path(file2, "test", "X_test.txt"))

testAct <- read.table(file.path(file2, "test", "y_test.txt"))

feat <- read.table(file.path(file2, "features.txt"), as.is = TRUE)

act <- read.table(file.path(file2, "activity_labels.txt"))

colnames(act) <- c("activityId", "activityLabel")

humanAct <- rbind(
  cbind(trainSub, trainVal, trainAct),
  cbind(testSub, testVal, testAct)
)

rm(trainSub, trainVal, trainAct, 
   testSub, testVal, testAct)

colnames(humanAct) <- c("subject", features[, 2], "activity")


#2. Extracts only the measurements on the mean and standard deviation for each measurement

cols <- grepl("subject|activity|mean|std", colnames(humanAct))

humanAct <- humanAct[, cols]


#3. Uses descriptive activity names to name the activities in the data set

humanAct$activity <- factor(humanAct$activity,levels = act[, 1], labels = act[, 2])


#4.Appropriately labels the data set with descriptive variable names.

humanActCols <- colnames(humanAct)

humanActCols <- gsub("[\\(\\)-]", "", humanActCols)

humanActCols <- gsub("^f", "frequencyDomain", humanActCols)

humanActCols <- gsub("^t", "timeDomain", humanActCols)

humanActCols <- gsub("Acc", "Accelerometer", humanActCols)

humanActCols <- gsub("Gyro", "Gyroscope", humanActCols)

humanActCols <- gsub("Mag", "Magnitude", humanActCols)

humanActCols <- gsub("Freq", "Frequency", humanActCols)

humanActCols <- gsub("mean", "Mean", humanActCols)

humanActCols <- gsub("std", "StandardDeviation", humanActCols)

humanActCols <- gsub("BodyBody", "Body", humanActCols)

colnames(humanAct) <- humanActCols


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity 
#and each subject.

humanActMeans <- humanAct %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(humanActMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
