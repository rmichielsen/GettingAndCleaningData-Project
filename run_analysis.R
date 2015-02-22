# Step1. Merges the training and the test sets to create one data set.
train.data <- read.table("./UCI HAR Dataset/train/X_train.txt")
train.label <- read.table("./UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

test.data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test.label <- read.table("./UCI HAR Dataset/test/y_test.txt") 
test.subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

merged.data <- rbind(train.data, test.data)
merged.label <- rbind(train.label, test.label)
merged.subject <- rbind(train.subject, test.subject)

# Step2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./UCI HAR Dataset/features.txt")

indices <- grep("mean\\(\\)|std\\(\\)", features[, 2])

merged.data <- merged.data[, indices]

names(merged.data) <- gsub("\\(\\)", "", features[indices, 2])

# Step3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity.label <- activity[merged.label[, 1], 2]
merged.label[, 1] <- activity.label
names(merged.label) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity names. 
names(merged.subject) <- "subject"
cleaned.data <- cbind(merged.subject, merged.label, merged.data)
write.table(cleaned.data, "tidy_data.txt",  row.name=FALSE)

# Step5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subjectLen <- length(table(merged.subject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleaned.data)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleaned.data)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(merged.subject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleaned.data$subject
    bool2 <- activity[j, 2] == cleaned.data$activity
    result[row, 3:columnLen] <- colMeans(cleaned.data[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
write.table(result, "tidy_data_with_means.txt",  row.name=FALSE) 