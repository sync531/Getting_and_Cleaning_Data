setwd("C:/Users/S.Y CHOI/Desktop/R/wd")

# # STEP 1: Read the data
subject_test <- read.table("./gcproject/test/subject_test.txt")
x_test <- read.table("./gcproject/test/X_test.txt")
y_test <- read.table("./gcproject/test/y_test.txt")

subject_train <- read.table("./gcproject/train/subject_train.txt")
x_train <- read.table("./gcproject/train/X_train.txt")
y_train <- read.table("./gcproject/train/y_train.txt")

features <- read.table("./gcproject/features.txt")
activity_labels <- read.table("./gcproject/activity_labels.txt")

# # STEP 2: Merge the training and test sets to create one data set
x.data <- rbind(x_train, x_test)
names(x.data) <- as.character(features[,2])

# # STEP 3: Extract only the measurements on the mean and standard deviation for each measurement
subset.mean <- grepl("mean()", names(x.data), fixed = TRUE)
subset.std <- grepl("std()", names(x.data), fixed = TRUE)
subset <- subset.mean | subset.std
meanstd.data <- x.data[,subset]

# # STEP 4: Use descriptive activity names to name the activities in the data set
y.data <- rbind(y_train, y_test)
rename <- as.character(activity_labels[,2])
names(rename) <- activity_labels[,1]
y.data[,1] <- rename[y.data[,1]]
names(y.data) <- "Activity"

## STEP 5: Appropriately label the data set with descriptive variable names
subject.data <- rbind(subject_train, subject_test)
names(subject.data) <- "Subject.No"

data <- cbind(subject.data, y.data, meanstd.data)

## STEP 6: From data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject
final.data <- aggregate(data[,3] ~ Subject.No + Activity, data = data, FUN = "mean")

for(i in 4:ncol(data)){
  temp <- aggregate(data[,i] ~ Subject.No + Activity, data = data, FUN = "mean")
  final.data <- cbind(final.data, temp[,3])
}

names(final.data)[3:ncol(final.data)] <- names(data)[3:ncol(data)]

write.table(final.data, file = "./gcproject/tidy_data.txt", row.name = FALSE)
