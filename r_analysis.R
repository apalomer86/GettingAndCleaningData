# 1. Merges the training and the test sets to create one data set.
library("dplyr")
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
features <- as.character(features[, 2]) 
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                              header = FALSE) 
activity_labels <- tolower(as.character(activity_labels[, 2])) 
train_X <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                      header = FALSE) 
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt", 
                      header = FALSE)
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt",
                            header = FALSE)
df_train <- data.frame(train_subject, train_y, train_X)
names(df_train) <- c(c('subject', 'activity'), features)
test_X <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                     header = FALSE) 
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt", 
                     header = FALSE) 
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                           header = FALSE) 
df_test <- data.frame(test_subject, test_y, test_X)
names(df_test) <- c(c('subject', 'activity'), features)
df_merge <- rbind(df_train, df_test)



# 2. Extracts only the measurements on the mean and standard deviation 
#    for each measurement. 
mean_std <- grep("mean|std", features)
df_mean_std <- df_merge[, c(1, 2, mean_std + 2)]



# 3. Uses descriptive activity names to name the activities in the
#    data set
df_mean_std$activity <- gsub("1", activity_labels[1], df_mean_std$activity)
df_mean_std$activity <- gsub("2", activity_labels[2], df_mean_std$activity) 
df_mean_std$activity <- gsub("3", activity_labels[3], df_mean_std$activity)
df_mean_std$activity <- gsub("4", activity_labels[4], df_mean_std$activity)
df_mean_std$activity <- gsub("5", activity_labels[5], df_mean_std$activity)
df_mean_std$activity <- gsub("6", activity_labels[6], df_mean_std$activity)



# 4. Appropriately labels the data set with descriptive variable names
colnames(df_mean_std) <- gsub("[(][)]", "", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("-", "_", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("mean", "Mean", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("std", "StandardDeviation", 
                              colnames(df_mean_std))
colnames(df_mean_std) <- gsub("Freq", "Frequency", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("^t", "TimeDomain_", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("^f", "FrequencyDomain_", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("Acc", "Accelerometer", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("Gyro", "Gyroscope", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("Mag", "Magnitude", colnames(df_mean_std))
colnames(df_mean_std) <- gsub("BodyBody", "Body", colnames(df_mean_std))
View(df_mean_std)



# 5. From the data set in step 4, creates a second, independent tidy data 
#    set with the average of each variable for each activity and each 
#    subject.
df_tidy <- df_mean_std %>%
  group_by(activity, subject) %>%
  summarize(across(everything()))
View(df_tidy)

tidy <- write.table(df_tidy, "df_tidy.txt")
tidy_data <- read.table("df_tidy.txt")
View(tidy_data)

