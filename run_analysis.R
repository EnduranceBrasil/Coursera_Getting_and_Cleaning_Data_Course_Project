library(dplyr)

# download zip file containing datas
zip_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip_File <- "UCI HAR Dataset.zip"

if (!file.exists(zip_File)) {
  download.file(zip_Url, zip_File, mode = "wb")
}

data_Path <- "UCI HAR Dataset"
if (!file.exists(data_Path)) {
  unzip(zip_File)
}

# read training data
training_Subjects <- read.table(file.path(data_Path, "train", "subject_train.txt"))
training_Values <- read.table(file.path(data_Path, "train", "X_train.txt"))
training_Activity <- read.table(file.path(data_Path, "train", "y_train.txt"))

# read test data
test_Subjects <- read.table(file.path(data_Path, "test", "subject_test.txt"))
test_Values <- read.table(file.path(data_Path, "test", "X_test.txt"))
test_Activity <- read.table(file.path(data_Path, "test", "y_test.txt"))

# read features
features <- read.table(file.path(data_Path, "features.txt"), as.is = TRUE)
features[,2] <- as.character(features[,2])

# read activity labels
activities <- read.table(file.path(data_Path, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# concatenate individual data tables to make single data table
human_Activity <- rbind(
  cbind(training_Subjects, training_Values, training_Activity),
  cbind(test_Subjects, test_Values, test_Activity)
)

# remove individual data tables to save memory
rm(training_Subjects, training_Values, training_Activity, 
   test_Subjects, test_Values, test_Activity)

# assign column names
colnames(human_Activity) <- c("subject", features[, 2], "activity")

# determine columns of data set to keep based on column name...
Data_Columns <- grepl("subject|activity|mean|std", colnames(human_Activity))

# ... and keep data in these columns only
human_Activity <- human_Activity[, Data_Columns]


# replace activity values with named factor levels
human_Activity$activity <- factor(human_Activity$activity, 
  levels = activities[, 1], labels = activities[, 2])

# get column names
Activity_Columns <- colnames(human_Activity)

# remove special characters
Activity_Columns <- gsub("[\\(\\)-]", "", Activity_Columns)

# clean up names
Activity_Columns <- gsub("^f", "frequencyDomain", Activity_Columns)
Activity_Columns <- gsub("^t", "timeDomain", Activity_Columns)
Activity_Columns <- gsub("Acc", "Accelerometer", Activity_Columns)
Activity_Columns <- gsub("Gyro", "Gyroscope", Activity_Columns)
Activity_Columns <- gsub("Mag", "Magnitude", Activity_Columns)
Activity_Columns <- gsub("Freq", "Frequency", Activity_Columns)
Activity_Columns <- gsub("mean", "Mean", Activity_Columns)
Activity_Columns <- gsub("std", "StandardDeviation", Activity_Columns)
Activity_Columns <- gsub("BodyBody", "Body", Activity_Columns)

# use new labels as column names
colnames(human_Activity) <- Activity_Columns

# group by subject and activity and summarise using mean
Activity_Means <- human_Activity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(Activity_Means, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)