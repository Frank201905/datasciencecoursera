library(dplyr)

project_file <- "ProjectData.zip"
# Checking if given name already exists.
if (!file.exists(project_file)){
  file_URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(file_URL, project_file, method="curl")
}
# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) unzip(project_file)
#Reading the tables of interest, adding column names and assigning each table to a variable.

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","feature"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activityId", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subjectId")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features [ ,2])
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activityId")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subjectId")
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features [ ,2])
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activityId")

#Merging train and test data

merged_train = cbind(ytrain, subject_train, xtrain)
merged_test = cbind(ytest, subject_test, xtest)
one_dataset = rbind(merged_train, merged_test)

#Including descriptive activity names to name activities in the data set. 
dataset_activity<-right_join(activities,one_dataset, by= "activityId")

#Selecting measurements on the mean and standard deviation

selectedData <- dataset_activity %>% select(subjectId, activity, contains("mean"), contains("std"))

#Labeled the data set with descriptive variables names


names(selectedData)<-gsub("Acc", "Acceleration", names(selectedData))
names(selectedData)<-gsub("Gyro", "Gyroscope", names(selectedData))
names(selectedData)<-gsub("BodyBody", "Body", names(selectedData))
names(selectedData)<-gsub("Mag", "Magnitude", names(selectedData))
names(selectedData)<-gsub("^t", "Time", names(selectedData))
names(selectedData)<-gsub("^f", "Frequency", names(selectedData))
names(selectedData)<-gsub("tBody", "TimeBody", names(selectedData))
names(selectedData)<-gsub("-mean()", "Mean", names(selectedData), ignore.case = TRUE)
names(selectedData)<-gsub("-std()", "STD", names(selectedData), ignore.case = TRUE)
names(selectedData)<-gsub("-freq()", "Frequency", names(selectedData), ignore.case = TRUE)
names(selectedData)<-gsub("angle", "Angle", names(selectedData))
names(selectedData)<-gsub("gravity", "Gravity", names(selectedData))

tidyDatabase <- selectedData %>%
  group_by(subjectId, activity) %>%
  summarise_all(mean)
write.table(tidyDatabase, "tidyDatabase.txt", row.name=FALSE)
