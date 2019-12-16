library(data.table)
library(dplyr)

#getting and unzipping file

filename <- "Benmarksons.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

setwd("~/UCI HAR Dataset")

#Reading metadata

featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE)

#Reading the different training data
subjectTrain <- read.table(".\\train\\subject_train.txt", header = FALSE)
activityTrain <- read.table(".\\train\\y_train.txt", header = FALSE)
featuresTrain <- read.table(".\\train\\X_train.txt", header = FALSE)

#Reading Test Data
subjectTest <- read.table(".\\test\\subject_test.txt", header = FALSE)
activityTest <- read.table(".\\test\\y_test.txt", header = FALSE)
featuresTest <- read.table(".\\test\\X_test.txt", header = FALSE)

#Merging of training data and test data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)


colnames(features) <- t(featureNames[2])


colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)


requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
## [1] 10299   563


extractedData <- completeData[,requiredColumns]
dim(extractedData)
## [1] 10299    88


extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}



extractedData$Activity <- as.factor(extractedData$Activity)




names(extractedData)




names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))




names(extractedData)



extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)


tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)



