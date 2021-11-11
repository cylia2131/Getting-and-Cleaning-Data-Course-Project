install.packages("dplyr")
install.packages("data.table")
library(dplyr)
library(data.table)
getwd()

## Downloading the data and preparing the data
## Use the directory "./Documents' as working directory

dwnl <- " https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCI HAR Dataset.zip"
download.file(dwnl , zipfile)

if(file.exists(zipfile)) unzip(zipfile)


## Files are downloaded and the following files exist
base <- "UCI HAR Dataset"

activitylabelsfile <- paste(base, "activity_labels.txt", sep="/")
featuresfile <- paste(base, "features.txt",  sep = "/")

testvariablesfile <- paste(base, "test/X_test.txt", sep="/")
testactivityfile <- paste(base, "test/y_test.txt", sep="/")
testsubjectfile <- paste(base, "test/subject_test.txt", sep="/")


trainvariablesfile <- paste(base, "train/X_train.txt", sep="/")
trainactivityfile <- paste(base, "train/y_train.txt", sep="/")
trainsubjectfile <- paste(base, "train/subject_train.txt", sep="/")

neededfiles <- c(activitylabelsfile ,
                 featuresfile,
                 testvariablesfile,
                 testactivityfile,
                 testsubjectfile,
                 trainvariablesfile,
                 trainactivityfile,
                 trainsubjectfile
                 )

sapply(neededfiles, function(f) if(!file.exists(f)) 
stop(paste("Needed file ", f, " doesn't exist. Exitting ...", sep="")))


## Read featuresfile
features <- read.table(featuresfile, col.names=c("rownumber","variablename"))



## Fix the issue with duplicate names (e.g.) 540. fBodyBodyGyroMag-skewness()
allvariables <- mutate(features, variablename = gsub("BodyBody", "Body", variablename))



## Filter the 66 variables - mean() and std()
neededvariables <- filter(allvariables, grepl("mean\\(\\)|std\\(\\)", variablename))



## Make the allvariables readable
## Remove special characters, Convert to lower case
allvariables <- mutate(allvariables, variablename = gsub("-", "", variablename),
                                     variablename = gsub("\\(", "", variablename),
                                     variablename = gsub("\\)", "", variablename),
                                     variablename = tolower(variablename))




## Make the neededvariables readable
##    Remove special characters, Convert to lower case
neededvariables <- mutate(neededvariables, variablename = gsub("-", "", variablename),
                                           variablename = gsub("\\(", "", variablename),
                                           variablename = gsub("\\)", "", variablename),
                                           variablename = tolower(variablename))


## Read activitylabelsfile
activitylabels <- read.table(activitylabelsfile, col.names=c("activity", "activitydescription"))



## Read in test data stats
testvalues <- read.table(testvariablesfile, col.names = allvariables$variablename)
testneededvalues <- testvalues[ , neededvariables$variablename]



## Read in test activities
testactivities <- read.table(testactivityfile, col.names=c("activity"))



## Read in test subjects
testsubjects <- read.table(testsubjectfile, col.names=c("subject"))



## Add a readable activity description
testactivitieswithdescr <- merge(testactivities, activitylabels)



## Put the test data together
## Assuming that the data is in the same order and all we need is cbind
## Combining values, activities, subjects
testdata <- cbind(testactivitieswithdescr, testsubjects, testneededvalues)



## Read in train variables
trainvalues <- read.table(trainvariablesfile, col.names = allvariables$variablename)
trainneededvalues <- trainvalues[ , neededvariables$variablename]



## Read in train activities
trainactivities <- read.table(trainactivityfile, col.names=c("activity"))



## Read in train subjects
trainsubjects <- read.table(trainsubjectfile, col.names=c("subject"))



## Add a readable activity description
trainactivitieswithdescr <- merge(trainactivities, activitylabels)



## Put the train data together
## Assuming that the data is in the same order and all we need is cbind
## Combining values, activities, subjects
traindata <- cbind(trainactivitieswithdescr, trainsubjects, trainneededvalues)



## Combine the testdata and traindata
## Additionally make subject a factor
alldata <- rbind(testdata, traindata) %>% select( -activity )
alldata <- mutate(alldata, subject = as.factor(alldata$subject))





## Write the data out
write.table(alldata, "Mean_And_StdDev_For_Activity_Subject.txt")





## Create a second, independent tidy data set with the average of each variable for each activity and each subject.
## Group the data by activity, subject
allgroupeddata <- group_by(alldata,activitydescription,subject)

## Get the average of each variable
summariseddata <- summarise_each(allgroupeddata, funs(mean))

## Write the data out
write.table(summariseddata , file = "Tidy.txt", row.names = FALSE)