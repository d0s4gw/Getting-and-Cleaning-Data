# Create the working directory
if (! file.exists("./data")) {
  dir.create("data")
}

# Collect and extract the data files
if (! file.exists("./data/UCI HAR Dataset")) {
  zipFile <- tempfile()
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileUrl, zipFile, mode="wb")
  unzip (zipfile = zipFile, exdir = "./data/")
}

# Read the data from files into tables
features     = read.table('./data/UCI HAR Dataset/features.txt',header=FALSE)
activityType = read.table('./data/UCI HAR Dataset/activity_labels.txt',header=FALSE)
subjectTrain = read.table('./data/UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain       = read.table('./data/UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain       = read.table('./data/UCI HAR Dataset/train/y_train.txt',header=FALSE)
subjectTest  = read.table('./data/UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest        = read.table('./data/UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest        = read.table('./data/UCI HAR Dataset/test/y_test.txt',header=FALSE)

# Assign column names
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"
colnames(subjectTest)   = "subjectId"
colnames(xTest)         = features[,2]
colnames(yTest)         = "activityId"

# bind the training and test datasets
trainingData = cbind(yTrain,subjectTrain,xTrain)
testData     = cbind(yTest,subjectTest,xTest)
finalData    = rbind(trainingData,testData)

# Create a vector for the column names
colNames  = colnames(finalData)

# Extract the means and standard deviations 
logicalVector = (grepl("activity..",colNames)
                 | grepl("subject..",colNames)
                 | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames)
                 | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
finalData = finalData[logicalVector==TRUE]

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)
colNames  = colnames(finalData)

# Clean up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(finalData) = colNames

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

# Summarize the finalDataNoActivityType table
tidyData = aggregate(finalDataNoActivityType[
  ,names(finalDataNoActivityType) != c('activityId','subjectId')]
  ,by=list(activityId=finalDataNoActivityType$activityId
    ,subjectId = finalDataNoActivityType$subjectId)
  ,mean)

# Merge the tidyData with activityType
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Export the tidy data
write.table(tidyData, './data/tidyData.txt',row.names=FALSE)