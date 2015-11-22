
## The test and train data files are merged after assigning them column names

setwd("C:/Users/gauta/Documents/R/data/UCI HAR Dataset")

# Read in the data from files
features     = read.table('./features.txt',header=FALSE); 
activity = read.table('./activity_labels.txt',header=FALSE); 

subTrain = read.table('./train/subject_train.txt',header=FALSE); 
xTrain       = read.table('./train/x_train.txt',header=FALSE); 
yTrain       = read.table('./train/y_train.txt',header=FALSE); 
subTest = read.table('./test/subject_test.txt',header=FALSE); 
xTest       = read.table('./test/x_test.txt',header=FALSE); 
yTest       = read.table('./test/y_test.txt',header=FALSE); 


# Assigin column names 
colnames(activity)  = c('activityId','activityType');
colnames(subTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
colnames(subTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# merging the data files to get the master dataset
trainingData = cbind(yTrain,subTrain,xTrain);
testData = cbind(yTest,subTest,xTest);
finalData = rbind(trainingData,testData);
colNames  = colnames(finalData); 

# # Then a logical vector is created to select only mean and stddev columns in the dataset

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | 
                   grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & 
                   !grepl("mean..-",colNames) | grepl("-std..",colNames) & 
                   !grepl("-std()..-",colNames));
finalData = finalData[logicalVector==TRUE];


# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 


## The variable column names are modified for better readability
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","_StdDev",colNames[i])
  colNames[i] = gsub("-mean","_Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time_",colNames[i])
  colNames[i] = gsub("^(f)","Freq_",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) = colNames;


# Calculates the averaages of the variables across each subject and activity
tidyData = aggregate(finalData2[,names(finalData2) != c('activityId','subjectId')],
                  by=list(activityId=finalData2$activityId,
                          subjectId = finalData2$subjectId),mean);

## Adds activity names to the data file by joining with the activity labels file
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidy_data.txt',row.names=TRUE,sep='\t');