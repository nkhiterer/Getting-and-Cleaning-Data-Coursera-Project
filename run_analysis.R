
#The purpose of this project to create one R script called run_analysis.R that does the following:

#1/Merges the training and the test sets to create one data set
#2/Extracts only the measurements on the mean and standard deviation for each measurement
#3/Uses descriptive activity names to name the activities in the data set
#4/Appropriately labels the data set with descriptive variable names
#5/From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
#------------------------------------------------------------------------------------------------------------------------------------------------
#1/Mergeging the training and the test sets to create one data set:
#READING FEATURES:
featurestest<-read.csv("test/X_test.txt", sep="",header=FALSE) 
featurestrain<-read.csv("train/X_train.txt", sep="",header=FALSE)
  
#READING LEVELS OF ACTIVITY:
activitylevels<-read.csv("activity_labels.txt", sep="",header=FALSE) 
  
#READING FEATURENAMES:
featurenames<-read.csv("features.txt", sep="",header=FALSE)
  
#READING ACTIVITIES:
activitiestest<-read.csv("test/Y_test.txt", sep="",header=FALSE) 
activitiestrain<-read.csv("train/Y_train.txt", sep="",header=FALSE)
  
#READING SUBJECTS
subjectstest<-read.csv("test/subject_test.txt", sep="",header=FALSE) 
subjectstrain<-read.csv("train/subject_train.txt", sep="",header=FALSE)
  
  
#MERGING FEATURES, ACTIVITIES, SUBJECTS TEST/TRAIN DATA:
mergedfeatures<-rbind(featurestest,featurestrain)
mergedactivities<-rbind(activitiestest,activitiestrain)
mergedsubjects<-rbind(subjectstest,subjectstrain)
  
#Assigning names to FEATURES, ACTIVITIES, SUBJECTS variables:
    
names(mergedfeatures)<-featurenames$V2
names(mergedsubjects)<-c("subject")
names(mergedactivities)<-c("activity")
  
#MERGING ACTIVITIES, FIATURES, SUBJECTS IN ALL COMBINED DATA 
bindedativitiesfeatures<-cbind(mergedactivities,mergedfeatures)
CompleteData<-cbind(mergedsubjects,bindedativitiesfeatures)
head(CompleteData,2) 

#2/Extracting only the measurements on the mean and standard deviation for each measurement:

#EXTRACTING MEANS AND STANDARD DEVIATIONS
meanstdfeatures<- featurenames$V2[grep("mean\\(\\)|std\\(\\)", featurenames $V2)]
#DATA EXTRACTED: Creating a Subset of DataComplete consistintg Standard Deviations and Means
extractednames<-c(as.character(meanstdfeatures), "subject", "activity")
DataExtracted<-subset(CompleteData, select = extractednames)

#3/Using descriptive activity names to name the activities in the data set:
#Converting "activities" to factor variable and assigning lebels
DataExtracted$activity<-factor(DataExtracted$activity,levels=c(1,2,3,4,5,6),
labels=c("WALKING","WALKINGUPSTAIRS","WALKINGDOWNSTAIRS","SITTING","STANDING","LAYING")) 
#CHECK:
head(DataExtracted,2)
 
#4/Appropriately labeling the data set with descriptive variable names:
#RE-LABELING THE DATAEXTRACTED WITH DESCRIPTIVE VARIABLE NAMES
names(DataExtracted)<-gsub("-mean()", "Mean", names (DataExtracted))
names(DataExtracted)<-gsub("-std()", "StDeviation", names (DataExtracted))
names(DataExtracted)<-gsub("^f", "frequency", names (DataExtracted))
names(DataExtracted)<-gsub("^t", "time", names (DataExtracted))
names(DataExtracted)<-gsub("BodyBody", "Body", names (DataExtracted))
names(DataExtracted)<-gsub("Gyro", "Gyroscope", names (DataExtracted))
names(DataExtracted)<-gsub("Acc", "Accelerometer", names (DataExtracted))
names(DataExtracted)<-gsub("Mag", "Magnitude", names (DataExtracted))
names(DataExtracted)<-gsub("()-", "", names (DataExtracted))
#CHECK
names(DataExtracted)

#5/From the data set in step 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject
#CREATING SECOND INDEPENDENT TIDY DATA SET 
 
library(plyr)
TidyDataset<-aggregate(.~activity+subject,DataExtracted, mean)
head(TidyDataset,2)
#CREATING OUTPUT .TXT FILES "tidydataset","CompleteData.txt"
write.table(TidyDataset, "tidydataset.txt",row.names=F)
 

 
