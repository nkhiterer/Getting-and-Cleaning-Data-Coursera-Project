---
title: "Codebook.md"
output:
  html_document:
    toc: true
    toc_depth: 4
    toc_float: true
    code_folding: 'hide'
    self_contained: true
  pdf_document:
    toc: yes
    toc_depth: 4
    latex_engine: xelatex
---
The purpose of this project to create one R script called run_analysis.R that does the following:
```

1/Merges the training and the test sets to create one data set

2/Extracts only the measurements on the mean and standard deviation for each measurement

3/Uses descriptive activity names to name the activities in the data set

4/Appropriately labels the data set with descriptive variable names

5/From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

6/Create a Codebook.md for the project
```

```{r setup}
knitr::opts_chunk$set(
  warning = TRUE, # show warnings during codebook generation
  message = TRUE, # show messages during codebook generation
  error = TRUE, # do not interrupt codebook generation in case of errors,
                # usually better for debugging
  echo = TRUE  # show R code
)
ggplot2::theme_set(ggplot2::theme_bw())
pander::panderOptions("table.split.table", Inf)
```

Source  for the data used: 
```
Human Activity Recognition Using Smartphones Dataset
Version 1.0
 
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Data were downloaded from:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

```
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

     

                      run_analysis.R

                     

1/Merging the training and the test sets to create one data set:
```
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
#READING SUBJECTS:
subjectstest<-read.csv("test/subject_test.txt", sep="",header=FALSE) 
subjectstrain<-read.csv("train/subject_train.txt", sep="",header=FALSE)

#MERGING FEATURES, ACTIVITIES, SUBJECTS TEST/TRAIN DATA:

mergedfeatures<-rbind(featurestest,featurestrain)
mergedactivities<-rbind(activitiestest,activitiestrain)
mergedsubjects<-rbind(subjectstest,subjectstrain)
  
Assigning names to FEATURES, ACTIVITIES, SUBJECTS variables:
names(mergedfeatures)<-featurenames$V2
names(mergedsubjects)<-c("subject")
names(mergedactivities)<-c("activity")
  
#MERGING ACTIVITIES, FIATURES, SUBJECTS IN ALL COMBINED DATA 
bindedativitiesfeatures<-cbind(mergedactivities,mergedfeatures)
CompleteData<-cbind(mergedsubjects,bindedativitiesfeatures)
summary(CompleteData) 
```

2/Extracting only the measurements on the mean and standard deviation for each measurement:
```
#EXTRACTING MEANS AND STANDARD DEVIATIONS
meanstdfeatures<- featurenames$V2[grep("mean\\(\\)|std\\(\\)", featurenames $V2)]
#DATA EXTRACTED: Creating a Subset of DataComplete consistintg Standard Deviations and Means
extractednames<-c(as.character(meanstdfeatures), "subject", "activity")
DataExtracted<-subset(CompleteData, select = extractednames)
```
3/Using descriptive activity names to name the activities in the data set:


```
#Converting "activities" to factor variable and assigning lebels
DataExtracted$activity<-factor(DataExtracted$activity,levels=c(1,2,3,4,5,6),
labels=c("WALKING","WALKINGUPSTAIRS","WALKINGDOWNSTAIRS","SITTING","STANDING","LAYING")) 
#CHECK:
summary(DataExtracted)
```
4/Appropriately labeling the data set with descriptive variable names:

```
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
```
5/From the data set in step 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject

```
#CREATING SECOND INDEPENDENT TIDY DATA SET 
 
library(plyr)
TidyDataset<-aggregate(.~activity+subject,DataExtracted, mean)
#CREATING OUTPUT .TXT FILES "tidydataset","CompleteData.txt"
write.table(TidyDataset, "tidydataset.txt",row.names=F)
summary(TidyDataset)
```
 
 

```{r prepare_codebook}
library(codebook)
codebook_data<- rio::import("tidydataset.txt")
 
# for CSV: codebook_data <- rio::import("mydata.csv")

# omit the following lines, if your missing values are already properly labelled
codebook_data <- detect_missing(codebook_data,
    only_labelled = TRUE, # only labelled values are autodetected as
                                   # missing
    negative_values_are_missing = FALSE, # negative values are missing values
    ninety_nine_problems = TRUE,   # 99/999 are missing values, if they
                                   # are more than 5 MAD from the median
    )
 
codebook_data <- detect_scales(codebook_data)
```


```Description of data used(citing from README.TXT):
"Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. "

For each record in the original dataset it was  provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The original dataset included the following files:

- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

Use of the original dataset in publications must be acknowledged by referencing the following publication:

Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
 
```
Data used for the purpose of the project "run_analysis.R" were:

- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.


```


To generate this automated codebook, the article was used:
```
How to automatically document data with the codebook package to facilitate
data re-use forthcoming in Advances in Methods and Practices in Psychological Science
Author: Ruben C. Arslan
Center for Adaptive Rationality, Max Planck Institute for Human Development, Berlin ruben.arslan@gmail.com 
```
 


```{r codebook}
codebook(codebook_data)
```
