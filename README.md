# run_analysis.R
# Coursera Data Cleaning Week 4 Assignment
# Below is coding directly from R. I have included heaps of comments and trust that, along with the Codebook and Instruction List, the 
# reader will gain adequate insight into my rambling approach to completing this assignment.

## Coursera Data Week4 Assignment

## WD should be set to "Desktop", where I've established a folder "Week4 Assignment", 
## i.e., "C:/Users/Jeff/Desktop/Week4 Assignment"
setwd("C:/Users/Jeff/Desktop/Week4 Assignment")
library(dplyr)
library(tidyr)
library(R.utils)
library(data.table)
library(plyr)

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./Dataset.zip")
Sys.Date() #2016-04-02 as of test run

datasetunzip <- unzip("./Dataset.zip",list=TRUE)
str(datasetunzip)
View(datasetunzip)

## Zip.file has been downloaded and structure examined / viewed.  At this point:
## TO PRESERVE THE ORIGINAL DATA:  Go to the desktop / WD 'Assignment' folder and open "Dataset.zip"
## I WOULD DO THIS MANUALLY:
## COPY the "UCI HAR Dataset" folder but take no other action with it.
## "UCI HAR Dataset" contains all we need / more than we need.
## GO UP ONE LEVEL AND PASTE IT. THIS IS THE FILE I WILL WORK WITH.
## Both "Dataset.zip" and "UCI HAR Dataset" should be found in: "C:/Users/Jeff/Desktop/Week4 Assignment"
## The original "Dataset.zip" download will remain untouched as original source data downloaded 2016-04-02.

## Examining "UCI HAR Dataset":
## "ReadMe" and "features_info" text files provide details of methodology, sources of data, etc. 
## and are best opened in a text program.
## Not all files are of interest.  Two folders pertaining to "Inertial Signals" can be ignored.  
## They contain original 128 readings sourced from subject cell phones, from which the measurements that interst us were derived.

## See below for subject, test and train files of interest. The "features" and "features_info" files will also find use.

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
str(subject_test)  
distinct(subject_test) 
## 9 distinct test subjects by number, incl: 2,4,9,10,12,13,18,20,24. 

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
str(subject_train)  
distinct(subject_train)  
# 21 distinct, different subjects; along with the 9 in "test", this gives us 30.


## I'll now load and look at the X_test and X_train files.
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "")
str(X_test)    
## dim + 2947 x 561 variables

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "")
str(X_train)    
## dim + 7352 x 561 variables


## Now for the y_test and y_train files
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", sep = "")
str(y_test)  
## dim = 2947 x 1  
distinct(y_test)  
## 6 distinct values.  Looks like the activity, coded

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", sep = "")
str(y_train)  
## dim = 7352 x 1  
distinct(y_train)  
## 6 distinct values; activity by code

## Observations: 
## 1) d.f's contain no indexes,
## 2) colnames are "V1, V2 ..." so need replaced with descriptive names,
## 3) Implicit assumption: the test and train data are in the same row order as subject files and 'y' activity files.


## Let's combine 'subject', 'X' and 'y' files and call them "subjects", "activities" and "feature_vectors" (in keeping with ReadMe).
subjects <- bind_rows(subject_test, subject_train)
activities <- bind_rows(y_test, y_train)
feature_vectors <- bind_rows(X_test, X_train)


## set up file to decode progressively the activities and substitute descriptive text.
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
print(activity_labels)
## V1                 V2
##  1            WALKING
##  2   WALKING_UPSTAIRS
##  3 WALKING_DOWNSTAIRS
##  4            SITTING
##  5           STANDING
##  6             LAYING

activities1 <- data.table(gsub(1, "walking", activities$V1)) 
activities2 <- data.table(gsub(2, "walkingupstairs", activities1$V1)) 
activities3 <- data.table(gsub(3, "wakingdownstairs", activities2$V1))
activities4 <- data.table(gsub(4, "sitting", activities3$V1)) 
activities5 <- data.table(gsub(5, "standing", activities4$V1))
activities6 <- data.table(gsub(6, "laying", activities5$V1))

rm(activities, activities1, activities2, activities3, activities4, activities5, activity_labels) # Not needed

## Now add an index,  give proper colnames and merge 'subjects' and 'activities6' files
subjects <- mutate(subjects, index = seq_along(V1)) 
activities6 <- mutate(activities6, index = seq_along(V1)) 

colnames(subjects) <- c("subject", "index")
colnames(activities6) <- c("activity", "index")

subjects_activities <- merge(subjects, activities6)


## Look at the lables in "features". 
## It contains various computed stats (means, std dev, abs dev, etc.) mentioned in "features_info.txt".
features <- read.table("./UCI HAR Dataset/features.txt", sep = "")
str(features)  ## dim = 561 obs x 2 variables. 
## These entries correspond to the 561 colname variables (V1, V2, etc.) in 'feature_vectors'.
## Although not "tidy" in the sense of being all lower case, etc., I find this format more intelligible. 
## Use them as is to add the col names to 'feature_vectors'.
featuresV2<- c(as.character(features$V2))
feature_vectors <- setNames(feature_vectors, featuresV2)


## Extract variables relating to:  mean(): Mean value  and   std(): Standard deviation
## Can I get mean and std variables in 'feature_vectors' by matching in the colnames?  Yes.
feature_vectors_meanlabels <- data.frame(select_vars(colnames(feature_vectors), matches(".mean.")))
feature_vectors_stdlabels <- data.frame(select_vars(colnames(feature_vectors), matches(".std.")))

## From quick visual check looks like it selected appropriately, however recalling "feature_info.txt",  
## I don't believe the "Angle" vectors (e.g., "Angle(X, gravityMean")) should be included.
## Double check for "Angle" entries and select out.
feature_vectors_meanlabels_unwanted <- data.frame(select_vars(rownames(feature_vectors_meanlabels), matches("^Angle.")))

## NOTE: It is not clear to me whether other measurements, relating to the X/Y/Z dimensions, are required
## or simply measurements further derived from the X/Y/Z statistics, so I have extracted and included them.
## I would want to discuss this with the engineers and project designers to confirm. If not needed,
## they can more easily be eliminated than brought back in...

##   7 such measurements will be eliminated, afterwhich the 'unwanted' file will be removed.
feature_vectors_meanlabels <- slice(feature_vectors_meanlabels, -c(47:53))
rm(feature_vectors_meanlabels_unwanted)

## Retain the rownames as they will be needed in column selection
rownames(feature_vectors_meanlabels) <- feature_vectors_meanlabels[ ,1]

## Enforce a common colname on both 'label' files so they can be combined.
colnames(feature_vectors_meanlabels) <- c("mean_std_labels")
colnames(feature_vectors_stdlabels) <- c("mean_std_labels")

## Combine and use as a vector to select columns in 'feature_vectors'; then remove uneeded 'meanlabels' and 'stdlabels' files.
feature_vectors_mean_std_labels <- rbind(feature_vectors_meanlabels, feature_vectors_stdlabels)
rm(feature_vectors_meanlabels, feature_vectors_stdlabels)

feature_vectorsDT <- as.data.table(feature_vectors)
## Now to select columns from "feature_vectors" using the meanlabels and stdlabels  
feature_vectors_mean_std_cols <- feature_vectorsDT[,c(rownames(feature_vectors_mean_std_labels)), with=FALSE]


## Now combine 'subjects_activities' with 'feature_vectors_mean_std_cols' to get one single complete dataset.
feature_vectors_mean_std_cols <- mutate(feature_vectors_mean_std_cols, index=seq_len(10299))
one_data_set <- merge(subjects_activities, feature_vectors_mean_std_cols)
one_data_set <- as.data.table(one_data_set)
View(one_data_set)


##  5.	From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
tidy_data_set <- one_data_set

## Fist get rid of the index column
tidy_data_set <- tidy_data_set[,index:= NULL]

## Now group and summarize the data using ddply:
tidy_data_grouped_averaged <- ddply(tidy_data_set, .(subject, activity), colwise(mean))

## Let's also clean up a few files!!
rm(activities6, subjects, subjects_activities, subject_train, subject_test, X_test, X_train, y_test, y_train, features)

View(tidy_data_grouped_averaged)

write.table(tidy_data_grouped_averaged, file = "C:/Users/Jeff/Desktop/Week4 Assignment/tidy_data_grouped_averaged_saved", sep="\t", row.name=FALSE)

