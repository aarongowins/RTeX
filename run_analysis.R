## Load the data file URL
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
## Create a directory named data and download and unzip the file
if(!file.exists("data"))
{dir.create("data")}
download.file(fileURL,destfile="HW.zip",method="curl")
unzip("HW.zip",exdir="./data/HW")
#list.files()  ## <-- Uncomment to see if the files are correct

## Read the files into data frames 
test_data<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/test/X_test.txt")
train_data<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/train/X_train.txt")

## 1. Concatenate the training and the test sets to create one data set.:
merged_data<-rbind(test_data,train_data)
#dim(merged_data) ## <-- uncomment to see if you like the data frame
feat<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/features.txt")

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.:
## Find those elements:
mean_rows<-grep(c("mean()"),feat$V2,fixed=TRUE)
std_rows<-grep(c("std()"),feat$V2,fixed=TRUE)
## Get the names:
mean_var_names<-feat[mean_rows,2]
std_var_names<-feat[std_rows,2]
#mean_var_names[1] ## <-- Uncomment to see if you are getting the names with "mean()"
#std_var_names[1]
## Extract those elements into df's called mean_cols/std_cols:
mean_cols<-merged_data[,mean_rows]
std_cols<-merged_data[,std_rows]
## Change type for names function:
m<-as.character(mean_var_names)
s<-as.character(std_var_names)
##  4. Appropriately labels the data set with descriptive variable names.:
names(std_cols)<-s
names(mean_cols)<-m

## Read in the activity data key:
activity<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/activity_labels.txt")
## Read in and concatenate the activity data:
y_train<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/train/y_train.txt")
y_test<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/test/y_test.txt")
merge_y<-rbind(y_test,y_train)
activities<-activity$V2[match(merge_y$V1,activity$V1)]

## Read in and concatenate the subject data:
subject_train<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/train/subject_train.txt")
subject_test<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/test/subject_test.txt")
merge_subject<-rbind(subject_test,subject_train)

## Concatenate the mean and std dataframes (finally)
all_data<-cbind(mean_cols,std_cols)

## Create a factor from the subject data to use with sapply and find the means by subject:
f<-factor(merge_subject[[1]])
all_means_by_subject<-sapply(split(all_data,f),colMeans)

## Create a factor from the activity data to use with sapply and find the means by activity:
ff<-factor(merge_y[[1]])
all_means_by_activity<-sapply(split(all_data,ff),colMeans)

## Type work for colnams function
n<-as.character(activity$V2)
## 3. Uses descriptive activity names to name the activities in the data set.:
colnames(all_means_by_activity)<-n
## Concatenate the subject means and activity means
all_means<-cbind(all_means_by_activity,all_means_by_subject)
t(all_means)
## 5. From the data set in step 4, creates a second, 
## independent tidy data set with the average of each variable for each activity and each subject.:
write.table(all_means,file="Coursera.txt",row.name=TRUE)
