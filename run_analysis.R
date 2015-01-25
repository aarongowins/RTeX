fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
if(!file.exists("data"))
{dir.create("data")}
download.file(fileURL,destfile="HW.zip",method="curl")
unzip("HW.zip",exdir="./data/HW")
#list.files()
test_data<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/test/X_test.txt")
train_data<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/train/X_train.txt")
merged_data<-rbind(test_data,train_data)
#dim(merged_data)
feat<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/features.txt")
mean_rows<-grep(c("mean()"),feat$V2,fixed=TRUE)
std_rows<-grep(c("std()"),feat$V2,fixed=TRUE)
mean_var_names<-feat[mean_rows,2]
std_var_names<-feat[std_rows,2]
#mean_var_names[1]
#std_var_names[1]
mean_cols<-merged_data[,mean_rows]
std_cols<-merged_data[,std_rows]
m<-as.character(mean_var_names)

s<-as.character(std_var_names)
names(std_cols)<-s
names(mean_cols)<-m

activity<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/activity_labels.txt")
y_train<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/train/y_train.txt")
y_test<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/test/y_test.txt")
merge_y<-rbind(y_test,y_train)
activities<-activity$V2[match(merge_y$V1,activity$V1)]
##std_cols$activity<-activities
##mean_cols$activity<-activities
subject_train<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/train/subject_train.txt")
subject_test<-read.table("/Users/aarongowins/data/HW/UCI HAR Dataset/test/subject_test.txt")
merge_subject<-rbind(subject_test,subject_train)
#std_cols$whose<-merge_subject
#mean_cols$whose<-merge_subject
all_data<-cbind(mean_cols,std_cols)
f<-factor(merge_subject[[1]])
all_means_by_subject<-sapply(split(all_data,f),colMeans)
ff<-factor(merge_y[[1]])
all_means_by_activity<-sapply(split(all_data,ff),colMeans)
n<-as.character(activity$V2)
colnames(all_means_by_activity)<-n
all_means<-cbind(all_means_by_activity,all_means_by_subject)
head(all_means)
write.table(all_means,file="Coursera.txt",row.name=TRUE)
