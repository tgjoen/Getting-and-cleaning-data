##This program merges two datasets of activity measurements from mobile phones 
#Extracts mean and standard deviation for each measurement
#Labels the data with descriptive names
#Creates a  new datset with only mean values for each activity and individual (tidy set)

#if (!require("reshape2")) install.packages("reshape2")
#install.packages("reshape2")

#if (!require("data.table")) install.packages("data.table")
#install.packages("data.table")



#Read file with labels
act_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

#Read file with readings
readings<-read.table("./UCI HAR Dataset/features.txt")[,2]

#Extract mean and standard deviation

extract_mean_std<-grepl("mean|std", readings)

#Read files with data
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test_<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

#Assign names to the data using the readings table

names(x_test) = readings

#Extract mean and std from the test tables
x_test = x_test[,extract_mean_std]

#Assign the name of the readings to the data
y_test[,2] = act_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Type_of_Activity")
#Assign subject number to data
names(subject_test) = "subject"

#Combine test data
test_data<-cbind(as.data.table(subject_test), y_test, x_test)
                                                
#Include data from the other group (train) and label as before
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")

subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

names(x_train) = readings

#Extract mean and std frm the train tables
x_train = x_train[,extract_mean_std]

#Assign the name of the readings to the data
y_train[,2] = act_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Type_of_Activity")
#Assign subject number to data
names(subject_train) = "subject"

#Combine train data
train_data<-cbind(as.data.table(subject_train), y_train, x_train)

#Merge test and train data
data = rbind(test_data,train_data)

id_labels = c("subject", "Activity_ID", "Type_of_Activity")
data_labels = setdiff(colnames(data),id_labels)
all_data = melt(data, id = id_labels, measure.vars = data_labels)

#Use dcast to find the means
tidy_data = dcast(all_data, subject + Type_of_Activity~variable,mean)

write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE)

