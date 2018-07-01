## Getting and Cleaning Data Course Project

# Here are the data for this project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# Set dirctory of Dataset 
dir_dat <-"~/UCI HAR Dataset"
## If your data location is different dirctory, please rewrite this as you can 

# Set Working Directory 
setwd(dir_dat)

########### 0. read Dataset  ########### 

# Create list of Dataset files with file path
filelist_Dataset <- list.files(dir_dat,          #path
                               pattern="txt",    #only text file
                               recursive=T,      #include sub folder
                               full.names = T)   #with full path

# Don't need "features_info" & "README" 
filelist_Dataset <- filelist_Dataset[-(3:4)]
# Don't need "Inertial Signals"
filelist_Dataset <- filelist_Dataset[c(-(3:11),-(15:23))]

# Create list of dataset
dat_list <- NULL
for (i in 1:length(filelist_Dataset)) {
        dat_list[[i]] <- read.table(filelist_Dataset[i])
}

# names of list  // creat filelist (without full path)
filelist_Dataset_names <- gsub(".txt","",
                                list.files(dir_dat,pattern="txt",recursive=T,full.names = F)
                          )
# Don't need "features_info" & "README" 
filelist_Dataset_names <- filelist_Dataset_names[-(3:4)]
# Don't need "Inertial Signals"
filelist_Dataset_names <- filelist_Dataset_names[c(-(3:11),-(15:23))]

# Set Data list name
names(dat_list) <- filelist_Dataset_names



########### 1. Merges the training and the test sets to create one data set  ###########


# load library "dplyr" & "tidyr"
library(dplyr)
library(tidyr)

# read test data 
dat_set_test <- dat_list$`test/X_test`
colnames(dat_set_test) <- dat_list$features[,2]

#  data frame of test data
dat_set_test <- data.frame(subject = dat_list$`test/subject_test`[,1],
                           type = "test",
                           label = dat_list$`test/y_test`[,1],
                           dat_set_test,
                           stringsAsFactors = FALSE)

#  tidy data of test data
dat2_set_test <- dat_set_test %>%
                 as_data_frame %>%
                 gather(key = features,value = value,-c(subject,type,label))

# read train data
dat_set_train <- dat_list$`train/X_train`
colnames(dat_set_train) <- dat_list$features[,2]

#  data frame of train data
dat_set_train <- data.frame(subject = dat_list$`train/subject_train`[,1],
                            type = "train",
                            label = dat_list$`train/y_train`[,1],
                            dat_set_train,
                            stringsAsFactors = FALSE)

#  tidy data of train data
dat2_set_train <- dat_set_train %>%
                  as_data_frame %>%
                  tidyr::gather(key = features,value = value,-c(subject,type,label))


# Merges the train and the test data 
dat_set_merge <- NULL
merge_by <- names(dat2_set_test)
dat_set_merge <- dplyr::full_join(dat2_set_test,dat2_set_train,by=merge_by) %>%
                 arrange(subject)

dat_set_merge

########### 2. Extracts only the measurements on the mean and standard deviation for each measurement  ###########

# filter by "mean" or "std" from merge dataset
dat_set_mean_std <- dat_set_merge %>%
        filter(grepl("mean|std",dat_set_merge$features))

dat_set_mean_std

########### 3. Uses descriptive activity names to name the activities in the data set  ###########

# as.factor $label from merge dataset
dat_set_merge$label <- factor(dat_set_merge$label)

# level is from "activity_labels"
levels(dat_set_merge$label) <- as.character(dat_list$activity_labels$V2)

# change as.facotr to as.character
dat_set_merge$label <- as.character(dat_set_merge$label)

dat_set_merge

########### 4. Appropriately labels the data set with descriptive variable names  ###########

# Set appropriately labels 
names(dat_set_merge) <- c("ID","train/test","activity","features","value")

dat_set_merge

########### 5. creates a second, independent tidy data set  ###########

# Create tidy data
dat_set_merge2 <- dat_set_merge %>%
                        group_by(ID,activity,features) %>%
                        summarize(Mean = mean(value))

dat_set_merge2

## write table 
write.table(dat_set_merge2,"tidydata.txt",row.name=FALSE)

