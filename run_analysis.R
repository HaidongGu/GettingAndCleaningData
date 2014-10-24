## load libraries
library(RCurl)
library(data.table)
library(dplyr)
library(tidyr)

## Function
## set/create working dir
my_setwd <- function(working_dir){
    setwd(working_dir)
    if (!file.exists(working_dir)) {
        dir.create(working_dir)
    }
    print(paste("working dir is", getwd()))
}

## Function
## Download and unzip File
download_unzip <- function(dest_zip_file){ 
    if (!file.exists(dest_zip_file)) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        print(paste("downloading", fileUrl));
        download.file(url=fileUrl, destfile= dest_zip_file, method="auto")
    } else {
        print(paste("local zip file found for", dest_zip_file, ", no need to download"))
    }
    
    dir_name <- "UCI HAR Dataset"
    if (!file.exists(dir_name)) {
        unzip(dest_zip_file)
    } else {
        print(paste("local directory exists for ", dir_name, "no need to unzip again"));
    }
    list.files(dir_name, recursive = TRUE)
    dir_name
}

## Function
## Read Text File as Table
read_table <- function(file_name, dir_name, sub_dir_name = NA){
    if (is.na(sub_dir_name)) {
        file_path = file.path(dir_name, file_name)
    } else {
        file_path = file.path(dir_name, sub_dir_name, file_name)
    }
    data.table(read.table(file_path))
}

## function
## extract feature with 
## use for labels the data set with descriptive variable names [Objective 4]
extract_feature <- function(features, my_greps, my_labels) {
    n <- length(my_greps)
    x <- matrix(seq(1, n), nrow = n)
    y <- NULL
    for (g in my_greps) {
        y <- cbind(y, grepl(g, features))
    }
    factor(y %*% x, labels = my_labels)
}

my_setwd("C:\\haidong\\datascience\\module03")
dir_name <- download_unzip("Assignment_Dataset.zip")


## read all data
dt_subject_train    <-   read_table("subject_train.txt", dir_name, "train")
dt_y_train          <-   read_table("Y_train.txt", dir_name, "train")
dt_x_train          <-   read_table("X_train.txt", dir_name, "train")

dt_subject_test     <-   read_table("subject_test.txt", dir_name, "test")
dt_y_test           <-   read_table("Y_test.txt", dir_name, "test") 
dt_x_test           <-   read_table("X_test.txt", dir_name, "test")

## merge data with rbind
dt_subject          <-   rbind(dt_subject_train, dt_subject_test)
dt_y                <-   rbind(dt_y_train, dt_y_test)
dt_x                <-   rbind(dt_x_train, dt_x_test)

rm(dt_subject_train, dt_y_train, dt_x_train, dt_subject_test, dt_y_test, dt_x_test)

## rename the columns and merge data with cbind
setnames(dt_subject, "V1", "subject")
setnames(dt_y, "V1", "activity_id")

dt_result          <-    cbind(dt_subject, dt_y, dt_x)

rm(dt_subject, dt_y, dt_x)

## ==> Objective 1: [DONE] Merges the training and the test sets to create one data set.


##  read features.txt, and extract rows for mean and standard deviation only
dt_features         <-      read_table("features.txt", dir_name)
setnames(dt_features, c("V1", "V2"), c("feature_id", "feature"))
dt_features         <-   dt_features[grepl("mean\\(\\)|std\\(\\)", feature), ]
dt_features         <-   mutate(dt_features, feature_vid=paste0("V", feature_id))

dt_result           <-   dt_result[, c("subject", "activity_id", dt_features$feature_vid), with = FALSE]

## ==> Objective 2: [DONE] Extracts only the measurements on the mean and standard deviation for each measurement.

dt_activity_labels  <-      read_table("activity_labels.txt", dir_name)
setnames(dt_activity_labels, c("V1", "V2"), c("activity_id", "activity"))

dt_result           <-      merge(dt_result, dt_activity_labels, by ='activity_id', all.x=TRUE)
dt_result           <-      dt_result[, c("subject", "activity", dt_features$feature_vid), with = FALSE]

## ==> Objective 3: [DONE] Uses descriptive activity names to name the activities in the data set.

dt_features <- mutate(dt_features, feature_domain=extract_feature(feature, c("^t", "^f"), c("Time", "Freq")))
dt_features <- mutate(dt_features, feature_instrument=extract_feature(feature, c("Acc", "Gyro"), c("Accelerometer", "Gyroscope")))
dt_features <- mutate(dt_features, feature_acceleration=extract_feature(feature, c("BodyAcc", "GravityAcc"), c(NA, "Body", "Gravity")))
dt_features <- mutate(dt_features, feature_variable=extract_feature(feature, c("mean()", "std()"), c("Mean", "STD")))
dt_features <- mutate(dt_features, feature_jerk=extract_feature(feature, c("Jerk"), c(NA, "Jerk")))
dt_features <- mutate(dt_features, feature_magnitude=extract_feature(feature, c("Mag"), c(NA, "Magnitude")))
dt_features <- mutate(dt_features, feature_axis=extract_feature(feature, c("-X", "-Y", "-Z"), c(NA, "X", "Y", "Z")))

dt_result <- gather(dt_result, feature_vid, value, -c(subject, activity))
dt_result <- group_by(dt_result, subject, activity, feature_vid)
dt_result <- summarise(dt_result, count=n(), average=mean(value))

dt_result <- merge(dt_result, dt_features, by = "feature_vid", all.x = TRUE)
dt_result <- dt_result[, c("subject", "activity", "feature_domain", "feature_instrument", 
                           "feature_acceleration", "feature_variable", "feature_jerk", "feature_magnitude",
                           "feature_axis", "count", "average"), with = FALSE]
dt_result <- arrange(dt_result, subject, activity, feature_domain, feature_instrument, feature_acceleration, 
                     feature_variable, feature_jerk, feature_magnitude, feature_axis, count, average)

## ==> Objective 4: [DONE]  Appropriately labels the data set with descriptive variable names.
## ==> Objective 5: [DONE]  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

write.table(dt_result, file="result_tidy_data.txt", row.name=FALSE)
