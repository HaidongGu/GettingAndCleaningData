Getting And Cleaning Data Course Project
======================

This is for the project work in classhttps://class.coursera.org/getdata-008/, the following paragraphs explain how run_analysis.R work.


Step 0: load libraries and crreate common functions
-------------

Load packages.

```r
library(RCurl)
library(data.table)
library(dplyr)
library(tidyr)
```

Common Function: set working dir
```r
my_setwd <- function(working_dir){
    setwd(working_dir)
    if (!file.exists(working_dir)) {
        dir.create(working_dir)
    }
    print(paste("working dir is", getwd()))
}
```

Common Function: Download and unzip File
```r
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
```

Common Function: Read Text File as Table
if there no sub_dir_name, means it is directly under the folder
```r
read_table <- function(file_name, dir_name, sub_dir_name = NA){
    if (is.na(sub_dir_name)) {
        file_path = file.path(dir_name, file_name)
    } else {
        file_path = file.path(dir_name, sub_dir_name, file_name)
    }
    data.table(read.table(file_path))
}
```

Common Function: Feature extraction
```r
extract_feature <- function(features, my_greps, my_labels) {
    n <- length(my_greps)
    x <- matrix(seq(1, n), nrow = n)
    y <- NULL
    for (g in my_greps) {
        y <- cbind(y, grepl(g, features))
    }
    factor(y %*% x, labels = my_labels)
}
```

this function is to extract more columns from existing long string, for example, 
```r
new_col <- extract_feature(feature, c("^t", "^f"), c("Time", "Freq"))) 
```
will get a new vector, new_col, which has the same size as feature, but if feature[i] starts with "t", new_col[i] will be "Time", if feature[i] starts with "f", new_col[i] will be "Freq". 

feature                 | new_col                                
------------------------|---------------
tBodyAcc-mean()-X       |   Time
tBodyAcc-std()-Z        |   Time
fBodyAccMag-std()       |   Freq


Step 1: Read Data
-------------
```r
## read all data from files
dt_subject_train    <-   read_table("subject_train.txt", dir_name, "train")
dt_y_train          <-   read_table("Y_train.txt", dir_name, "train")
dt_x_train          <-   read_table("X_train.txt", dir_name, "train")

dt_subject_test     <-   read_table("subject_test.txt", dir_name, "test")
dt_y_test           <-   read_table("Y_test.txt", dir_name, "test") 
dt_x_test           <-   read_table("X_test.txt", dir_name, "test")

## merge data (subject and test) with rbind
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
```

The data like this now

```r
tbl_df(dt_result)
Source: local data frame [10,299 x 563]

   subject activity_id        V1           V2         V3         V4         V5         V6         V7         V8         V9        V10
1        1           5 0.2885845 -0.020294171 -0.1329051 -0.9952786 -0.9831106 -0.9135264 -0.9951121 -0.9831846 -0.9235270 -0.9347238
2        1           5 0.2784188 -0.016410568 -0.1235202 -0.9982453 -0.9753002 -0.9603220 -0.9988072 -0.9749144 -0.9576862 -0.9430675
3        1           5 0.2796531 -0.019467156 -0.1134617 -0.9953796 -0.9671870 -0.9789440 -0.9965199 -0.9636684 -0.9774686 -0.9386916
4        1           5 0.2791739 -0.026200646 -0.1232826 -0.9960915 -0.9834027 -0.9906751 -0.9970995 -0.9827498 -0.9893025 -0.9386916
..     ...         ...       ...          ...        ...        ...        ...        ...        ...        ...        ...        ...
Variables not shown: V11 (dbl), V12 (dbl), V13 (dbl), V14 (dbl), V15 (dbl),..........V560 (dbl), V561 (dbl)
```


Step 2: Extract only the measurements on the mean and standard deviation for each measurement
-------------
read features.txt, and extract rows for mean and standard deviation only
```r
dt_features         <-      read_table("features.txt", dir_name)
setnames(dt_features, c("V1", "V2"), c("feature_id", "feature"))
dt_features         <-   dt_features[grepl("mean\\(\\)|std\\(\\)", feature), ]
dt_features         <-   mutate(dt_features, feature_vid=paste0("V", feature_id))
```

The feature like this now

```r
> tbl_df(dt_features)
Source: local data frame [66 x 3]

   feature_id                     feature feature_vid
1           1           tBodyAcc-mean()-X          V1
2           2           tBodyAcc-mean()-Y          V2
3           3           tBodyAcc-mean()-Z          V3
4           4            tBodyAcc-std()-X          V4
.....................
```

merge with main data set
```r
dt_result           <-   dt_result[, c("subject", "activity_id", dt_features$feature_vid), with = FALSE]

## ==> Objective 2: [DONE] Extracts only the measurements on the mean and standard deviation for each measurement.
```

The data like this now, (note that after "V6", it is "V41")

```r
> tbl_df(dt_result)
Source: local data frame [10,299 x 68]

   subject activity_id        V1           V2         V3         V4         V5         V6       V41        V42        V43        V44        V45
1        1           5 0.2885845 -0.020294171 -0.1329051 -0.9952786 -0.9831106 -0.9135264 0.9633961 -0.1408397 0.11537494 -0.9852497 -0.9817084
2        1           5 0.2784188 -0.016410568 -0.1235202 -0.9982453 -0.9753002 -0.9603220 0.9665611 -0.1415513 0.10937881 -0.9974113 -0.9894474
3        1           5 0.2796531 -0.019467156 -0.1134617 -0.9953796 -0.9671870 -0.9789440 0.9668781 -0.1420098 0.10188392 -0.9995740 -0.9928658
4        1           5 0.2791739 -0.026200646 -0.1232826 -0.9960915 -0.9834027 -0.9906751 0.9676152 -0.1439765 0.09985014 -0.9966456 -0.9813928
..     ...         ...       ...          ...        ...        ...        ...        ...       ...        ...        ...        ...        ...
Variables not shown: V46 (dbl), V81 (dbl), V82 (dbl), V83 (dbl), V84 (dbl), V85 (dbl), V86 (dbl), V121 (dbl), V122 (dbl), V123 (dbl), V124
  (dbl), V125 (dbl), V126 (dbl), V161 (dbl), V162 (dbl), V163 (dbl), V164 (dbl), V165 (dbl), V166 (dbl), V201 (dbl), V202 (dbl), V214 (dbl),
  V215 (dbl), V227 (dbl), V228 (dbl), V240 (dbl), V241 (dbl), V253 (dbl), V254 (dbl), V266 (dbl), V267 (dbl), V268 (dbl), V269 (dbl), V270
  (dbl), V271 (dbl), V345 (dbl), V346 (dbl), V347 (dbl), V348 (dbl), V349 (dbl), V350 (dbl), V424 (dbl), V425 (dbl), V426 (dbl), V427 (dbl),
  V428 (dbl), V429 (dbl), V503 (dbl), V504 (dbl), V516 (dbl), V517 (dbl), V529 (dbl), V530 (dbl), V542 (dbl), V543 (dbl)
```



Step 3: Replace activity_id with activity in the main data set
-------------
Merge the main dataset with dt_activity_labels, and re-select/order the columns
```r
dt_activity_labels  <-      read_table("activity_labels.txt", dir_name)
setnames(dt_activity_labels, c("V1", "V2"), c("activity_id", "activity"))

dt_result           <-      merge(dt_result, dt_activity_labels, by ='activity_id', all.x=TRUE)
dt_result           <-      dt_result[, c("subject", "activity", dt_features$feature_vid), with = FALSE]

## ==> Objective 3: [DONE] Uses descriptive activity names to name the activities in the data set.
```

data likes this, note that the second column is the activity name, instead of activity id now.
```r
> tbl_df(dt_result)
Source: local data frame [10,299 x 68]

   subject activity        V1           V2          V3         V4          V5         V6       V41        V42         V43        V44        V45
1        1  WALKING 0.2820216 -0.037696218 -0.13489730 -0.3282802 -0.13715339 -0.1890859 0.9453028 -0.2459414 -0.03216478 -0.9840476 -0.9289281
2        1  WALKING 0.2558408 -0.064550029 -0.09518634 -0.2292069  0.01650608 -0.2603109 0.9411130 -0.2520352 -0.03288345 -0.9839625 -0.9174993
3        1  WALKING 0.2548672  0.003814723 -0.12365809 -0.2751579  0.01307987 -0.2843713 0.9463639 -0.2642781 -0.02557507 -0.9628101 -0.9561309
4        1  WALKING 0.3433705 -0.014446221 -0.16737697 -0.2299235  0.17391077 -0.2133875 0.9524451 -0.2598379 -0.02613106 -0.9811001 -0.9643989
..     ...      ...       ...          ...         ...        ...         ...        ...       ...        ...         ...        ...        ...
Variables not shown: V46 (dbl), V81 (dbl), V82 (dbl), V83 (dbl), V84 (dbl), V85 (dbl), V86 (dbl), V121 (dbl), V122 (dbl), V123 (dbl), V124
  (dbl), V125 (dbl), V126 (dbl), V161 (dbl), V162 (dbl), V163 (dbl), V164 (dbl), V165 (dbl), V166 (dbl), V201 (dbl), V202 (dbl), V214 (dbl),
  V215 (dbl), V227 (dbl), V228 (dbl), V240 (dbl), V241 (dbl), V253 (dbl), V254 (dbl), V266 (dbl), V267 (dbl), V268 (dbl), V269 (dbl), V270
  (dbl), V271 (dbl), V345 (dbl), V346 (dbl), V347 (dbl), V348 (dbl), V349 (dbl), V350 (dbl), V424 (dbl), V425 (dbl), V426 (dbl), V427 (dbl),
  V428 (dbl), V429 (dbl), V503 (dbl), V504 (dbl), V516 (dbl), V517 (dbl), V529 (dbl), V530 (dbl), V542 (dbl), V543 (dbl)
```



Step 4: Expand Feature dataset
-------------

before expanding, The feature like this now

```r
> tbl_df(dt_features)
Source: local data frame [66 x 3]

   feature_id                     feature feature_vid
1           1           tBodyAcc-mean()-X          V1
2           2           tBodyAcc-mean()-Y          V2
3           3           tBodyAcc-mean()-Z          V3
4           4            tBodyAcc-std()-X          V4
.....................
```

use extract_feature to expand the feature dataset with more columns
```r
dt_features <- mutate(dt_features, feature_domain=extract_feature(feature, c("^t", "^f"), c("Time", "Freq")))
dt_features <- mutate(dt_features, feature_instrument=extract_feature(feature, c("Acc", "Gyro"), c("Accelerometer", "Gyroscope")))
dt_features <- mutate(dt_features, feature_acceleration=extract_feature(feature, c("BodyAcc", "GravityAcc"), c(NA, "Body", "Gravity")))
dt_features <- mutate(dt_features, feature_variable=extract_feature(feature, c("mean()", "std()"), c("Mean", "STD")))
dt_features <- mutate(dt_features, feature_jerk=extract_feature(feature, c("Jerk"), c(NA, "Jerk")))
dt_features <- mutate(dt_features, feature_magnitude=extract_feature(feature, c("Mag"), c(NA, "Magnitude")))
dt_features <- mutate(dt_features, feature_axis=extract_feature(feature, c("-X", "-Y", "-Z"), c(NA, "X", "Y", "Z")))
```

after expanding, The feature like this now

```r
> > tbl_df(dt_features)
Source: local data frame [66 x 10]

   feature_id                     feature feature_vid feature_domain feature_instrument feature_acceleration feature_variable feature_jerk
1           1           tBodyAcc-mean()-X          V1           Time      Accelerometer                 Body             Mean           NA
2           2           tBodyAcc-mean()-Y          V2           Time      Accelerometer                 Body             Mean           NA
3           3           tBodyAcc-mean()-Z          V3           Time      Accelerometer                 Body             Mean           NA
4           4            tBodyAcc-std()-X          V4           Time      Accelerometer                 Body              STD           NA
.....
Variables not shown: feature_magnitude (fctr), feature_axis (fctr)
```

Step 5: Resharp, Group and Summarise the data in the main data
-------------
```r
dt_result <- gather(dt_result, feature_vid, value, -c(subject, activity))
dt_result <- group_by(dt_result, subject, activity, feature_vid)
dt_result <- summarise(dt_result, count=n(), average=mean(value))
```

the dt_result looks like this
```r
> tbl_df(dt_result)
Source: local data frame [11,880 x 5]
   subject activity feature_vid count     average
1        1   LAYING          V1    50  0.22159824
2        1   LAYING          V2    50 -0.04051395
3        1   LAYING          V3    50 -0.11320355
4        1   LAYING          V4    50 -0.92805647
..     ...      ...         ...   ...         ...
```

Step 6: Merge main dataset with feature dataset and reset it
-------------
```r
t_result <- merge(dt_result, dt_features, by = "feature_vid", all.x = TRUE)
dt_result <- dt_result[, c("subject", "activity", "feature_domain", "feature_instrument", 
                           "feature_acceleration", "feature_variable", "feature_jerk", "feature_magnitude",
                           "feature_axis", "count", "average"), with = FALSE]
dt_result <- arrange(dt_result, subject, activity, feature_domain, feature_instrument, feature_acceleration, 
                     feature_variable, feature_jerk, feature_magnitude, feature_axis, count, average)

## ==> Objective 4: [DONE]  Appropriately labels the data set with descriptive variable names.
## ==> Objective 5: [DONE]  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
```
the dt_result looks like this
```r
> tbl_df(dt_result)
Source: local data frame [11,880 x 11]

   subject activity feature_domain feature_instrument feature_acceleration feature_variable feature_jerk feature_magnitude feature_axis count
1        1   LAYING           Time      Accelerometer                 Body             Mean           NA                NA            X    50
2        1   LAYING           Time      Accelerometer                 Body             Mean           NA                NA            Y    50
3        1   LAYING           Time      Accelerometer                 Body             Mean           NA                NA            Z    50
4        1   LAYING           Time      Accelerometer                 Body             Mean           NA         Magnitude           NA    50
..     ...      ...            ...                ...                  ...              ...          ...               ...          ...   ...
Variables not shown: average (dbl)
```


Step 7: Write to the file
-------------
```r
write.table(dt_result, file="result_tidy_data.txt", row.name=FALSE)
```
