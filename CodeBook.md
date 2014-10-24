CodeBook for the tidy dataset
=============================

Data source
-----------
This dataset is derived from the "Human Activity Recognition Using Smartphones Data Set" which was originally made avaiable here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Variable list and descriptions
------------------------------

Variable name        | Description                                | Range
---------------------|--------------------------------------------|----------
subject              | Subject ID who performed the activity.     | seq(1, 30)
activity             | Activity name                              | c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"]
feature_domain       | Feature: Time signal or frequency signal   | c("Time", "Freq")
feature_instrument   | Feature: Measuring instrument              | c("Accelerometer", "Gyroscope")
feature_acceleration | Feature: Acceleration signal               | c(NA, "Body", "Gravity")
feature_variable     | Feature: Variable                          | c("Mean", "STD")                                      
feature_jerk         | Feature: Jerk signal                       | c(NA, "Jerk")
feature_magnitude    | Feature: Magnitude of the signals          | c(NA, "Magnitude")
feature_axis         | Feature: 3-axial signals                   | c(NA, "X", "Y", "Z")
count                | total count                                |
average              | mean value                                 | [-1,1]
