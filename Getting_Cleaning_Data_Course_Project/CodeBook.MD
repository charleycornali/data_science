#CodeBook
###Description

This code book describes the variables, the data, and any transformations or work was performed to clean up the data inital data.
The data used for this project was collected from the accelerometers from the Samsung Galaxy S smartphone.

A full description of the data is available at the site where the data was obtained:
[The UCI Machine Learning Repository]
(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
The source data for this project can be found here:
[Human Activity Recognition Using Smartphones Data Set]
(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

###Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each subject performed various activites while wearing a smartphone (Samsung Galaxy S II) on the waist. 

######These activities included:
    - WALKING (value 1): subject was walking during the test
    - WALKING_UPSTAIRS (value 2): subject was walking up a staircase during the test
    - WALKING_DOWNSTAIRS (value 3): subject was walking down a staircase during the test
    - SITTING (value 4): subject was sitting during the test
    - STANDING (value 5): subject was standing during the test
    - LAYING (value 6): subject was laying down during the test

Using the embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz were captured. 
The data set has been randomly partitioned into two sets (test and train), where 70% of the subjects were place in the train group.

The accelerometer and gyroscope sensor signals were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). 
The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. 
The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. 
From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

######Attribute Information for each dataset record:
    - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
    - Triaxial Angular velocity from the gyroscope. 
    - A 561-feature vector with time and frequency domain variables. 
    - Its activity label. 
    - An identifier of the subject who carried out the experiment.

####Measurements

###Running the Analysis

The following are a list of the variable names and transformations used to created the combined dataframe to clean the dataset up...
- **activity_labels** is the dataframe that contains the activity id and label
- **activity_data** is the dataframe that contains both the test and train rows from their respective files
  - the activity_id is instead re-assigned to the actual label from the actitivty_labels data frame (i.e.- the integer 6 is "LAYING")
- **subjecy_data** is the dataframe that contains both the test and train rows from their respective files
- **features** is the dataframe that is the feature id and the name of the measurement
  - **features_desired** is features that only match the data on mean and standard deviation from the features.txt using the grep function
    - additionally, these variable names have been renamed to be more descriptive
- **features_data** is the dataframe that contains both the test and train rows with the desired features from their respective files
- **combined_df** is the dataframe combined with cbind that is comprised of the subject_id, activity label, and the desired features
  - the column names have been renamed to the appropriate values
- **tidy_data** is the independent dataset that has contains the average of each feature variables for each of the activities for each subject
  
#####Phase 1: Merge the training and the test sets to create one data set
#####Phase 2: Extracts only the measurements on the mean and standard deviation for each measurement
#####Phase 3: Uses descriptive activity names to name the activities in the data set
#####Phase 4: Appropriately labels the data set with descriptive variable names
#####Phase 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject using the aggregate function
