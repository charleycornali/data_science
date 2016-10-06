#   Function Description:
#       1. Merges the training and the test sets to create one data set.
#       2. Extracts only the measurements on the mean and standard deviation for each measurement.
#       3. Uses descriptive activity names to name the activities in the data set
#       4. Appropriately labels the data set with descriptive variable names.
#       5. From the data set in step 4, creates a second, independent tidy data set with the average of 
#           each variable for each activity and each subject.

library(data.table)

#   Clean up workspace
rm(list=ls())
    
dataset_dir <- "UCI HAR Dataset"
file_name <- "uci_dataset.zip"
    
#   Download and unzip the dataset:
if (!file.exists(file_name)){
    
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                  file_name, 
                  method = "curl")
}

if (!file.exists(dataset_dir)) { 
        
    unzip(file_name) 
}
    
#   Set the working directory to the folder where all the txt files are located
setwd(file.path(getwd(), dataset_dir))
    
#   Load activity labels and features txt files 
#   Imports activity_labels.txt and creates a activity_label table
activity_labels <- read.table('./activity_labels.txt')
activity_labels[, 2] <- as.character(activity_labels[, 2])
    
#   Read the activity files and combine them into one data.frame
activity_data <- rbind(read.table('./test/Y_test.txt', header = FALSE), 
                       read.table('./train/Y_train.txt', header = FALSE))

activity_data$V1 <- activity_labels$V2[activity_data$V1]

#   Reads the subject files and merges into one data.frame
subject_data <- rbind(read.table('./test/subject_test.txt', header = FALSE), 
                      read.table('./train/subject_train.txt', header = FALSE))

#   Imports features.txt and creates a features table
features <- read.table('./features.txt')
features[, 2] <- as.character(features[, 2])

#   Extract only the data on mean and standard deviation from the features.txt
features_desired <- grep("mean\\(\\)|std\\(\\)", features[, 2])
features_desired.names <- features[features_desired, 2]

#   Cleaning up the variable names
#   Strip the () and - characters from the features
features_desired.names <- gsub('-', '_', features_desired.names)
features_desired.names <- gsub('[()]', '', features_desired.names)

#   Change the capitalization for the features to the desired format
features_desired.names <- gsub('mean', 'Mean', features_desired.names)
features_desired.names <- gsub('std', 'Std', features_desired.names)

#   Appropriately labels the data set with descriptive variable names
features_desired.names <- gsub("^t", "time_", features_desired.names)
features_desired.names <- gsub("^f", "frequency_", features_desired.names)
features_desired.names <- gsub("Acc", "Accelerometer", features_desired.names)
features_desired.names <- gsub("Gyro", "Gyroscope", features_desired.names)
features_desired.names <- gsub("Mag", "Magnitude", features_desired.names)
features_desired.names <- gsub("BodyBody", "Body_", features_desired.names)

#   Read fearures files and merges into one data.frame
#   only gets the rows with the desired features
features_data <- rbind(read.table('./test/X_test.txt', header = FALSE)[features_desired],
                       read.table('./train/X_train.txt', header = FALSE)[features_desired])

#   Comines all the columns from the test, train, activity and subject data into one data.frame
combined_df <- cbind(subject_data, activity_data, features_data)
#   Sets the column names to the names to subject, activity_id, and the features_desired variable names
colnames(combined_df) <- c("subject_id", "activity", features_desired.names)

#   Making tidy data
tidy_data <- aggregate(. ~subject_id + activity, combined_df, mean)
tidy_data <- tidy_data[order(tidy_data$subject_id, tidy_data$activity), ]

#   Write tidy_data to txt file
write.table(tidy_data, './tidy_data.txt', row.names = TRUE, sep = '\t')    
