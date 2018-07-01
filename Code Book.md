# Code Book

# about Dataset

original source: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
The following files are available for the train and test data. Their descriptions are equivalent. 
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.

# about run_analysis.R

### 0. read Dataset
 - Create list of Dataset files with file path and read data

### 1. Merges the training and the test sets to create one data set.
 - Create a data frame of the test data & the train data
 - usinig the "tidyr" package , create the tidy data from the data frame
 - Merge the train and the test data 

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
 - usinig the "dplyr" package and filter function, extracts the mean and the standard deviation data

### 3. Uses descriptive activity names to name the activities in the data set
 - Set activity names to tidy data by "activity_labels.txt"
 
### 4. Appropriately labels the data set with descriptive activity names.
 - Set descriptive activity names
 
### 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
 - usinig group_by function of "dplyr",Create a data set of the average of each variable for each activity and each subject
