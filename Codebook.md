==================================================================

## Code Book

Human Activity Recognition Using Smartphones Dataset
Version 1.0

==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities *__(WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)__* wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

## For each record it is provided:


- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## The dataset includes the following files:

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

### Notes: 
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws


The tidy data with aggregates calculated for the above data set is created with the following:

The following acronyms are replaced in the coloumns generated

__Acc__ replaced with __Accelerometer__

__Gyro__  replaced with __Gyroscope__

__BodyBody__  replaced with Body

__Mag__ replaced with __Magnitude__

Character __f__ replaced with __Frequency__

Character __t__  replaced with __Time__

### Column Names: 

 [1] "Subject"                                     
 [2] "Activity"                                    
 [3] "timeBodyAccelerometer-mean-X"                
 [4] "timeBodyAccelerometer-mean-Y"                
 [5] "timeBodyAccelerometer-mean-Z"                
 [6] "timeBodyAccelerometer-std-X"                 
 [7] "timeBodyAccelerometer-std-Y"                 
 [8] "timeBodyAccelerometer-std-Z"                 
 [9] "timeGravityAccelerometer-mean-X"             
[10] "timeGravityAccelerometer-mean-Y"             
[11] "timeGravityAccelerometer-mean-Z"             
[12] "timeGravityAccelerometer-std-X"              
[13] "timeGravityAccelerometer-std-Y"              
[14] "timeGravityAccelerometer-std-Z"              
[15] "timeBodyAccelerometerJerk-mean-X"            
[16] "timeBodyAccelerometerJerk-mean-Y"            
[17] "timeBodyAccelerometerJerk-mean-Z"            
[18] "timeBodyAccelerometerJerk-std-X"             
[19] "timeBodyAccelerometerJerk-std-Y"             
[20] "timeBodyAccelerometerJerk-std-Z"             
[21] "timeBodyGyroscope-mean-X"                    
[22] "timeBodyGyroscope-mean-Y"                    
[23] "timeBodyGyroscope-mean-Z"                    
[24] "timeBodyGyroscope-std-X"                     
[25] "timeBodyGyroscope-std-Y"                     
[26] "timeBodyGyroscope-std-Z"                     
[27] "timeBodyGyroscopeJerk-mean-X"                
[28] "timeBodyGyroscopeJerk-mean-Y"                
[29] "timeBodyGyroscopeJerk-mean-Z"                
[30] "timeBodyGyroscopeJerk-std-X"                 
[31] "timeBodyGyroscopeJerk-std-Y"                 
[32] "timeBodyGyroscopeJerk-std-Z"                 
[33] "timeBodyAccelerometerMagnitude-mean"         
[34] "timeBodyAccelerometerMagnitude-std"          
[35] "timeGravityAccelerometerMagnitude-mean"      
[36] "timeGravityAccelerometerMagnitude-std"       
[37] "timeBodyAccelerometerJerkMagnitude-mean"     
[38] "timeBodyAccelerometerJerkMagnitude-std"      
[39] "timeBodyGyroscopeMagnitude-mean"             
[40] "timeBodyGyroscopeMagnitude-std"              
[41] "timeBodyGyroscopeJerkMagnitude-mean"         
[42] "timeBodyGyroscopeJerkMagnitude-std"          
[43] "frequencyBodyAccelerometer-mean-X"           
[44] "frequencyBodyAccelerometer-mean-Y"           
[45] "frequencyBodyAccelerometer-mean-Z"           
[46] "frequencyBodyAccelerometer-std-X"            
[47] "frequencyBodyAccelerometer-std-Y"            
[48] "frequencyBodyAccelerometer-std-Z"            
[49] "frequencyBodyAccelerometerJerk-mean-X"       
[50] "frequencyBodyAccelerometerJerk-mean-Y"       
[51] "frequencyBodyAccelerometerJerk-mean-Z"       
[52] "frequencyBodyAccelerometerJerk-std-X"        
[53] "frequencyBodyAccelerometerJerk-std-Y"        
[54] "frequencyBodyAccelerometerJerk-std-Z"        
[55] "frequencyBodyGyroscope-mean-X"               
[56] "frequencyBodyGyroscope-mean-Y"               
[57] "frequencyBodyGyroscope-mean-Z"               
[58] "frequencyBodyGyroscope-std-X"                
[59] "frequencyBodyGyroscope-std-Y"                
[60] "frequencyBodyGyroscope-std-Z"                
[61] "frequencyBodyAccelerometerMagnitude-mean"    
[62] "frequencyBodyAccelerometerMagnitude-std"     
[63] "frequencyBodyAccelerometerJerkMagnitude-mean"
[64] "frequencyBodyAccelerometerJerkMagnitude-std" 
[65] "frequencyBodyGyroscopeMagnitude-mean"        
[66] "frequencyBodyGyroscopeMagnitude-std"         
[67] "frequencyBodyGyroscopeJerkMagnitude-mean"    
[68] "frequencyBodyGyroscopeJerkMagnitude-std"  


## License:
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
