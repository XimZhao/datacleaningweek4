##############################################################################

#

# FILE

#   run_analysis.R

#

# OVERVIEW

#   Using data collected from the accelerometers from the Samsung Galaxy S

#   smartphone, work with the data and make a clean data set, outputting the

#   resulting tidy data to a file named "tidy_data.txt".

#   See README.md for details.

#



library(dplyr)





##############################################################################

# STEP 0B - Read data

##############################################################################









# read training data

trainingSubjects <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt')

trainingValues <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt')

trainingActivity <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt')



# read test data

testSubjects <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt')

testValues <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt')

testActivity <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt')



# read features, don't convert text labels to factors

features <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt', as.is = TRUE)

## note: feature names (in features[, 2]) are not unique

##       e.g. fBodyAcc-bandsEnergy()-9,16



# read activity labels

activities <- read.table('C:/Users/u553cr/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt')

colnames(activities) <- c("activityId", "activityLabel")





##############################################################################

# Step 1 - Merge the training and the test sets to create one data set

##############################################################################



# concatenate individual data tables to make single data table

train_test <- rbind(
  
  cbind(trainingSubjects, trainingValues, trainingActivity),
  
  cbind(testSubjects, testValues, testActivity)
  
)



# remove individual data tables to save memory

rm(trainingSubjects, trainingValues, trainingActivity,
   
   testSubjects, testValues, testActivity)



# assign column names

colnames(train_test) <- c("subject", features[, 2], "activity")





##############################################################################

# Step 2 - Extract only the measurements on the mean and standard deviation

#          for each measurement

##############################################################################



# determine columns of data set to keep based on column name...

columnsToKeep <- grepl("subject|activity|mean|std", colnames(train_test))



# ... and keep data in these columns only

train_test <- train_test[, columnsToKeep]





##############################################################################

# Step 3 - Use descriptive activity names to name the activities in the data

#          set

##############################################################################



# replace activity values with named factor levels

train_test$activity <- factor(train_test$activity,
                              
                              levels = activities[, 1], labels = activities[, 2])





##############################################################################

# Step 4 - Appropriately label the data set with descriptive variable names

##############################################################################



# get column names

traintestCols <- colnames(train_test)



# remove special characters

traintestCols <- gsub("[\\(\\)-]", "", traintestCols)



# expand abbreviations and clean up names

traintestCols <- gsub("^f", "frequencyDomain", traintestCols)

traintestCols <- gsub("^t", "timeDomain", traintestCols)

traintestCols <- gsub("Acc", "Accelerometer", traintestCols)

traintestCols <- gsub("Gyro", "Gyroscope", traintestCols)

traintestCols <- gsub("Mag", "Magnitude", traintestCols)

traintestCols <- gsub("Freq", "Frequency", traintestCols)

traintestCols <- gsub("mean", "Mean", traintestCols)

traintestCols <- gsub("std", "StandardDeviation", traintestCols)



# correct typo

traintestCols <- gsub("BodyBody", "Body", traintestCols)



# use new labels as column names

colnames(train_test) <- traintestCols





##############################################################################

# Step 5 - Create a second, independent tidy set with the average of each

#          variable for each activity and each subject

##############################################################################



# group by subject and activity and summarise using mean

traintestMeans <- train_test %>%
  
  group_by(subject, activity) %>%
  
  summarise_each(funs(mean))



# output to file "tidy_data.txt"

write.table(traintestMeans, "tidy_data.txt", row.names = FALSE,
            
            quote = FALSE)