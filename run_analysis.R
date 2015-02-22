library(dplyr);
library(stringr);

## Set the folder of the UCI HAR Dataset as working Directory
x_test <- read.table("test/X_test.txt"); 

y_test <- read.table("test/y_test.txt");
y_test <- y_test$V1;
y_test <- as.factor(y_test); ## obtain activity description for test folder

subject_test <- read.table("test/subject_test.txt");
subject_test <- subject_test$V1;
subject_test <- as.factor(subject_test); ## obtain subject description for test folder

x_train <- read.table("train/X_train.txt");

y_train <- read.table("train/y_train.txt");
y_train <- y_train$V1;
y_train <- as.factor(y_train); ## obtain activity description for train folder

subject_train <- read.table("train/subject_train.txt");
subject_train <- subject_train$V1;
subject_train <- as.factor(subject_train); ## obtain subject description for train folder

features <- read.table("features.txt");
features <- as.character(features$V2);
features <- str_replace_all(features, "-", "");
features <- str_replace_all(features, "[(][)]", "");##obtain features for column names and
                                                    ##remove special characters from them

colnames(x_test) <- features;
colnames(x_train) <- features; ## assign column names to test and train data

x_test <- x_test[1:6];
x_train <- x_train[1:6]; ##Extract only the measurements for mean and standard deviation

x_test <- mutate(x_test, Subject = subject_test, Activity = y_test);
x_train <- mutate(x_train, Subject = subject_train, Activity = y_train);

x_test <- group_by(x_test, Subject, Activity, add = T);
x_train <- group_by(x_train, Subject, Activity, add = T);

x_test <- summarise(x_test, meanX = mean(tBodyAccmeanX), meanY =  mean(tBodyAccmeanY), 
                    meanZ = mean(tBodyAccmeanZ), stdX = mean(tBodyAccstdX), 
                    stdY = mean(tBodyAccstdY), stdZ = mean(tBodyAccstdZ));
x_train <- summarise(x_train, meanX = mean(tBodyAccmeanX), meanY =  mean(tBodyAccmeanY), 
                    meanZ = mean(tBodyAccmeanZ), stdX = mean(tBodyAccstdX), 
                    stdY = mean(tBodyAccstdY), stdZ = mean(tBodyAccstdZ));
## Get mean for each measurement for each activity grouped by each subject

Final_table <- rbind(x_test, x_train); ## Merge the two data sets
Final_table <- mutate(Final_table, Activity = factor(1:6), Activity_labels = c("WALKING", 
                                      "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING",
                                      "STANDING", "LAYING"));
## Give descriptive activities names to activities in data set
Final_table <- select(Final_table, -Activity);
Final_table <- Final_table[c(1,8,2,3,4,5,6,7)]; ## Rearranging columns for nice look
Final_table$Subject <- as.numeric(Final_table$Subject); ## Converting Subject from factor 
                                                        ## to numeric for their arrangement
