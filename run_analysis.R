library(dplyr)
library(tidyr)

create_dataset <- function(){
        
        "
        1. Merges the training and the test sets to create one data set.
        
        "
        
        # Import training feature set (x_train, x_test), subject set (subject_train, subject_test) and target set (y_train, y_test) from directory
        x_train       <- read.table("train/X_train.txt")
        subject_train <- read.table("train/subject_train.txt") 
        y_train       <- read.table("train/y_train.txt")
        
        x_test        <- read.table("test/X_test.txt")
        subject_test  <- read.table("test/subject_test.txt")
        y_test        <- read.table("test/y_test.txt")
        
        # Bind X and y to form a complete training/testing set
        
        ## Firstly, rename the variable in y and subject before merging
        names(y_train)          <- "activity_code"
        names(y_test)           <- "activity_code"
        names(subject_train)    <- "subject"
        names(subject_test)     <- "subject"
        
        ## Use cbind to bind activity codes from y to x sets (subject | x | y)
        train <- cbind(subject_train,x_train, y_train)
        test  <- cbind(subject_test, x_test,  y_test)
        
        # Finally, bind train and test sets to return a master set containing all the data
        rbind(train, test)
}

extract_imp_features <- function(){
        "
                2. Extracts only the measurements on the mean and standard deviation for each measurement.

        "
        
        # Call previous function to load the complete dataset from the files contained within the directory
        master <- create_dataset()
        
        # Extract a list of feature names from the features.txt file (This list will come in handy for Step#3 as well)
        feature_names <- read.table("features.txt")[,2]
        
        # Use RegEx search to read through the list and only keep track of the variables strictly containing 'mean()' or 'std()'
        # as a substring. The indices are saved on another list
        imp_feature_indices <- grepl("-(mean|std)[()]", feature_names)
        
        # Add an additional element to the list to preserve the final column of the dataframe (though it wasn't needed)
        imp_feature_indices <- c(TRUE, imp_feature_indices, TRUE)
        
        # Set master to only contain columns pertaining to the important variables. Luckily, since V1 (our first column) is  
        # a mean variable, our last column (i.e. acitivty_code) would've still been preserved without the previous statement
        tbl_df(master[,imp_feature_indices])
}

rename_activities <- function(){
        "
                3. Uses descriptive activity names to name the activities in the data set
        "
        # Call the previous function to load the reduced tibble with only the important variables.
        data <-  extract_imp_features()
        
        # Load Acivity labels corresponding to each activity number from acitivity_labels.txt and store in a list
        activities <- read.table("activity_labels.txt")[,2]
        
        # Add a column to the tibble that assigns an acitivity label corresponding to the activity code.
        data <- data %>% mutate(activty = activities[activity_code])
        
        # Since we already have a column for the "activity" variable, tidy up the data by removing activity_code before returning
        data %>% select(-activity_code)
}
rename_variables <- function(){
        "
                4. Appropriately labels the data set with descriptive variable names.
        "
        # Call the previous function to load the reduced tibble with only the important variables AND the labelled activities.
        data <-  rename_activities()
        
        # For the sake of maintaining authenticity, we'll use the names provided in features.txt as labels for their respective variables
        
        ## For this, we can recall the feature_names list we created in the previous function but just as strings describing the features
        feature_names <- read.table("features.txt", as.is = TRUE)[,2]
        
        ## Get the subset of the indices that we've preserved in our reduced tibble (i.e. the mean and std variables)
        imp_feature_indices <- grepl("-(mean|std)[()]", feature_names)
        
        ## Preserve the names of only the important indices we retained in extract_imp_features and apply them as column names
        feature_names <- feature_names[imp_feature_indices]
        column_names <- c("subject", feature_names, "activity")
        names(data) <- as.factor(column_names)
        
        # Return the tibble
        data
}

create_mean_tidy_set <- function(){
        "
               5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        "
        # Call the previous function to load the reduced tibble with the important, labelled variables AND the labelled activities.
        data <-  rename_variables()
        
        # Use tidyr and dplyr verbs to generate the required summary
        tidy <- data %>% gather(value = value, key = variable, -subject, -activity) %>% group_by(subject,variable,activity) %>% summarise(average = mean(value))
        
        # Write the tidy set to a file
        write.table(tidy, file = "tidy_set.txt", row.names = FALSE)
        
        # Return it to the console for review
        tidy
}