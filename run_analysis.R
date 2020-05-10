#Part 1: Merges the training and the test sets to create one data set.

    #1a) Read all txt files as dataframes 
    activity_labels<-read.table("activity_labels.txt")
    features<-read.table("features.txt")
    
    #Read test data files 
    test_data<-read.table("test/X_test.txt")
    test_labels<-read.table("test/y_test.txt")
    subject_test<-read.table("test/subject_test.txt")
    
    #Read training data files 
    train_data<-read.table("train/X_train.txt")
    train_labels<-read.table("train/y_train.txt")
    subject_train<-read.table("train/subject_train.txt")
    
    #Rename the features in the test and training data files 
    names(test_data)<-features$V2
    names(train_data)<-features$V2
    
    #combine data with labels and subject identification for both train and test data
    train_data2<-cbind(subject_train,train_labels,train_data)
    test_data2<-cbind(subject_test,test_labels,test_data)
    
    #merge training and test data as one dataset
    full_data<-rbind(train_data2,test_data2)
    
    #****Note: For Assignment, completes Part 3****
    #merge activity labels with dataset 
    full_data<-merge(activity_labels,full_data,by.x = 1,by.y = 2)
    
    #drop first column
    full_data<-full_data[,-1]
    
    #****Note: For Assignment, completes Part 4****
    #rename first 2 columns
    names(full_data)[1:2]<-c("activity","subjectid")

#Part 2: Extracts only the measurements on the mean and standard deviation for each measurement.

    #change all column names to lower case 
    names(full_data)<-tolower(names(full_data))

    #subset full data on only those measurements that contain mean and standard deviation 
    full_data2<-full_data[,c(1,2,(grep("mean\\()|std\\()",names(full_data))))]
    
#Part 3: Uses descriptive activity names to name the activities in the data set
  #****Note already completed in Part 1: Merge activity labels with dataset****


#Part 4: Appropriately labels the data set with descriptive variable names.
  #****Note See Part 5: ***


#Part 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  library(reshape2)
  library(dplyr)
  #Melt the data 
  full_data3<-melt(full_data2,id=c("activity","subjectid"))
  
  #take the average of each variable for each activity and each subject
  full_data3<-dcast(full_data3,activity + subjectid ~variable,mean)
  
  #arrange the data 
  full_data3<-arrange(full_data3,subjectid,activity)
  
  #add an observation ID variable 
  final_data<-mutate(full_data3,id=seq.int(nrow(full_data3)))
  
  #rorder columns by position 
  final_data<-final_data[,c(69,2,1,3:68)]
  
  #remove all dashes and brackets from column names 
  names(final_data)<-gsub("-"," ",names(final_data))
  names(final_data)<-gsub("\\()","",names(final_data))
  
  #create more descriptive variable names
  names(final_data)<-gsub("std","standard deviation",names(final_data))
  names(final_data)<-gsub("\\bx\\b","x-axis",names(final_data))
  names(final_data)<-gsub("\\by\\b","y-axis",names(final_data))
  names(final_data)<-gsub("\\bz\\b","z-axis",names(final_data))
  
  #Create a txt file to upload for assignment
  write.table(final_data,"Final Tidy Data.txt", row.name = FALSE)
