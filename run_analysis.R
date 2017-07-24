
#read in all used data
TestTE <- read.table(file="./test/X_test.txt")
TestTR <- read.table(file="./train/X_train.txt")
TestYTrain<- read.table(file="./train/y_train.txt")
TestYTest<- read.table(file="./test/y_test.txt")
TestSubjTrain<- read.table(file="./train/subject_train.txt")
TestSubjTest<- read.table(file="./test/subject_test.txt")
Activities<- read.table(file="./activity_labels.txt")

#merge the training and test data set
TestALL_X<-rbind(TestTE,TestTR)
TestALL_Y<-rbind(TestYTrain,TestYTest)
TestALL_Sub<-rbind(TestSubjTrain,TestSubjTest)
Headrs<-read.table(file="./features.txt")

#label the data set with discriptive variable names
Headrs[,2]<-gsub("-", "_", Headrs[,2])
Headrs[,2]<-gsub("\\(|\\)", "", Headrs[,2])
colnames(TestALL_X) <- Headrs$V2
names(TestALL_Y)<-"activity"
names(TestALL_Sub)<-"subjects"

#Extract meassurement on mean and standard deviation
TestSOME_mean <- TestALL[ , grepl(( "mean" ) , names( TestALL ) ) ]
TestSOME_std <- TestALL[ , grepl(( "std" ) , names( TestALL ) ) ]
ind <- apply( TestSOME_std , 1 , function(x) any( x > 0 ) )
TestSOME_std[ ind , ]
Test_ALL<-cbind(TestALL_Sub, TestALL_Y,TestSOME_mean,TestSOME_std)
Test_ALL <- Test_ALL[ , !grepl(( "-X"  ) , names( Test_ALL ) ) ]
Test_ALL <- Test_ALL[ , !grepl(( "-Y"  ) , names( Test_ALL ) ) ]
Test_ALL <- Test_ALL[ , !grepl(( "-Z"  ) , names( Test_ALL ) ) ]
Test_ALL <- Test_ALL[ , !grepl(( "Freq()"  ) , names( Test_ALL ) ) ]
ind <- apply( Test_ALL , 1 , function(x) any( x > 0 ) )
Test_ALL[ ind , ] 

# name with discriptive activity names
Test_ALL$activity <- factor(Test_ALL$activity, levels = Activities[, 1], labels = Activities[, 2]) 

#creates a second, independent tidy data set with the average of each variable for each activity and each subject
Test_ALLGroup<-Test_ALL %>% group_by(subjects, activity) 
Test_Data <- aggregate(Test_ALLGroup[, 3:ncol(Test_ALLGroup)],by=list( activity = Test_ALLGroup$activity,subject = Test_ALLGroup$subjects),mean)

print(dim(Test_Data))
View(Test_Data)
print(Test_Data[1,])

# save to file
write.table(Test_Data, row.name=FALSE, file = "tidy_data.txt")
