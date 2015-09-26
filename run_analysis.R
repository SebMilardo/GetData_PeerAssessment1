library(plyr)
library(dplyr)
library(data.table)

setwd(".")

# unzip data file
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"
unzip(filename);

# read all the files in the zip archive
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

getData <- function(type){ 

# read X and put the correct names
X <- read.table(paste0("./UCI HAR Dataset/",type,"/X_",type,".txt"))
names(X) <- features[,2]

# read the subjects
subject <- read.table(paste0("./UCI HAR Dataset/",type,"/subject_",type,".txt"))
subject[,1] <- factor(subject[,1])
names(subject) <- "subject"

# read the activity, change numbers -> factors, add name 
y <- read.table(paste0("./UCI HAR Dataset/",type,"/y_",type,".txt"))
activity <- join(y,activity_labels)[,2]
names(activity) <- c("activity")

data <- cbind(subject,activity,X)
data
}

# merge test and train data
Data = rbind(getData("test"),getData("train"))

# extracts only the measurements on the mean and standard deviation for each measurement. 
Data<- Data[,c(1,2,grep("mean|std", names(Data)))]

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- aggregate(. ~subject + activity, Data, mean)
tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]

write.table(tidyData, file = "tidyData.txt", row.names = FALSE)
