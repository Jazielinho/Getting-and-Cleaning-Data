#Change working directory
#setwd("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
getwd()

############################################################################################################################
#1.- Merges the training and the test sets to create one data set.
#Load data
Xtest<-read.table("./test/X_test.txt")
Xtrain<-read.table("./train/X_train.txt")
X<-rbind(Xtest,Xtrain)

Ytest<-read.table("./test/Y_test.txt")
Ytrain<-read.table("./train/Y_train.txt")
Y<-rbind(Ytest,Ytrain)

Stest<-read.table("./test/subject_test.txt")
Strain<-read.table("./train/subject_train.txt")
Subject<-rbind(Stest,Strain)

head(X);head(Y);head(Subject)

############################################################################################################################
#2.- Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("features.txt")
ind_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, ind_features]
names(X) <- tolower(gsub("\\(|\\)", "", features[ind_features, 2]))
head(X)

############################################################################################################################
#3.- Uses descriptive activity names to name the activities in the data set
Activity <- read.table("activity_labels.txt")
Activity[, 2] = gsub("_", "", tolower(as.character(Activity[, 2])))
Y[,1] = Activity[Y[,1], 2]
names(Y) <- "Activity"

############################################################################################################################
#4.- Appropriately labels the data set with descriptive activity names. 
names(Subject) <- "Subject"
clean <- cbind(Subject, Y, X)
head(clean)

############################################################################################################################
#5.- Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
uniqSubject = unique(Subject)[,1]
numSubject = length(uniqSubject)
numActivity = length(Activity[,1])
numCols = dim(clean)[2]
limit<-numSubject*numActivity
result = clean[1:limit, ]

for (i in 1:numSubject) {
	for (j in 1:numActivity) {
			for (k in 1:limit){
				result[k, 1] = uniqSubject [i]
				result[k, 2] = Activity[j, 2]
				tmp <- clean[clean$subject==i & clean$Activity==Activity[j, 2], ]
				result[k, 3:numCols] <- colMeans(tmp[, 3:numCols])
			}
	}
}
write.table(result, "data.set_with_averages.txt")

