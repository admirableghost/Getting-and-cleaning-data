# run_analysis.R
# course project script

# Preliminary: 

# Get the data.  it's in a zip file, so download contents
# from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# Then, gather the relevant files into a folder to be named "UCIHAR" in your working directory

# The code:

# 1. I moved all these files individuallly to my working directory first

subtest <- read.table("subject_test.txt")
xtest <- read.table("X_test.txt")
ytest <- read.table("Y_test.txt")
subtrain <- read.table("subject_train.txt")
xtrain <- read.table("X_train.txt")
ytrain <- read.table("Y_train.txt")
features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")

# 2. joining tables
xall <- rbind(xtrain, xtest)
yall <- rbind(ytrain, ytest)
suball <- rbind(subtrain, subtest)
allofit <- cbind(suball, yall, xall)

rm(xtest,ytest,xtrain,ytrain,subtrain,subtest,xall,yall,suball)  # housecleaning

# 3. new col
featureNames <- as.character(features[,2])
newCols <- c("subject", "activity", featureNames)
colnames(allofit) <- newCols

# 4. necessary dimensions

onlyMeans <- grep("mean()", colnames(allofit))
onlyStDevs <- grep("std()", colnames(allofit))
revisedColumns <- c(onlyMeans, onlyStDevs)
revisedColumns2 <- sort(revisedColumns) 
newDataFrame <- allofit[, c(1,2,revisedColumns2)]
newDataFrame2 <- newDataFrame[, !grepl("Freq", colnames(newDataFrame))] #get rid of the meanFreq columns

rm(newDataFrame, allofit)  

# 5. 
tidyframe <- data.frame()
for (i in 1:30) {
        subj<- subset(newDataFrame2,subject==i)
        for (j in 1:6){
                actv<- subset(subj, activity==j)
                myresult<-as.vector(apply(actv,2,mean))
                tidyframe<-rbind(tidyframe,myresult) 
        }
        
}

# 6. final steps
colnames(tidyframe)<-colnames(newDataFrame2) #rename the columns again, as the names get lost in the mix above
levels(tidyframe[,2])<-c('walk','upstairswalk','downstairswalk', 'sit','stand', 'lay')
write.table(tidyframe, "Samsung_Data.txt", sep = "")
