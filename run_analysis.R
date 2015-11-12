main <- function() {
  library(data.table)
  library(dplyr)
  
  zipFile <- "zz.zip"
  if (!file.exists(zipFile)) {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url, zipFile)
  }
  
  zippedFilesList <- tbl_dt(unzip(zipFile, list=TRUE)) %>% select(Name)
  commonFilesList <- zippedFilesList %>% filter(grepl("^[^/]*/[^/]+$", Name))
  testFilesList <- zippedFilesList %>% filter(grepl("^.*/test/.+[^/]$", Name))
  trainFilesList <- zippedFilesList %>% filter(grepl("^.*/train/.+[^/]$", Name))
  commonFilesList <- commonFilesList$Name
  testFilesList <- testFilesList$Name
  trainFilesList <- trainFilesList$Name
  
  activityLabels <- tbl_dt(read.table(getFilePathForUnzip("activity_labels.txt", commonFilesList)))
  features <- tbl_dt(read.table(getFilePathForUnzip("features.txt", commonFilesList)))
  meanStdColumns <- ""

  if (!exists("dat")) {
    trainX <- tbl_dt(read.table(getFilePathForUnzip("X_train.txt", trainFilesList)))
    setnames(trainX, colnames(trainX), as.character(select(features,2)[[1]]))
    meanStdColumns <- grep("(mean\\(\\))|(std\\(\\))", colnames(trainX), value=TRUE)
    trainX <- trainX %>% select(one_of(meanStdColumns))
    trainSubject <- tbl_dt(read.table(getFilePathForUnzip("subject_train.txt", trainFilesList)))
    trainY <- tbl_dt(read.table(getFilePathForUnzip("y_train.txt", trainFilesList)))
    trainY <- left_join(trainY, activityLabels)$V2
    train <- trainX %>% mutate(subject=trainSubject, label=trainY)

    testX <- tbl_dt(read.table(getFilePathForUnzip("X_test.txt", testFilesList)))
    setnames(testX, colnames(testX), as.character(select(features,2)[[1]]))
    testX <- testX %>% select(one_of(meanStdColumns))
    testSubject <- tbl_dt(read.table(getFilePathForUnzip("subject_test.txt", testFilesList)))
    testY <- tbl_dt(read.table(getFilePathForUnzip("y_test.txt", testFilesList)))
    testY <- left_join(testY, activityLabels)$V2
    test <- testX %>% mutate(subject=testSubject, label=testY)
    
    dat <<- union(train, test)
  }
  
  tidyDat <<- dat %>% 
      group_by(label, subject) %>% 
      summarise_each(funs(mean)) %>% 
      arrange(label, subject)
  write.table(tidyDat, file="tidyData.txt", row.name=FALSE)
}

getFilePathForUnzip <- function(fileName, fileNamesList) {
  if (grepl("\\\\", fileName)) {
    stop("IllegalArgumentException")
  }
  fileName <- gsub("([\\.\\*])", "\\\\\\1", fileName)
  result <- grep(paste0("^.*/", fileName), fileNamesList, value=TRUE)
}

main()