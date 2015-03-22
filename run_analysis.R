
## This must be executed inside the folder where UCI HAR Dataset
## is located.

combineDataSet <- function(){
        folders <- c("test","train");
        
        library(data.table);
        ##install.packages("plyr");
        library(plyr);
        
        activityLabels <- fread("./UCI HAR Dataset/activity_labels.txt");
        setNames(object = activityLabels, nm = c("ActivityId","ActivityName"));
        features <- read.table("./UCI HAR Dataset/features.txt");
        names(features) <- c("featureId","featureName");
        listOfDS1 <- list();
        
        ## Combining data set of each type involves identical process
        for(indexes in seq_along(along.with = folders)) {
                folderName <- folders[indexes];
                fileUrl <- paste("./UCI HAR Dataset/",folderName,"/X_",folderName,".txt",sep="")
                observations <- read.table(fileUrl);
                observations <- data.table(observations);
                setNames(object = observations, nm= as.character(features[,"featureName"]));
                sequence <- 1 : nrow(observations);
                observations[,index:=sequence];

        
                fileUrl <- paste("./UCI HAR Dataset/",folderName,"/subject_",folderName,".txt",sep="");
                subjects <- fread(fileUrl);
                setNames(object = subjects, nm =   c("subjectId"));
                subjects[,index:=sequence];
        
                fileUrl <- paste("./UCI HAR Dataset/",folderName, "/y_", folderName, ".txt",sep = "");
                activities <- fread(fileUrl);
                setNames(object = activities, nm=c("ActivityId"));
        
                activityWithLabels <- join(activities,activityLabels,"ActivityId");
                activityWithLabels[,index:=sequence];
                activityWithLabels <- activityWithLabels[,c("ActivityName","index"),with=FALSE];

                subjectsWithActivities <- join(subjects,activityWithLabels, "index");
                subjectsWithActivitiesAndObservations <- join(subjectsWithActivities,observations,"index");
                namesOfColumnsInDS <- names(subjectsWithActivitiesAndObservations);
                namesOfRequiredColumnsInDS <- namesOfColumnsInDS[namesOfColumnsInDS!="index"];
                subjectsWithActivitiesAndObservations <- subjectsWithActivitiesAndObservations[,namesOfRequiredColumnsInDS,with=FALSE]
                listOfDS1[[indexes]] <- subjectsWithActivitiesAndObservations;
        }
        
        
        combinedDataSet  <- rbind(listOfDS1[[1]],listOfDS1[[2]]);
        ##return(listOfDS1Merge);
        
        ## BEGIN:Process for Step 2; Evaluation of average of each variable for each activity and each subject.
        columnNames <- names(combinedDataSet)
        selectedColumnNames <- columnNames[1:2] ## We need subjectId and ActivityName columns for sure.
        
        columnsWithStd <- columnNames[grepl("std()",columnNames,fixed = TRUE)] ## Gets Column Names which have std() and mean() in them.
        columnsWithMean <- columnNames[ grepl("mean()", columnNames,fixed = TRUE)]
        selectedColumnNames <- append(selectedColumnNames,columnsWithMean)
        selectedColumnNames <- append(selectedColumnNames,columnsWithStd) ## Final list of Columns that we need for Step 2.
        dsWithStdAndMeanForFeaturesOnly <- combinedDataSet[,selectedColumnNames,with=FALSE]
        ## END:Process for Step 2; Evaluation of average of each variable for each activity and each subject.
        
        ## Begin:Process for Step 5; Evaluation of average of each variable for each activity and each subject.
        library(reshape2)
        measuredColumnNames <- selectedColumnNames[!(selectedColumnNames  %in% c("subjectId", "ActivityName"))]
        meltedDs <- melt(dsWithStdAndMeanForFeaturesOnly, id=c("subjectId","ActivityName"),measure.vars = measuredColumnNames)
        dsWithAvgOfMeasurementForEachSubjectAndActivity <- dcast(meltedDs, subjectId + ActivityName ~ variable , mean)
        ## End:Process for Step 5; Evaluation of average of each variable for each activity and each subject.
        
        ## Begin:Process for Prefix name of the measured columns to "Avg of {measuredValudColumnName}"
        namesOfFinalDS <- names(dsWithAvgOfMeasurementForEachSubjectAndActivity)
        
        for(index in seq_along(namesOfFinalDS)){
                if(! ((namesOfFinalDS[index]== "subjectId") || (namesOfFinalDS[index]== "ActivityName"))) {
                        namesOfFinalDS[index] <- paste("Avg of ",namesOfFinalDS[index], sep="");
                }
        }
        names(dsWithAvgOfMeasurementForEachSubjectAndActivity)  <- namesOfFinalDS;
        ## End:Process for Prefix name of the measured columns to "Avg of {measuredValudColumnName}"
        
        
        return(dsWithAvgOfMeasurementForEachSubjectAndActivity);
}


