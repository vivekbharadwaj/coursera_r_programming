rankall <- function(outcome, num="best") {
        #read file
        outcome_file <- read.csv("outcome-of-care-measures.csv",
                        colClasses="character",na.string="Not Available")
        convertcols<-c(11,17,23)
        suppressWarnings(outcome_file[convertcols]<-
                                 lapply(outcome_file[convertcols],as.numeric))
        answer<-data.frame()
        
        #argument validation
        #check for outcome
        if (!(outcome=="heart failure" || outcome =="heart attack" || 
                      outcome == "pneumonia"))
                stop("invalid outcome")
        
        #isolating only required columns of outcome_file based on outcome and 
        #storing only 3 columns in another data frame called "short_outcome"
        switch(outcome,
               "heart failure"={
                       short_outcome<-data.frame(outcome_file[c(7,17,2)])
               },
               "heart attack"={
                       short_outcome<-data.frame(outcome_file[c(7,11,2)])
               },
               "pneumonia"={
                       short_outcome<-data.frame(outcome_file[c(7,23,2)])
               })
        
        #order and split by state in both asc and desc order (based on 
        # num argument best or worst)
        asc_ordered_df <- data.frame(short_outcome[
                order(short_outcome[,1],short_outcome[,2],short_outcome[,3],
                      na.last=TRUE),])
        desc_ordered_df <- data.frame(short_outcome[
                order(short_outcome[,1],-short_outcome[,2],short_outcome[,3],
                      na.last=TRUE),])
        
        
        #handle exception cases for argument num
        if (num=="best")
        {
                num<-1
                split_ordered_list <- split(asc_ordered_df,asc_ordered_df[,1])
        }
        else if (num=="worst")
        {
                num<-1
                split_ordered_list <- split(desc_ordered_df,desc_ordered_df[,1])
        }
        else split_ordered_list <- split(asc_ordered_df,asc_ordered_df[,1])
        
        #computing hospital names based on num argument
        hospital_name_vec <- sapply(split_ordered_list,function(x) x[num,3])
        state_list <- unique(asc_ordered_df[,1])
        answer<-data.frame(cbind(hospital_name_vec,state_list))
        names(answer)<-c("hospital","state")
        answer

 }