rankhospital <- function(state,outcome,num="best") {
        #read file
        outcome_file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
        convertcols<-c(11,17,23)
        suppressWarnings(outcome_file[convertcols]<-lapply(outcome_file[convertcols],as.numeric))
        answer<-character()
        
        #argument validation
        # 1. check for outcome
        if (!(outcome=="heart failure" || outcome =="heart attack" || outcome == "pneumonia"))
                stop("invalid outcome")
        # 2. check for state
        if(!(state %in% outcome_file$State))
                stop("invalid state")
        
        #creating unsorted df for the required state based on outcome
        state_split<-split(outcome_file,outcome_file$State)
        switch(outcome,
               "heart failure"={
                        final_list<-data.frame(state_split[state])[c(17,2)]
               },
               "heart attack"={
                        final_list<-data.frame(state_split[state])[c(11,2)]
               },
               "pneumonia"={
                        final_list<-data.frame(state_split[state])[c(23,2)]
               })
        
        #### continue from here
        
        #ordering based on outcome and hospital name(in case of tie)
        ordered_df<-final_list[order(final_list[,1],final_list[,2],na.last=NA),]
        
        #handle exception cases for argument num
        if (num=="best")
                num<-1
        else if (num=="worst")
                num<-nrow(ordered_df)
        else if (num>nrow(ordered_df)) {
                answer <- NA
                return (answer)
        }
        
        #returning hospital rank
        answer <- ordered_df[num,2]
        answer
        
 }