best <- function(state,outcome) {
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
        
        state_split<-split(outcome_file,outcome_file$State)
        final_list<-data.frame(state_split[state])
        switch(outcome,
               "heart failure"={
                       ordered_df<-final_list[order(final_list[,17],final_list[,2],na.last=NA),]
               },
               "heart attack"={
                       ordered_df<-final_list[order(final_list[,11],final_list[,2],na.last=NA),]
               },
               "pneumonia"={
                       ordered_df<-final_list[order(final_list[,23],final_list[,2],na.last=TRUE),]
               })
        answer<-ordered_df[1,2]
        answer
}