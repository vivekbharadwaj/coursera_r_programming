pollutantmean <- function(directory, pollutant, id = 1:332) {
        all_files <- dir(paste(getwd(),directory,sep="/"),full.names=TRUE)
        for (monitor in id) 
        {
                if(!exists ("reqd_file")) 
                        reqd_file<-all_files[monitor]
                else
                        reqd_file<-c(reqd_file,all_files[monitor])
        }
        data<-do.call("rbind",lapply(reqd_file,read.csv,header=TRUE))
        mean(data[[pollutant]],na.rm=TRUE)
}

