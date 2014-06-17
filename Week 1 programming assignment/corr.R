corr <- function(directory, threshold = 0) {
        cwd<-getwd()
        setwd(directory)
        
        all_files<-dir()
        good<-data.frame()
        correlation<-numeric()
        
        for (i in 1:332) {
                data<-read.csv(all_files[i])
                if(sum(complete.cases(data)) >= threshold) {
                        good <- data[complete.cases(data),]
                        if(nrow(good)>0) {
                                correlation<-c(correlation,cor(good$sulfate,good$nitrate,use="complete.obs"))
                        }
                }        
        }
        
        setwd(cwd)
        correlation
}