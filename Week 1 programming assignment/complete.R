complete<-function(directory, id = 1:332) {
        cwd<-getwd()
        final<-data.frame()
        setwd(directory)
        
        all_files<-dir()
        
        for (i in id) {
                data<-read.csv(all_files[i])
                nobs<-sum(complete.cases(data))
                final<-rbind(final,c(i,nobs))        
                }
        names(final)<-c("id","nobs")
        
        setwd(cwd)
        final
        }