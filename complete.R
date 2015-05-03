complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- list.files(directory, pattern = "*.csv", FALSE)
  val<-matrix(nrow=length(id),ncol=2)
  ct<-1
  for(i in id){
    dt<-read.csv(paste(directory,files[i],sep='/'))
    tmp<-as.matrix(dt)
    tmp1<-na.omit(tmp)
    val[ct,1]<-i
    val[ct,2]<-nrow(tmp1)
    ct<-ct+1
  }
  colnames(val)<-c('id','nobs')
  as.data.frame(val)
}