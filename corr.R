corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  Cval<-c()
  count<-1
  files <- list.files(directory, pattern = "*.csv", FALSE)
  for(i in 1:length(files)){
    fl <- paste(directory,files[i],sep='/')
    data <- read.csv(fl)
    data1<- as.matrix(data)
    data2<-na.omit(data1)
    if(nrow(data2)>threshold){
      x1<-as.numeric(data2[,2])
      x2<-as.numeric(data2[,3])
      Cval[count]<-cor(x1,x2)
      count<-count+1
    }
  }
  Cval
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}