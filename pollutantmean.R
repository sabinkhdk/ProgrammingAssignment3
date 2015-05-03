pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- list.files(directory, pattern = "*.csv", FALSE)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  col<-if(pollutant=='sulfate'){
    2
    } else { 
      3 }
  dmat<-matrix()
  for(i in id){
    fl <- paste(directory,files[i],sep='/')
    dfl <- read.csv(fl)
    dcol <- as.matrix(dfl[,col])
    dmat<-rbind(dmat,dcol)
  }
  data<-na.omit(dmat)
  Mdata<-mean(data)
  Mdata
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
}