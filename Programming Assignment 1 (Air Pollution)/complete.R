#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
#The function should return a data frame where 
#the first column is the name of the file 
#the second column is the number of complete cases.
complete<- function(directory,id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  n <- length(id)
  completedata <- rep(0,n)
  j<-1
  for(i in id){
    data <- read.csv(sprintf('%s/%03d.csv', directory, i), header=TRUE, sep=",")
    completedata[j]<-sum(!is.na(data$sulfate) & !is.na(data$nitrate))
    j<-j+1
  }
  com<-data.frame(id,nobs=completedata)
  com
}