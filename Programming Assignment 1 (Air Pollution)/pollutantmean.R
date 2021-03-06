#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the 'directory' argument and
#returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.
pollutantmean<- function(directory,pollutant,id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  n <- length(id)
  completedata <- rep(0,n)
  sumdata<-rep(0,n)
  for(i in id){
    data <- read.csv(sprintf('%s/%03d.csv', directory, i), header=TRUE, sep=",")
    p=data[,pollutant]
    completedata <-completedata+ sum(!is.na(p))
    sumdata<-sumdata+sum(p,na.rm=TRUE)
    avg<-sumdata/completedata
  }
  avg
}