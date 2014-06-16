#Write a function that takes a directory of data files and a threshold for complete cases and 
#calculates the correlation between sulfate and nitrate for monitor locations where 
#the number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.
corr<- function(directory,threshold=0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  id=1:332
  n <- length(id)
  nobs <- rep(0,n)
  cr<-rep(0,n)
  j<-1
  for(i in id){
    data <- read.csv(sprintf('%s/%03d.csv', directory, i), header=TRUE, sep=",")
    nobs <- sum(complete.cases(data))
    if (nobs > threshold) {
      cr[j]<-(cor(data$nitrate, data$sulfate, use="complete.obs"))
      j<-j+1
    }
  }
  cr[1:j-1] ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
}