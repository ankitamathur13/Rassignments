complete <- function(directory, id = 1:332) {
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
	filesx <- dir(directory)
	result<- data.frame(id=integer(),nobs=integer())
	for(i in id)
	{
		f <-filesx[i]
		df<-read.csv(paste(c(directory,f),collapse=.Platform$file.sep),header=TRUE)
		nob<- sum(complete.cases(df))
		row<-c(i,nob)
		
		result<- rbind(result, row )
	}
	names(result)<- c("id","nobs")
	result
	
}