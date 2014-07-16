corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations	
	files<- dir(directory)
	numfiles<- length(files)
	corr <- numeric(0)
	for(i in 1:numfiles)
	{
		f <-files[i]
		df<-read.csv(paste(c(directory,f),collapse=.Platform$file.sep),header=TRUE)
		nob<- sum(complete.cases(df))		
		if(nob>=threshold)
		{			
			correlation<-cor(df$sulfate,df$nitrate, use="pairwise.complete.obs")			
			corr<-append(corr,correlation)
		}		
	}
	corr
}