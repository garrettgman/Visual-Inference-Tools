# handler function for Read Data

handlerReadData <- function(h, ...){
	path <- gfile("Select a csv file...", filter = list("csv" = 
		list(patterns = c("*.csv"))), handler = function(h, ...) NULL)
	
	if(!is.na(path)){
		df <- read.csv(path)
		
		# some function that determines the type of VIT the data requires
		# some method that saves canvas$new(df) to the VIT object
}
	

