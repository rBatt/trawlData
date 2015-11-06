
#' Sum NA
#' 
#' Sum where NA's are removed, but if all are NA, return NA (not 0)
#' 
#' @param x numbers to be summed
#' @template helperStatTemplate
#' @export
sumna <- function(x){
	if(!all(is.na(x))) return(sum(x, na.rm=T))
	if(all(is.na(x))) return(as.numeric(NA))
}

#' Mean NA
#' 
#' Mean where NA's are removed, but if all are NA, return NA (not 0)
#' 
#' @param x take the mean of these numbers
#' @template helperStatTemplate
#' @export
meanna <- function(x){
	if(!all(is.na(x))) return(mean(x, na.rm=T))
	if(all(is.na(x))) return(as.numeric(NA))
}

#' Weighted Average, NA
#' 
#' Take the weighted average, with NA's removed, but if all are NA, return NA (not 0)
#' 
#' @param x the value to be average over
#' @param y the value to be transformed into weights
#' @template helperStatTemplate
#' @export
# Avg by wtcpue
wtAvg <- function(x,y){
	# x is something like temperature (the value to be averaged)
	# y is something like wtcpue (the value to be used for weighting)
	totW <- sum(y[is.finite(x)])
	propW <- y/totW
	sumna(x*propW)
}

#' Fill Mean
#' 
#' Fill the NA values of a vector with the mean of the non-NA portion
#' 
#' @param x numeric vector whose NA values to be replaced by the non-NA mean
#' @template helperStatTemplate
#' @export
fill.mean <- function(x){
	nai <- is.na(x)
	x[nai] <- meanna(x)
	x
}



