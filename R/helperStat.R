
#' Sum NA
#' 
#' Sum where NA's are removed, but if all are NA, return NA (not 0)
#' 
#' @param x numbers to be summed
#' @param na.rm logical, default TRUE, remove NA's?
#' @param ... unused
#' 
#' @template helperStatTemplate
#' @export
sumna <- function(x, na.rm=TRUE, ...){
	if(!all(is.na(x))) return(sum(x, na.rm=na.rm))
	if(all(is.na(x))) return(NA_real_)
}


#' Mean NA
#' 
#' Mean where NA's are removed, but if all are NA, return NA (not 0)
#' 
#' @param x take the mean of these numbers
#' @param na.rm logical, default TRUE, remove NA's?
#' @param ... unused
#' 
#' @template helperStatTemplate
#' @export
meanna <- function(x, na.rm=TRUE, ...){
	if(!anyNA(x) | !na.rm){
		return(sum(x)/length(x))
	}else{
		nax <- is.na(x)
		if(!all(nax)){
			n <- sum(!nax)
			return(sum(x,na.rm=na.rm)/n)
		}else{
			return(NA_real_)
		}
	}
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
#' @export fill.mean
fill.mean <- function(x){
	nai <- is.na(x)
	x[nai] <- meanna(x)
	x
}



