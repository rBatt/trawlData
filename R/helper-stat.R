sumna <- function(x){
	if(!all(is.na(x))) return(sum(x, na.rm=T))
	if(all(is.na(x))) return(as.numeric(NA))
}

meanna <- function(x){
	if(!all(is.na(x))) return(mean(x, na.rm=T))
	if(all(is.na(x))) return(as.numeric(NA))
}

# Avg by wtcpue
wtAvg <- function(x,y){
	# x is something like temperature (the value to be averaged)
	# y is something like wtcpue (the value to be used for weighting)
	totW <- sum(y[is.finite(x)])
	propW <- y/totW
	sumna(x*propW)
}

# Fill the NA values of a vector with the mean of the non-NA portion
fill.mean <- function(x){
	nai <- is.na(x)
	x[nai] <- meanna(x)
	x
}



