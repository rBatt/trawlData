
# String to call
# convert a character vector to a call of the desired class
# e.g., convert c("column1","column2") to list(column1, column2)
# intended for use in a data.table (see ?"[.data.table")

# x is the string
# type is the class resulting from the evaluation of the call returned by s2c

s2c <- function(x, type="list"){
	as.call(lapply(c(type, x), as.symbol))
}

# example
# first <- data.table(cray=sample(letters,4),"one"=c(1,2,3,4), "two"=c(1,3,5,7))
# second <- data.table("cray"=sample(letters,35, TRUE))
#
# oc <- CJ(one=first[,(one)], cray=second[[1]])
#
# sub <- s2c(c("one","cray"))
#
# oc[,eval(sub)]





# Reorder 1st dim of array w/o knowing total dims
orderD1 <- function(x, ord){	
	dims <- dim(x)
	ndim <- length(dims)
	
	stopifnot(ndim>0)
	
	if(ndim==1){
		return(x[ord])
	}

	wl_i <- which(letters=="i")
	dimLetters <- letters[wl_i:(wl_i+ndim-1)]

	dimList <- structure(vector("list",ndim), .Names=dimLetters)
	dimList[[1]] <- ord
	for(i in 2:ndim){
		dimList[[i]] <- 1:dims[i]
	}
	do.call("[",c(list(x=x),dimList))
}

# # Example
# orderD1(arr, 4:1)
# arr <- array(1:24, dim=c(4,3,2))
# arr[4:1,,]
#
# library(microbenchmark)
# microbenchmark(arr[4:1,,], orderD1(arr, 4:1), times=1E3)
# Unit: nanoseconds
#               expr   min    lq      mean median      uq    max neval
#       arr[4:1, , ]   864  1241  1445.876   1451  1596.0  17191  1000
#  orderD1(arr, 4:1) 52020 54061 56286.856  54909 56194.5 179363  1000



# so simple, but so convenient (and short!)
lu <- function(x) length(unique(x))




# Cut years into 9 chunks (or min(n.yrs))
cy <- function(x){
	lux <- length(unique(x))
	if(lux>1){
		as.character(cut(as.numeric(x), breaks=min(2,lux)))
	}else{
		unique(x)	
	}
}