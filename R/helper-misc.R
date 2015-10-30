
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


# Added a check to make sure that names repeated in different tables did 
# not have different values, or were reported as such
# also drops columns duplicated from merging,
# and handles 'conflicts' arising from one column being
# NA, and the other !is.na(), replacing the NA with the non-NA value
# These checks and changes make the function a bit slow
trim.autoColumn <- function(X){
	strip.names <- gsub("\\.[xy]", "", names(X))
	dup.names <- strip.names[duplicated(strip.names)]
	if(length(dup.names)==0){
		message("No names duplicated in the form of 'column.x' 'column.y'")
		return(NULL)
	}
	
	reduce.na <- function(x){
		# if only 1 column has an NA for that row,
		# because the columns presumably represent same measurements,
		# replace the NA value with the non-NA value
		x.x <- X[,eval(s2c(paste0(x,".x")))][[1]]
		x.y <- X[,eval(s2c(paste0(x,".y")))][[1]]
		
		na.only.x <- is.na(x.x) & !is.na(x.y)
		if(any(na.only.x)){
			X[na.only.x, c(paste0(x,".x")):=eval(s2c(paste0(x,".y")))]
		}
		
		na.only.y <- is.na(x.y) & !is.na(x.x)
		if(any(na.only.y)){
			X[na.only.y, c(paste0(x,".y")):=eval(s2c(paste0(x,".x")))]
		}
		
	}
	suppressWarnings(invisible(sapply(dup.names, reduce.na)))
	
	test.match <- function(x){
		x.x <- X[,eval(s2c(paste0(x,".x")))][[1]]
		x.y <- X[,eval(s2c(paste0(x,".y")))][[1]]
		!any((x.x != x.y) & (!is.na(x.x) & !is.na(x.y)))
	}
	dup.names.same <- sapply(dup.names, test.match)

	drop.y <- function(x){
		X[,c(paste0(x,".y")):=NULL]
		setnames(X, paste0(x,".x"), x)
	}
	if(any(dup.names.same & !is.na(dup.names.same))){
		drop.y(dup.names[dup.names.same])
	}
	

	if(any(!dup.names.same | is.na(dup.names.same))){
		message("These columns had names that differed only in the suffix '.x' or '.y',\nbut they had different values, implying conflict in merged tables:\n",paste(dup.names[!dup.names.same | is.na(dup.names.same)], collapse="\n"))
	}
}



# handy lookup function
match.tbl <- function(ref, tbl.ref, tbl.val, exact=FALSE){
	# ref = gmex[,spp.orig]
	# tbl.ref = taxInfo[,raw.spp]
	# tbl.val = taxInfo[,spp]
	
	nref <- length(ref)
	na.vec <- rep(NA, nref)
	if(!exact){
		ref.for <- cullParen(cullSp(fixCase(cullExSpace(ref))))
	}
	
	
	m1 <- match(ref, tbl.ref)
	if(!exact){
		m2 <- match(ref.for, tbl.ref)
	}
	
	
	m1.na <- is.na(m1)
	m1.fill <- !m1.na
	if(!exact){
		m2.na <- is.na(m2)
		m2.fill <- m1.na & !m2.na
	}
	
	
	val <- na.vec
	val[m1.fill] <- tbl.val[m1[m1.fill]]
	if(!exact){
		val[m2.fill] <- tbl.val[m2[m2.fill]]
	}
	
	if(!exact){
		if(any(is.na(val))){
			qa <- function(x,y){
				agrep(pattern=x, x=y, ignore.case=T, value=F, max.distance=0.25)[1]
			}
			m3 <- na.vec
			nav <- is.na(val)
			m3[nav] <- sapply(ref[nav], qa, y=tbl.ref)
			m3.fill <- !is.na(m3)
			val[m3.fill] <- tbl.val[m3[m3.fill]]
		}else{
			m3.fill <- rep(FALSE, nref)
		}
	
		if(any(is.na(val))){
			m4 <- na.vec
			nav <- is.na(val)
			m4[nav] <- sapply(ref.for[nav], qa, y=tbl.ref)
			m4.fill <- !is.na(m4)
			val[m4.fill] <- tbl.val[m4[m4.fill]]
		}else{
			m4.fill <- rep(FALSE, nref)
		}
	}
	
	
	val.src <- na.vec
	val.src[m1.fill] <- "m1"
	if(!exact){
		val.src[m2.fill] <- "m2"
		val.src[m3.fill] <- "m3"
		val.src[m4.fill] <- "m4"
	}
	
	
	tbl.row <- na.vec
	tbl.row[m1.fill] <- m1[m1.fill]
	if(!exact){
		tbl.row[m2.fill] <- m2[m2.fill]
		tbl.row[m3.fill] <- m3[m3.fill]
		tbl.row[m4.fill] <- m4[m4.fill]
	}
	
	
	
	out <- data.table(
		ref = ref,
		val = val,
		val.src = val.src,
		tbl.row = tbl.row
	)
	
	return(out)
}