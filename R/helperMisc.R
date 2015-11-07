
#' String to call
#' Convert a character vector to a call of the desired class
#' 
#' @param x a character string (e.g., of column names in a data.table)
#' @param type the class resulting from the evaluation of the call returned by s2c
#' 
#' @details
#' e.g., convert \code{c("column1","column2")} to \code{list(column1, column2)}
#' intended for use in a data.table (see \code{\link{data.table}})
#' 
#' @return a list
#' 
#'  @examples
#' library(data.table)
#' first <- data.table(cray=sample(letters,4),"one"=c(1,2,3,4), "two"=c(1,3,5,7))
#' second <- data.table("cray"=sample(letters,35, TRUE))
#' oc <- CJ(one=first[,(one)], cray=second[[1]])
#' sub <- s2c(c("one","cray"))
#' oc[,eval(sub)]
#' 
#' @export
s2c <- function(x, type="list"){
	as.call(lapply(c(type, x), as.symbol))
}





#' Order Dimension 1
#' 
#' Reorder the first dimension of array
#' 
#' @param x an array
#' @param ord vector indicating the order of the elements; for format, see \link{\code{order}}
#' 
#' @details
#' This function is only useful if you don't know the total number of dimensions in the array; if the total number of dimensions are known, it is much faster to simply do \code{x[ord,,]} (for 3d array).
#' 
#' @return a reordered array
#' 
#' @section Note:
#' This function is slow, and there are several alternatives that are far faster. Future versions of the function will upgrade to those. See examples for timings, but note that the faster alternatives do not contain any checks, which adds to their advantage.
#' 
#' @source \url{http://stackoverflow.com/q/32000387/2343633}
#' 
#' @examples
#' arr <- array(1:240, dim=c(40,3,2))
#' orderD1(arr, 40:1)
#' arr[40:1,,]
#' 
#' #' \donttest{
#' 
#' # first alternative
#' # can be even faster if ls0 is allowed to be moved outside
#' # if it can be, is fastest
#' od1 <- function(x, ord){
#' 	ls0 <- list(substitute(x[])[[3]])
#' 	do.call(`[`,c(list(x,ord),rep(ls0,length(dim(x))-1)))
#' }
#' 
#' # second alternative
#' tlm <- function(x, ord){
#' 	do.call(`[`,c(list(x,ord),rep(TRUE,length(dim(x))-1)))
#' }
#' 
#' # library(microbenchmark)
#' # microbenchmark(arr[40:1,,], orderD1(arr, 40:1), od1(arr, 40:1), tlm(arr, 40:1), times=1E3)
#' 
#' # Unit: microseconds
#' #                expr    min      lq      mean  median      uq     max neval  cld
#' #       arr[40:1, , ]  3.287  3.7565  4.318502  3.9240  4.1425 183.473  1000 a
#' #  orderD1(arr, 40:1) 24.595 26.6875 28.706691 27.3930 28.5110 191.236  1000    d
#' #      od1(arr, 40:1)  8.480  9.5665 10.383376 10.1195 10.6590  55.539  1000   c
#' #      tlm(arr, 40:1)  7.431  8.3760  8.994415  8.8275  9.3100  20.897  1000  b
#' }
#' 
#' @export
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
	# dimList <- structure(vector("list",ndim))
	dimList[[1]] <- ord
	for(i in 2:ndim){
		dimList[[i]] <- 1:dims[i]
	}
	do.call("[",c(list(x=x),dimList))
}

#' Length Unique
#' 
#' The length of the vector of unique elements
#' 
#' @param x a vector
#' 
#' @details Very convenient shorthand.
#' @return an integer of the number of unique elements in \code{x}
#' @examples
#' lu(letters)
#' @export
lu <- function(x) length(unique(x))




# Cut years into 9 chunks (or min(n.yrs))
# I can't figure out why this is useful
# cy <- function(x, nc){
# 	if(missing(nc)){nc <- 2}
# 	if(lu(x)>1){
# 		as.character(cut(as.numeric(x), breaks=min(2,nc)))
# 	}else{
# 		unique(x)
# 	}
# }


# Added a check to make sure that names repeated in different tables did 
# not have different values, or were reported as such
# also drops columns duplicated from merging,
# and handles 'conflicts' arising from one column being
# NA, and the other !is.na(), replacing the NA with the non-NA value
# These checks and changes make the function a bit slow
#' Trim Automatic Columns
#' 
#' Trim columns generated automatically during a merge of data sets that share non-"by" columns
#' 
#' @param X a data.table
#' 
#' @details
#' Expects that duplicate column names are given the suffixes ".x" and ".y". The ".y" columns are dropped (thus, columns from the second data set in \code{merge(x, y)}), and the suffix dropped from the name of the ".x" column.
#' The function also checks to make sure that the column being dropped has the same content as the column being retained. If the ".x" column had NA values where the ".y" column had non-NA's, then the ".x" column gets teh values of the ".y" column for those rows. If there are diffences between the two columns for non-NA cases, a \code{\link{message}} is printed indicating as much. 
#' A message is also printed if this function does not find any columns to trim.
#' 
#' @return NULL or nothing; however, has the side affect of change the content of \code{X}.
#' 
#' @examples
#' dt1 <- data.table(a=1:10, b=c(10:2, NA))
#' dt2 <- data.table(a=1:10, b=10:1)
#' dt.m <- merge(dt1, dt2, by="a", all.x=T)
#' trim.autoColumn(dt.m)
#' print(dt.m)
#' 
#' @export trim.autoColumn
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


#' Match Table
#' 
#' Find (via potentially fuzzy matching) values in a table given a lookup reference
#' 
#' @param ref a vector of reference values to lookup
#' @param tbl.ref reference values in the table to be matched to \code{ref}
#' @param tbl.val values to be retrieved from the table when found in association with \code{tbl.ref}
#' @param exact logical, default FALSE; if TRUE, matches of \code{ref} to \code{tbl.ref} needn't be exact
#' 
#' @return
#' A data.table with 4 columns:  
#' \tabular{rlll}{
#'  [,1] \tab ref \tab \code{class(ref)} \tab the reference value\cr
#'  [,2] \tab val \tab \code{class(val)} \tab the value from the table (usually this is the desired output)\cr
#'  [,3] \tab val.src \tab character \tab the source of the match\cr
#'  [,4] \tab tbl.row \tab integer \tab Row of match in the table\cr
#' }
#' 
#' @details When \code{exact=FALSE}, \code{match.tbl} performs a \code{\link{match}}, with the added utility of returning a value in the table (rather than simply the index of the matches). When \code{exact=TRUE}, functions in \code{\link{cull}} are called to reformat \code{ref} and search for a match. If not match is found still, then fuzzy matching is performed via \code{\link{agrep}} on versions of ref that have and have not been formatted via \code{\link{cull}}.
#' 
#' The values of \code{val.src} indicate the amount of fuzziness involved in the match:
#' \tabular{ll}{
#' m1 \tab an exact match\cr
#' m2 \tab exact match after \code{\link{cull}}\cr
#' m3 \tab fuzzy match performed on ref\cr
#' m4 \tab fuzzy match performed on \code{cull(ref)}
#' }
#' Fuzzy matching performed with \code{\link{agrep}}, with arguments \code{ignore.case=T, max.distance=0.25}.
#' @section Warning:
#' I am suspicous that the values returned in \code{tbl.ref} may be in accurate. However, this quality, and the function in general, has not been thoroughly tested. Although use-cases have given desirable results, albeit I think that the fuzzy matching can be a bit too fuzzy (finding matches where there shouldn't be any). Be aware.
#' 
#' @examples
#' library(data.table)
#' tbl <- data.table(animal=c("cats","dogs","elephant","giraffe","monkey","person","Gadus morhua", "Paralichthys dentatus", "Pomatomus saltatrix", "Amphiprioninae"), a=1:10, b=10:1)
#' ref <- c("GADUS MORHUA", "Amphiprion (the computer)", "elehpant", "dogs", "squirrel", "gaus")
#' tbl.ref <- tbl[,animal]
#' match.tbl(ref, tbl.ref, tbl[,animal]) # return what was matched to
#' match.tbl(ref, tbl.ref, tbl[,a]) # return another column
#' 
#' @export match.tbl
match.tbl <- function(ref, tbl.ref, tbl.val, exact=FALSE){
	# ref = gmex[,spp.orig]
	# tbl.ref = taxInfo[,raw.spp]
	# tbl.val = taxInfo[,spp]
	
	nref <- length(ref)
	na.vec <- rep(NA, nref)
	if(!exact){
		# _for_matted ref
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
			m3 <- na.vec
			m3.fill <- rep(FALSE, nref)
		}
	
		if(any(is.na(val))){
			m4 <- na.vec
			nav <- is.na(val)
			m4[nav] <- sapply(ref.for[nav], qa, y=tbl.ref)
			m4.fill <- !is.na(m4)
			val[m4.fill] <- tbl.val[m4[m4.fill]]
		}else{
			m4 <- na.vec
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


# ============================
# = Get a Season from a Date =
# ============================
#' Get Season
#' 
#' Get the season from the date
#' 
#' @param DATES dates formatted as "%Y-%m-%d"
#' 
#' @details
#' Get the season of the year, based on solstices and equinoxes.
#' 
#' @return a character vector of seasons
#' 
#' @seealso \code{\link{strptime}} for date formatting details, \code{\link{clean.columns}} for where this function is implemented.
#' 
#' @source \url{http://stackoverflow.com/a/9501225/2343633}
#' 
#' @export
getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d", tz="GMT") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d", tz="GMT") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d", tz="GMT") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d", tz="GMT") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d", tz="GMT"))

    ifelse (d >= WS | d < SE, "winter",
      ifelse (d >= SE & d < SS, "spring",
        ifelse (d >= SS & d < FE, "summer", "fall")))
}