
# ====================================
# = Functions for cleaning spp names =
# ====================================
#' Cull Bad Species Characters
#' 
#' Remove/ fix characters that are unlikely to be part of a species name
#' 
#' @param x A character vector
#' 
#' @details
#' \code{cull} performs the following corrections, in this order:  
#' 1. remove extra spaces
#' 2. change to sentence case
#' 3. remove generic species indicators (spp or Sp., e.g.)
#' 4. Remove everything in parentheses
#' 5. Remove words after the first 2
#' 
#' @return a character vector that has been altered by removing content unlikely to belong to a species name.
#' 
#' @seealso \code{\link{clean.tax}} \code{\link{clean.trimRow}}
#' 
#' @export
cull <- function(x) cullPost2(cullParen(cullSp(fixCase(cullExSpace(x)))))

#' @describeIn cull Fix case to sentence case
fixCase <- function(x){
	s <- paste(toupper(substring(x, 1, 1)), substring(x, 2), sep="")
	paste(substring(s, 1, 1), tolower(substring(s, 2)), sep="")
}

#' @describeIn cull Remove extra spaces
cullExSpace <- function(x){
	gsub("\\s+", " ", x)
}

#' @describeIn cull Remove generic species indicator
cullSp <- function(x){
	gsub("\\s(s[p]{1,2}|unid)\\..*", "", x)
}

#' @describeIn cull Remove parentheses and contents
cullParen <- function(x){
	gsub("\\s?\\(.*\\)", "", x)
}

#' @describeIn cull Remove words after the last two
cullPost2 <- function(x){
	gsub("^(\\b[A-Za-z]{1,}\\b\\s+)(\\b[A-Za-z]{1,}\\b).*", "\\1\\2", x)
}

#' Is Species
#' 
#' Are there are least two words in this string?
#' 
#' @param x character vector
#' 
#' @return
#' logical vector of same length as x
#' @export
is.species <- function(x){
	sapply(strsplit(x, " "), length) >= 2
}

#' Remove White Space
#' 
#' Remove white space from a data.table
#' 
#' @param x a data.table
#' 
#' @details
#' Removes white space from the columns of a data.table that are characters (tested by \code{is.character}). Affects the data.table passed to \code{x}.
#' 
#' @return
#' Nothing, but has the side affect of impacting whatever object was passed as \code{x}.
#' 
#' @seealso \code{\link{rm9s}} \code{\link{clean.format}}
#' 
#' @export
rmWhite <- function(x){
	stopifnot(is.data.table(x))
	has.cc <- names(x)[sapply(x, is.character)]
	x[,c(has.cc):=lapply(eval(s2c(has.cc)), stringr::str_trim)]
}


#' Remove 9's
#' 
#' Remove 9's and switch them to NA
#' 
#' @param x A data.table
#' 
#' @details
#' All instances of -9999 (numeric or integer) are replaced as NA's of the appropriate class. Checks also for class "integer64".
#' 
#' @return Nothing, but affects data.table passed as \code{x}.
#' 
#' @seealso \code{\link{rmWhite}} \code{\link{clean.format}}
#' 
#' @export
rm9s <- function(x){
	stopifnot(is.data.table(x))
	for(i in seq_along(x)){
		TRUE.x <- x[[i]]
		TRUE.class <- class(TRUE.x)
		if(TRUE.class=="integer64"){
			set(x, i=which(TRUE.x==-9999L | TRUE.x==-9999.0), j=i, value=bit64::as.integer64(NA))
		}else{
			set(x, i=which(TRUE.x==-9999L | TRUE.x==-9999.0), j=i, value=as(NA,Class=TRUE.class))
		}
		
	}
}


#' Make it an ASCII 'Character'
#' 
#' Turn factor or character columns of a data.table into ASCII characters
#' 
#' @param X a data.table containing columns to be converted
#' 
#' @details
#' Dual functionality: turn factors into a characters, and ensure those characters are encoded as ASCII. Converting to ASCII relies on the \code{stringi} package, particularly  \code{stringi::stri_enc_mark} (for detection of non-ASCII) and \code{stringi::stri_enc_toascii} (for conversion to ASCII).
#' 
#' This function is used when resaving data sets when building the package to ensure that it is portable.
#' 
#' @return NULL (invisibly), but affects the contents of the data.table whose name was passed to this function
#' 
#' @export
makeAsciiChar <- function(X){
	stopifnot(is.data.table(X))
	
	# Check if there are factors to be converted
	isfactor <- sapply(X, is.factor)
	
	# If there are factors, convert them to characters
	if(any(isfactor)){
		has.fc <- names(X)[isfactor]
		invisible(X[,c(has.fc):=lapply(eval(s2c(has.fc)), as.character)])
	}
	
	# Check to see if there are any character columns
	# whose encodings need to be checked
	nm.char.names <- names(X)[sapply(X, is.character)]
	if(length(nm.char.names)>0){
		
		# If there are character columns, check encodings
		encs <- sapply(X[,eval(s2c(nm.char.names))], function(x)any(stringi::stri_enc_mark(x[!is.na(x)])!="ASCII"))
		if(any(encs)){
			cols2conv <- names(encs)[encs] # which columns have non-ASCII?
			X[,(cols2conv):=lapply(eval(s2c(cols2conv)), stringi::stri_enc_toascii)] # convert to ASCII where needed
		}
		
	}
	
	invisible(NULL)
}


#' Get Date
#' 
#' Robust conversion of variably formatted characters to dates
#' 
#' @param x vector of character dates
#' @param orders \code{\link{lubridate}} formatting possibilities. If missing, selects a reasonable default (see Details).
#' @param year 4 digit integer, the oldest year expected in  \code{x}; default is 1957. See Note.
#' @param tz time zone of output; default is GMT (in GMT no times missing due to daylight savings, so more robust)
#' @param ... arguments to pass to \code{\link{lubridate::parse_date_time}}
#' 
#' @details
#' See \code{\link{lubridate::parse_date_time}} for a summary of how to specify \code{orders}. Examples show a conversion of variable formats. The only reason this function exists is that \code{parse_date_time} did not handle the century very well on some test data.
#' 
#' The default \code{orders} is 
#' \code{paste0(
#' 	rep(c("ymd", "mdy", "Ymd", "mdY"),each=5), 
#' 	c(" HMS"," HM", " H", "M", "")
#' )}
#' 
#' @section Note:
#' In 2056 I will turn 70. At that point, I'll still be able to assume that a date of '57 associated with an ecological field observation was probably made in 1957. If I see '56, I'll round it up to 2056. I'll probably retire by the time I'm 70, or hopefully someone else will have cleaned up the date formats in all ecological data sets by that time. Either way, it is in my own self interest to set the default as `year=1957`; I do not currently use very many data sets that begin before 1957 (and none of such vast size that I need computer code to automate the corrections), and as a result, the default 1957 will continue to work for me until I retire. After that, a date of '57 that was actually taken in 2057 will have its date reverted to 1957. Shame on them.
#' 
#' Oh, and the oldest observation in this package is 1958, I believe (the soda bottom temperatures). As for trawl data, NEUS goes back to 1963. So 1957 is a date choice that will work for all dates currently in this package, and given a 1 year buffer, maximizes the duration of the appropriateness of this default for these data sets into the future.
#' 
#' @return a vector of dates formatted as POSIXct
#' 
#' @examples
#' test <- c(
#' 	"2012-11-11", "12-5-23", "12/5/86",
#' 	"2015-12-16 1300", "8/6/92 3:00", 
#' 	"11/6/14 4", "10/31/14 52", "06/15/2014 14:37:01", 
#' 	"2/10/06", "95-06-26", "82-10-03", 
#' 	"11/18/56 2:30:42pm", "11/18/57 1:00", "11/18/58"
#' )
#' getDate(test, orders=orders, truncated=3) # default orders ignores pm
#' 
#' @export
getDate <-  function(x, orders, year=1957, tz="GMT", ...){
	requireNamespace("lubridate", quietly=TRUE)
	year <- as.integer(year)
	stopifnot(nchar(year) == 4L)
	if(missing(orders)){
		# orders <- paste0(rep(c("ymd", "mdy", "Ymd", "mdY"),each=5), c(" HMS"," HM", " H", "M", ""))
		orders <- paste0(rep(c("mdy", "mdY", "ymd", "Ymd"),each=5), c(" HMS"," HM", " H", "M", ""))
	}
	
	x <- lubridate::parse_date_time(x, orders=orders, tz=tz, ...)
	yx <- lubridate::year(x)
	# yxl <- yx < 100
	# if(any(yxl)){
	#   m <- yx[yxl]
	# 	lubridate::year(x)[yxl] <- ifelse(m >= year %% 100, 1900+m, 2000+m)
	# }
	m <- yx %% 100
	lubridate::year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

