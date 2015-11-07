
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
#' @export is.species
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
#' @export
rmWhite <- function(x){
	stopifnot(is.data.table(x))
	has.cc <- names(x)[sapply(x, is.character)]
	x[,c(has.cc):=lapply(eval(s2c(has.cc)), str_trim)]
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
#' @export
rm9s <- function(x){
	stopifnot(is.data.table(x))
	for(i in seq_along(x)){
		TRUE.x <- x[[i]]
		TRUE.class <- class(TRUE.x)
		if(TRUE.class=="integer64"){
			set(x, i=which(TRUE.x==-9999L | TRUE.x==-9999.0), j=i, value=as.integer64(NA))
		}else{
			set(x, i=which(TRUE.x==-9999L | TRUE.x==-9999.0), j=i, value=as(NA,Class=TRUE.class))
		}
		
	}
}



