#' Pick example values
#' 
#' Given a characher vector, take samples from that vector, and then the unique value of those samples
#' 
#' @param x A character vector from which to pick
#' @param n How many samples to pick
#' @param value Logical, return value? Default FALSE returns logical index index
#' @param w Logical, weight by the frequency occurence of each unique value? (default TRUE )
#' @param na.rm Logical, remove NA's from consideration?
#' 
#' @details
#' The anticipated use-case related to trawl data is subsampling a data set.
#' 
#' @seealso \code{\link{mpick}}
#' 
#' @return Either a logical vector of same length as \code{x}, or a a vector with same class as \code{x} and length \code{n}
#' 
#' @examples
#' # =========
#' # = Basic Use =
#' # =========
#' # simple example
#' pick(letters, 5)
#' 
#' # more complicated
#' # make suitable vector to illustrate
#' v <- trunc(rlnorm(1E3))
#' ind2NA <- sample.int(n=length(v), size=length(v)/2)
#' v[ind2NA] <- NA_real_ # set half to NA
#' 
#' # repeat simple use
#' set.seed(42)
#' ind <- pick(v, 2)
#' 
#' # show value from simple use
#' v[ind]
#' 
#' 
#' # get simple use unique value directly
#' set.seed(42)
#' pick(v, 2, value=TRUE)
#' 
#' 
#' # allow sampling of NA's, and weight
#' set.seed(42)
#' pick(x=v, n=2, value=TRUE, na.rm=TRUE, w=TRUE)
#' 
#' # ===============
#' # = E.g. for Trawl Data =
#' # ===============
#' expr <- expression(
#' 	pick(spp, 3, w=TRUE)
#' 	& pick(year, 2)
#' 	& keep.row
#' )
#' mini_data <- clean.ai[eval(expr)]
#' # When combining logical vectors with &,
#' # might end up with less than n;
#' # if you combine with |, likely to end up w/ more
#' # (could subset iteratively to ensure)
#' mini_data[,list(nSpp=lu(spp), nY=lu(year))]
#' 
#' @export
pick <- function(x, n, value=FALSE, w=FALSE, na.rm=TRUE){
	if(!w){
		val <- sample(una(x, na.rm=na.rm), n)
		if(value){
			return(val)
		}else{
			x%in%val
		}
	}else{
		tbl <- table(x, useNA=c("ifany", "no")[(na.rm+1L)])
		val <- sample(names(tbl), size=n, prob=tbl)
		ind <- x%in%val
		if(value){
			return(unique(x[ind]))
		}else{
			ind
		}		
	}
}