
#' Clean by Triming Columns
#' 
#' @description
#' Trims columns from a data.table containing trawl data
#' 
#' @param X A \code{data.table} containing trawl data
#' @param cols Character vector of column names to have in the output; if missing, defaults to reasonable set of names.
#' @param c.add A character vector of column names to append to \code{cols}. Default (NULL) adds nothing.
#' @param c.drop A character vector of column names to drop from \code{cols}. Default (NULL) drops nothing.
#' 
#' @return Returns \code{NULL} invisibly. As a side effect, the columns present in the object passed as \code{X} may be altered (removed).  
#' 
#' @details
#' This function will drop 0 or more columns from a data.table passed to it (via \code{X}). If \code{cols} is not supplied, it is set to a reasonable set of column names:  
#' (\code{c("reg", "stratum", "lon", "lat", "year", "season", "datetime", "haulid", "ref", "spp", "common", "taxLvl", "species", "genus", "weight", "cnt", "effort", "wtcpue", "cntcpue", "depth", "btemp", "stemp","keep.row")}). On one hand, 23 columns seems like a lot; however, most regions have far more than this: as of the writing of this documentation, the regions have between 47 and 98 columns.  
#' 
#' Recognizing that users are very likely to want to add or remove columns, but that typing out a full set of names can clutter clode, we added the \code{c.add} and \code{c.drop} arguments. These arguments will add or drop, respectively, names from \code{cols} (and this action is take regardless of whether a vector of names is explicitly passed to \code{cols}, or if the default is used).  
#' 
#' If names are supplied to \code{cols} that do not exist in \code{X}, those columns simply will not be included, without warning. Similarly, if \code{c.add} (\code{c.drop}) tries to add (drop) names to (from) \code{cols} that are not contained in \code{X}, those columns will not be added (dropped), without warning.  
#' 
#' Names passed to \code{c.drop} take precedence over names passed to \code{cols} or \code{c.add}; e.g., if the same name is passed to both \code{c.drop} and \code{c.add}, it will not be included in the final data.table. The choice is somewhat arbitrary, although giving preference to dropping names is consistent with the intended use of the function.  
#' 
#' Finally, duplicate columns will not be returned if a name is supplied to both \code{cols} and to \code{c.add}.  
#'   
#' @examples
#' # use a subset of Aleutian Islands
#' subset.index <- sort(sample(1:nrow(clean.ai),nrow(clean.ai)*0.05))
#' ai.eg <- clean.ai[subset.index] # small subset (5%)
#'  
#' # use defaults
#' ai.trim <- copy(ai.eg) # copy so not affect original
#' clean.trimCol(ai.trim) # uses default
#'  
#' # custom column trim
#' ai.trim2 <- copy(ai.eg) # copy
#' clean.trimCol(ai.trim2)
#'  
#' @import data.table
#' @export clean.trimCol
clean.trimCol <- function(X, cols, c.add=NULL, c.drop=NULL){
	
	if(missing(cols)){
		cols <- c("reg", "stratum", "lon", "lat", "year", "season", "datetime", "haulid", "ref", "spp", "common", "taxLvl", "species", "genus", "weight", "cnt", "effort", "wtcpue", "cntcpue", "depth", "btemp", "stemp","keep.row")
	}
	
	x.n <- names(X)
	c.match <- x.n[match(c(cols,c.add),x.n)]
	cols <- c.match[!is.na(c.match)]
	if(!is.null(c.drop)){
		c.drop <- c.drop[c.drop%in%cols]
	}
	
	cols.use <- cols[!cols%in%c.drop]
	cols.drop <- x.n[!x.n%in%cols.use]
	X[,(cols.drop):=NULL]
	
	cols4order <- cols.use[cols.use%in%names(X)]
	setcolorder(X, cols4order)
	
	invisible(NULL)
	
}