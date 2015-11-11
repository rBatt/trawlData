#' Trim Trawl Data
#' Get a trimmed data set for one or more regions
#' 
#' @param reg a list of data.tables or a character vector of regions to be in data set. If a list of data.tables, trimming and combining is performed on those data.tables. If a charcter vector of region names, package data sets by the name of \code{paste0("clean.", reg)} are used. If nothing is supplied, \code{reg} is a character vector of all region names.
#' @param ... arguments to be passed to \code{\link{clean.trimCol}}
#' 
#' @details 
#' This function drops columns and deletes rows. Column dropping is performed by \code{\link{clean.trimCol}}, rows to drop determined by flagging performed by \code{\link{clean.trimRow}}.  
#' 
#' If multiple regions are supplied to \code{reg}, those regions will be collated via \code{\link{rbindlist}}. This is possible because trimming columns standardizes content. Additionally, although \code{\link{clean.trimRow}} adds the \code{keep.row} column, this function will actually drop those rows for which \code{keep.row} is FALSE.
#' 
#' @examples
#' # default usage
#' trimData(c("ebs","goa"))
#' # usage to get an extra column
#' trimData(c("ebs","shelf"), c.add="WIND")
#' 
#' @export trimData
trimData <- function(reg, ...){
	reg.opts <- c("ai","ebs","gmex","goa","neus","newf","sa","sgulf","shelf","wcann","wctri")
	
	
	# Turn character 
	if(missing(reg)){
		reg <- paste0("clean.",reg.opts)
	}else if(!is.list(reg)){ # data.tables are lists; so are lists of data.tables
		reg <- match.arg(reg, choices=reg.opts, several.ok=TRUE)
		reg <- paste0("clean.",reg)
		reg <- lapply(reg, function(x)copy(get(x)))
	}
	# if(!is.list(reg)){
	# 	reg <- lapply(reg, function(x)copy(get(x)))
	# }
	# print(str(reg))
	
	if(is.data.table(reg)){
		reg <- list(reg)
	}
	stopifnot(is.list(reg) & !is.data.table(reg))
	stopifnot(all(sapply(reg, is.data.table)))
	
	# trim columns
	# if(length(reg)>1){
		# stopifnot(all(sapply(reg, is.data.table)))
		# lcd.names <- Reduce(intersect, lapply(reg, names))
# 		equal.names <- Reduce(setequal, lapply(reg, names))
# 		if(!eaual.names){
# 			warning(paste(c("Data sets in reg do not have the same column names, and are being trimmed to the following shared columns before passing to clean.trimCol:\n",lcd.names), collapse="    "))
# 			invisible(lapply(reg, function(X){(X[,(names(X)[!names(X)%in%lcd.names]):=NULL])}))
# 		}
		
		
		
	# }
	lapply(reg, clean.trimCol, ...)
	z <- rbindlist(reg, use.names=TRUE, fill=TRUE)
	
	# drop rows
	z <- z[(keep.row)][,keep.row:=NULL]
	
	return(z)
	
}

