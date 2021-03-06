#' Aggregate Trawl Data
#' 
#' Aggregate trawl data to specified levels of biological, spatial, and temporal dimensions
#' 
#' @param X A data.table containing trawl data
#' @param FUN Function used for aggregating each subset of \code{X}.
#' @param bio_lvl,space_lvl,time_lvl Level of biological, spatial, and temporal specificity used in subsetting. Can be abbreviated. If an abbreviated match is not found, the supplied character is assumed to refer to a column in \code{X}. Each must be of length 1. See 'Details'.
#' @param bioFun,envFun Functions to be applied to measured biological and environmental columns, respectively. Default is to use \code{FUN}.
#' @param bioCols,envCols Character vector specifying names of columns to be considered as measured biological and environmental columns, respectively. 
#' @param metaCols If NULL (default), includes all columns of \code{X} not used by \code{bio_lvl}, \code{space_lvl}, \code{time_lvl}, \code{bioCols}, or \code{envCols}.
#' @param meta.action Method for handling \code{metaCols} variables. "drop" results in the columns being dropped (the default), "unique1" returns the first unique value, "collapse" returns a character of unique values separated by a comma, "lu" returns the number of unique elements (via \code{\link{lu}}), and "FUN" indicates the use of function(s) specified by \code{metaFun}.
#' @param metaFun If \code{meta.action} is "FUN", a function or list of functions to be applied to each column in \code{metaCols} during aggregation. The default, NULL, will return an error if \code{meta.action} is "FUN". If a single function is to be applied to all column, it will be recycled and does not need to be in a list.
#' @param use_nAgg Add a column to output indicating the number of elements aggregated. The name of this column is "nAgg".
#' @param na.rm Logical (default TRUE)
#' 
#' @details
#' In each of \code{bio_lvl}, \code{space_lvl}, and \code{time_lvl}, the default arguments are listed in order of decreasing specificity. Some of these levels are crossed, others nested. Default behavior and side-effects of the organization of these factors are described below, but in general they are not limiting to the user. The default behaviors are meant to be intuitive for common analyses.
#' 
#' The \code{bio_lvl} columns are "crossed" below the "spp" level, thus referring to "sex" also refers to "spp". Avoiding the behavior of implied references can be avoided by, for example, creating a new column called "sex2" and setting \code{bio_lvl="sex2"}. Note in this special case (of "sex"), "spp" is not included in the default of \code{metaCols}, and thus when using \code{bio_lvl="sex"}, "spp" will not be included in \code{metaCols} and will not be affected by \code{meta.action}. However, if \code{bio_lvl="individual"}, internally \code{bio_lvl} is just NULL, referring to no columns, so it may be advisable to include columns like "sex" and "spp" in \code{metaCols} (which would be done anyway in the default of metaCols), and to retain those columns by using a non-default for \code{meta.action}. See Examples.
#' 
#' In \code{time_lvl} all levels above "datetime" are assumed to be crossed, thus referring to "season" will aggregate with a temporal grain of seasons within a year. Specifying \code{time_lvl="season"} does not imply a reference to "year" in the sense that "year" will still be included in \code{metaCols} by default, and thus affected by \code{meta.action} (whose default is "drop"). However, temporal factors are created by adding a new "time_lvl" column in the output data.table. When possible, this new column will be of class POSIXct; otherwise, a character. Note that "day" refers to "day of year".
#' 
#' The levels of \code{space_lvl} are nested, not crossed, and thus their handling is more intuitive than for biological or temporal levels. The only oddity here is the "lon-lat" indicator, which simply indicates that both "lon" and "lat" columns are to be used in aggregation. 
#' 
#' The value "haulid" is special because this column refers to both space and time. In general, space and time are correlated within a region, because different places tend to be sampled at different times.
#' 
#' When \code{meta.action="FUN"}, \code{metaFun} can be a named list to refer to each of \code{metaCols} (or it can just be used as \code{metaFun=length}, e.g., where the same function is applied to all columns). When \code{metaFun} is a list, functions are matched to columns by name, not by order of listing. For example, if \code{metaCols=c("reg","trophicLevel", "lon")}, it might be useful to take the unique values of the regions, the means of the trophic levels, and the first unique value of longitude after rounding to 1 decimal place. In that case, one could do \code{metaFun=list(reg=unique, trophicLevel=mean, lon=function(x)unique(round(x,1)))}.
#' 
#' The argument \code{na.rm} affects any of the functions passed as arguments, even custom functions (so long as they accept "na.rm" as arguments). All functions must accept na.rm as an argument; if a function does not, re-rewrite so it does (e.g., function(x, ...){length(x)}). \code{na.rm} also affects "unique1", "collapse", and "lu" in \code{meta.action}. In intances where the functions \code{\link{mean}} or \code{\link{sum}} are used and \code{na.rm=TRUE}, consider instead using \code{\link{meanna}} and \code{\link{sumna}}, respectively.
#' 
#' @section Note:
#' The use of \code{use_nAgg} is complicated by the fact that when na.rm=TRUE, each column may very well have a different number of aggregated values. So right now, \code{use_nAgg} does not adhere to na.rm=TRUE, and includes NA values in its count. 
#' 
#' @return Returns an aggregated data.table. See 'Details' for columns returned.
#' 
#' @examples
#' trim.neus <- trawlTrim("neus", c.add=c("length","sex"))
#' mini_data <- trim.neus[
#' 	pick(spp, 2, w=TRUE)
#' 	& pick(stratum, 5, w=TRUE)
#' 	& pick(year,5, w=TRUE)
#' ]
#'
#' # aggregate species within a haul (among individuals)
#' # this means taking the sum of many bio metrics
#' # Note that I put 'sex' in metaCols, b/c I don't want
#' # the bioFun applied to it (preferring instead to
#' # take the first unique value as a form of aggregating)
#' neus1 <- trawlAgg(
#' 	X=mini_data,
#' 	bioFun=sumna,
#' 	envFun=meanna,
#' 	bio_lvl="spp", space_lvl="haulid", time_lvl="season",
#' 	bioCols=c("wtcpue","cntcpue"),
#' 	envCols=c("btemp"),
#' 	metaCols=c("reg","common","datetime","stratum","sex"),
#' 	meta.action=c("unique1")
#' )
#'
#' # aggregate within a species within stratum
#' # refer to the time_lvl column from previous trawlAgg()
#' # can use mean for both bio and env
#' neus2 <- trawlAgg(
#' 	X=neus1,
#' 	FUN=meanna,
#' 	bio_lvl="spp", space_lvl="stratum", time_lvl="time_lvl",
#' 	bioCols=c("wtcpue","cntcpue"),
#' 	envCols=c("btemp"),
#' 	metaCols=c("reg","common","datetime"),
#' 	meta.action=c("unique1")
#' )
#'
#' # A more complex example
#' # Say we want the weight, count, and length
#' # Within a stratum, of a given sex of a given species, during a season
#' # To illustrate a complex situation, let's take the
#' # mean of the weight and length, and sum of count
#' # Because only 1 type of function can be applied to bio_Cols,
#' # we can just exercise the extreme flexibility of
#' # metaCols and metaFunwe to achieve goals.
#' # Also, notice how we transform the "datetime" column to year
#' trawlAgg(
#' 	X=mini_data,
#' 	FUN=meanna,
#' 	bio_lvl="individual", space_lvl="stratum",time_lvl="season",
#' 	bioCols=c("weight","length"),
#' 	envCols=c("stemp","btemp", "depth"),
#' 	metaCols=c("datetime","reg", "cnt", "spp", "common", "sex"),
#' 	meta.action=c("FUN"),
#' 	metaFun=list(
#' 	# note that these are named, and don't need
#' 	# to be in the same order as metaCols
#' 		sex = function(x, ...)una(x, ...)[1],
#' 		reg = function(x, ...)una(x, ...)[1], # this is unique1
#' 		datetime = function(x, ...)una(data.table::year(x), ...)[1],
#' 		common = function(x, ...)una(x, ...)[1],
#' 		spp = function(x, ...)una(x, ...)[1],
#' 		cnt = sumna
#' 	)
#' ) # not surprisingly, there wasn't any aggregation at the level of individuals

#' @export
trawlAgg <- function(X, FUN=NULL, bio_lvl=c("individual","sex","spp","species","genus"), space_lvl=c("haulid","lon-lat","lat","lon","stratum","reg"), time_lvl=c("haulid","datetime","day","month","season","year"), bioFun=FUN, envFun=FUN, bioCols=c("wtcpue","cntcpue"), envCols=c("stemp","btemp","depth"), metaCols=NULL, meta.action=c("drop","unique1","collapse", "lu", "FUN"), metaFun=NULL, use_nAgg=TRUE, na.rm=TRUE){
	
	
	# ==========
	# = Checks =
	# ==========
	stopifnot(is.data.table(X))
	stopifnot(!is.null(bioFun) & !is.null(envFun))
	stopifnot(is.logical(use_nAgg))
	stopifnot(is.logical(na.rm))
	
	
	# ======================================
	# = Collect Names and Formal Arguments =
	# ======================================
	X <- copy(X)
	ad.form <- formals(trawlAgg)
	x.names <- names(X)
	
	bio_def <- eval(ad.form$bio_lvl)
	time_def <- eval(ad.form$time_lvl)
	space_def <- eval(ad.form$space_lvl)
	ma_def <- eval(ad.form$meta.action)
	
	
	# ===================
	# = Match Arguments =
	# ===================
	bio_lvl <- match.arg(bio_lvl, choices=c(bio_def,x.names))
	space_lvl <- match.arg(space_lvl, choices=c(space_def,x.names))
	time_lvl <- match.arg(time_lvl, choices=c(time_def,x.names))
	bioCols <- match.arg(bioCols, choices=x.names, several.ok=TRUE)
	envCols <- match.arg(envCols, choices=x.names, several.ok=TRUE)
	if(!is.null(metaCols)){
		metaCols <- match.arg(metaCols, choices=x.names, several.ok=TRUE)
	}
	meta.action <- match.arg(meta.action, choices=ma_def)
	
	
	# =================
	# = Set up Levels =
	# =================
	# set up Bio levels
	if(bio_lvl=="individual"){
		bio_lvl <- "bio_lvl" # individual is by doing no bio agg
		X[,bio_lvl:=paste(spp,haulid, (1:(.N)), sep="."),by=c("spp","haulid")]
	# }else if(bio_lvl%in%bio_def){
	}else if(bio_lvl=="sex"){
		# need to implement hierarchy
		# wherein referring to "sex" also refers to
		# "spp", "species", "genus"
		# bio_lvl <- bio_def[which(bio_def==bio_lvl):length(bio_def)]
		bio_lvl <- c("sex", "spp")
		
	} # if not individual or sex, just leave bio_lvl as-is
	stopifnot(all(bio_lvl%in%x.names))
	
	# set up Space levels
	if(space_lvl=="lon-lat"){
		space_lvl <- c("lon","lat")
	}
	stopifnot(all(space_lvl%in%x.names))
	
	# set up Time levels
	# if(time_lvl%in%time_def & !time_lvl%in%x.names){
	if(time_lvl%in%time_def){
		switch(time_lvl,
			day = X[,time_lvl:=format.Date(datetime, format="%Y-%m-%d")],
			month = X[,time_lvl:=format.Date(datetime, format="%Y-%m")],
			season = X[,time_lvl:=paste(format.Date(datetime, format="%Y"),getSeason(datetime),sep="-")],
			year = X[,time_lvl:=format.Date(datetime, format="%Y")],
			haulid = X[,time_lvl:=haulid]
		)
	}else{
		stopifnot(all(time_lvl%in%x.names))
		invisible(X[,time_lvl:=eval(s2c(time_lvl))])
	}
	
	
	# ===================
	# = Define metaCols =
	# ===================
	byCols <- unique(c(space_lvl, "time_lvl", bio_lvl))
	otherCols <- x.names[!x.names%in%c(byCols,bioCols,envCols)]
	if(is.null(metaCols)){
		metaCols <- otherCols
	}else{
		metaCols <- match.arg(metaCols, choices=otherCols, several.ok=TRUE)
	}
	
	
	# =============================
	# = Match and Check Functions =
	# =============================
	
	# Match Bio and Env Funs
	bioFun <- match.fun(bioFun)
	envFun <- match.fun(envFun)
	
	# Match Meta Fun
	if(meta.action=="FUN"){
		stopifnot(!is.null(metaFun))
		if(length(metaFun)==1){
			metaFun <- match.fun(metaFun)
			metaFun <- list(metaFun)
			
		}else{
			stopifnot(is.list(metaFun))
			stopifnot(length(metaFun)==length(metaCols))
			metaFun <- lapply(metaFun, match.fun)[metaCols] # matches and sorts
		}
	}else{
		
		metaFun <- switch(meta.action,
			drop = function(x)NULL,
			unique1 = function(x)una(x, na.rm=na.rm)[1],
			collapse = function(x)paste(una(x, na.rm=na.rm), collapse=","),
			lu = function(x)lu(x, na.rm=na.rm)
		)
		metaFun <- list(metaFun)
	}
	
	
	# =============
	# = Aggregate =
	# =============
	dc <- function(x,y){do.call(x, list(y))}
	X[,nAgg:=.N,by=c(byCols)]
	out1 <- X[nAgg==1,eval(s2c(c(byCols,bioCols,envCols,metaCols,"nAgg")))]
	out <- X[nAgg!=1,j={
			# I'm still amazed that this works.
			c(
				structure(lapply(eval(s2c(bioCols)), bioFun, na.rm=na.rm), .Names=bioCols),
				structure(lapply(eval(s2c(envCols)), envFun, na.rm=na.rm), .Names=envCols),
				structure(mapply(dc, metaFun, eval(s2c(metaCols)), SIMPLIFY=FALSE), .Names=metaCols),
				"nAgg"=nAgg[1]#if(use_nAgg){(.N)}
			)
	
		},by=c(byCols)
	]
	out <- rbind(out1, out, fill=TRUE)
	if(!use_nAgg){out[,nAgg:=NULL]}
	
	
	# ==========
	# = Return =
	# ==========	
	return(out)
	
}

