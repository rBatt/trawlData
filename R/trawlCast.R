#' Cast Trawl
#' 
#' Cast a data.table of trawl data to an array
#' 
#' @param x A data.table with column names to be used in \code{formula}, \code{valueName}, and potentially \code{allNA_noSamp}
#'  @param formula Formula describing array dimensions, in order as would be given by \code{\link{dim}}. Passed to \code{formula} in \code{\link{acast}}.
#' @param valueName Column name whose elements will fill the array. Passed to \code{value.var} in \code{\link{acast}}.
#' @param valFill Value to use for filling in missing combinations; defaults to NA.  Passed to \code{fill} in \code{\link{acast}}.
#' @param fixAbsent Logical (default TRUE) to indicate the need to fill one value for no sampling (\code{valFill}), and another for a true absence (\code{valAbsent}). See 'Details'.
#' @param allNA_noSamp A character indicator the column/ dimension, which, if all its elements are NA's, indicates a no-sampling event, as opposed to an absence. When \code{all(is.na(allNA_noSamp))} is FALSE for a combination of the other dimensions in \code{formula}, \code{valAbsent} will be used instead of \code{valFill}.
#' @param valAbsent value to be used in lieu of \code{valFill} to indicate an absence as opposed to not-sampled.
#' @param grandNamesOut Grand dimension names for output array (e.g., \code{names(dimnames(x))})
#' @param ... Other arguments to be passed to \code{\link{acast}}.
#' 
#' @details
#' Many columns in bottom trawl data can be described as summarizing 3 aspects of metadata: when, where, and what. This same logic is expressed in the function \code{\link{aggData}}, which prompts users to conceptualize aggregating trawl data as aggregating at different specificities for time, space, and biological dimensions. In this function's default for \code{formula}, the "where" is described by "stratum" (a sampling site), "when" by "year", and "what" by "spp" (species). The "K" value is a replicate, which could mean either "when" or "what" (and is similar to "haulid" in \code{\link{aggData}}, which describes it as being indicative of both time and space). Given those identifying dimensions, we can then appropriately contextualize a measured value, e.g. "weight". Not all cases need these same dimensions to be in \code{formula} (e.g., if the measured value is bottom temperature ("btemp") the "what" dimension is not needed), which is why this function doesn't impose as much structure on what categories of columns should comprise \code{formula}.
#' 
#' However, it can be useful to think of that structure for \code{formula} when trying to understand the distinction and between elements to be filled with \code{valFill} vs. \code{valAbsent}.
#' 
#' For species data, there is an important distinction between a species not being present, and no sampling occurring. For example, entries for species data often do not include 0's, but 0's are implied for Species X when a site is sampled and no value is reported for Species X, even though a value is reported for other species in this instance and Species X is reported in other sampling events. In this case, the observation is 0, not NA. 
#' 
#' In the context just described, \code{valFill} would be NA (the default); if we wanted to change Species X (-esque) values from NA to 0 (under appropriate conditions), set \code{fixAbsent} to TRUE (default) and \code{valAbsent} to 0 (default). More generally, the \code{allNA_noSamp} argument defines the array dimension(s) that, if all elements are NA while varying \code{allNA_noSamp} and holding other dimensions constant, that the NA values are appropriate and that those NA's should not be switched to \code{valAbsent}when \code{fixAbsent=TRUE}. For the species example given above, the default \code{allNA_noSamp="spp"} would be appropriate. In general, it may be fair to say that \code{allNA_noSamp} should be set to the "what" dimension(s) (as described above), and that \code{valAbsent} should be set to the value taken on by \code{valueName} when a measurement is attempted for a particular factor level of \code{valueName} that is absent.
#' 
#' As implied the previous Details, casting data expands the number of explicit \code{valueName} elements in the data set. This function casts to an array because casting to a data.frame or data.table will take up far more RAM. The the difference in RAM increases with the number of identifying variables and how many unique levels they have (but also depends on whether those identifying variables are encoded as characters, factors, integers, doubles, etc).
#' 
#' @return An array with dimensions equal to the number of unique values in each column in \code{formula}.
#' @export
trawlCast <- function(x, formula=stratum~K~spp~year, valueName="wtcpue", valFill=NA, fixAbsent=TRUE, allNA_noSamp="spp", valAbsent=0, grandNamesOut=c("j","k","i","t"), ...){
	
	xa <- reshape2::acast(x, formula=formula, value.var=valueName, fill=valFill, drop=FALSE, ...)
		
		
	if(fixAbsent){
		formulaNames <- unlist(strsplit(deparse(formula), "\\s*~\\s*"))
		apermDimLogic <- !formulaNames%in%allNA_noSamp
		apermDim <- which(apermDimLogic)
		apermDimNot <- which(!apermDimLogic)
		apermDimNames <- formulaNames[apermDimLogic]
		
		unsamp.JK0 <- apply(xa, apermDim, function(x)all(is.na(x)))
		unsamp.JK <- array(unsamp.JK0, dim=dim(xa)[c(apermDim,apermDimNot)])
		unsamp.JK <- aperm(unsamp.JK, c(apermDim,apermDimNot))
		# apply(unsamp.JK, 2, sum) # how often each K had an NA
		xa.fix <- xa
		fix0 <- (is.na(xa) & !unsamp.JK) # had an NA, but actually sampled (some species non-NA)
		fixNA <- (is.na(xa) & unsamp.JK) # had an NA, probably not sampled (no species non-NA)
		stopifnot(all(is.na(xa[fixNA]))) # making sure unsamp.JK got it right
		xa.fix[fix0] <- 0 # switch NA but sampled to 0's
		stopifnot(all(is.na(xa.fix[fixNA]))) # making sure fix0 got it right
		stopifnot(all((xa.fix[fix0]==0))) # making sure fixNA still right
		
		xa <- xa.fix
	}
	
	names(dimnames(xa)) <- grandNamesOut
	
	return(xa)
	
}
