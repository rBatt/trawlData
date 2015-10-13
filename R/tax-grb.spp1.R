

countN <- function(x){ # count the number of times the letter "n" appears
	sapply(strsplit(x,""), FUN=function(x)length(grep("n",x)))
}
grb.spp1 <- function(x) {
	tryCatch(
		{
			x <- x$results
			x <- x[!duplicated(x[,"matched_name2"]),]
			adjN <- pmax(countN(x$matched_name2) - countN(x$submitted_name), 0)*0.01 # gets bonus match score if the matched name has more n's, because n's appear to be missing a lot
			x$score <- x$score + adjN
			x <- x[max(which.max(x[,"score"]),1),c("submitted_name","matched_name2")]
			if(x[,"matched_name2"]==""){x[,"matched_name2"] <- NA}
			return(x)
		}, 
		error=function(cond){
			tryCatch(
				{
					data.frame(submitted_name=x$results[1, "submitted_name"], matched_name2=as.character(NA))
				},
				error=function(cond){data.frame(submitted_name=NA, matched_name2=NA)}
			)
		}	
	)
}