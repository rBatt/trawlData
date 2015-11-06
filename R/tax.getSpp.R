#' Get Species
#' Lookup, and correct, species names
#' 
#' @param uspp: character vector of unique species names to be checked
#' 
#' @details
#' Lookup species names using taxize, with option to only lookup names not in a reference data.table of previously-checked names. Currently looks up 1 species name at a time, but function could be modified to look up chunks. Relies heavily on taxize.
#' 
#' @return
#' A data.table with 2 columns; "spp" column contains unchecked species names, "sppCorr" contains corrected (checked) species names
#' 
#' @seealso \code{\link{getCmmn}}, \code{\link{getTax}}, \code{\link{getTL}}, \code{\link{updateDB}}
#' 
#' @export
getSpp <- function(uspp){
	
	# Break unique species names into chunks (currently trivial)
	uspp.chunks <- as.character(cut(seq_along(uspp), length(uspp))) # right now breaking into chunks of length 1
	u.uspp.chunks <- unique(uspp.chunks) # unique chunks
	
	# Loop through species to look up
	spp.pb <- txtProgressBar(min=1, max=length(u.uspp.chunks), style=3) # create progress bar for lookup process
	for(s in seq_along(u.uspp.chunks)){ # for each chunk ...
	
		# Define chunks and get current species to check
		t.chunk <- u.uspp.chunks[s] # get the cut() reslt corresponding to current chunk of species to look up
		t.uspp <- uspp[uspp.chunks==t.chunk] # turn the chunk name into species names
		
		# Look up current spcies
		t.spp.corr1.names <- gnr_resolve(t.uspp, stripauthority=TRUE, http="get", resolve_once=TRUE) # check w/ taxize
		t.spp.corr1 <- data.table(grb.spp1(t.spp.corr1.names)) # store checked name in data.table
		
		# Accumulate lookup results
		if(s==1){ # if first iteration, create spp.corr1
			spp.corr1 <- t.spp.corr1
		}else{ # otherwise rbind() to accumulate
			spp.corr1 <- rbind(spp.corr1, t.spp.corr1)	
		}
		
		setTxtProgressBar(spp.pb, s) # update progress
	
	} # exit looping through lookup
	
	close(spp.pb) # close progress bar
	
	setnames(spp.corr1, c("submitted_name", "matched_name"), c("spp", "sppCorr"))
	
	
	# ===========================
	# = Some manual corrections =
	# ===========================
	# spp.corr1[is.na(sppCorr)]
	spp.corr1[spp=="Antipatharian", sppCorr:="Antipatharia"]
	spp.corr1[spp=="Gorgonian", sppCorr:="Gorgonacea"]
	spp.corr1[spp=="Gymothorax igromargiatus", sppCorr:="Gymnothorax nigromargiatus"]
	spp.corr1[spp=="Micropaope uttigii", sppCorr:="Micropanope nuttingi"]
	spp.corr1[spp=="Neptheid", sppCorr:="Neptheidae"]
	spp.corr1[spp=="Ogocephalidae", sppCorr:="Ogcocephalidae"]
	spp.corr1[spp=="Raioides", sppCorr:="Raioidea"]
	spp.corr1[spp=="Seapen", sppCorr:="Pennatulacea"]
	spp.corr1[spp=="Eoraja siusmexicaus", sppCorr:="Neoraja sinusmexicanus"]
	
}



#' @describeIn getSpp Count the number of N's in a word
countN <- function(x){ # count the number of times the letter "n" appears
	sapply(strsplit(x,""), FUN=function(x)length(grep("n",x)))
}

#' @describeIn getSpp Grab Species (helper function)
grb.spp1 <- function(x) {
	tryCatch(
		{
			# x <- x$results
			x <- x[!duplicated(x[,"matched_name"]),]
			adjN <- pmax(countN(x$matched_name) - countN(x$submitted_name), 0)*0.01 # gets bonus match score if the matched name has more n's, because n's appear to be missing a lot
			x$score <- x$score + adjN
			x <- x[max(which.max(x[,"score"]),1),c("submitted_name","matched_name")]
			if(x[,"matched_name"]==""){x[,"matched_name"] <- NA}
			return(x)
		}, 
		error=function(cond){
			tryCatch(
				{
					data.frame(submitted_name=x$results[1, "submitted_name"], matched_name=as.character(NA))
				},
				error=function(cond){data.frame(submitted_name=NA, matched_name=NA)}
			)
		}	
	)
}