#' Get Taxonomy
#' 
#' Get the taxonomic classification for a Latin name
#' 
#' @param sppCorr a scientific name; character vector
#' @param class.names the taxonomic classification names to search for and return
#' 
#' @details
#' Uses taxize. Tries several data bases
#' 
#' @return
#' A data.table with columns corresponding to the taxonomic level, the original species name, and \code{taxLvl} as the most specific (lowest) taxonomic level for which a match was found.
#' 
#' @seealso \code{\link{getCmmn}}, \code{\link{getTax}}
#' 
#' @examples
#' getTax(c(
#' 	"Chionoecetes tanneri"
#' ))
#' 
#' @export
getTax <- function(sppCorr, class.names=c("species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom")){



	# Prepare to loop through each species and look up its taxonomic classification
	if(length(sppCorr)>1){
		tlvl.pb <- txtProgressBar(min=1, max=length(sppCorr), style=3) # initiate progress bar
	}
	for(i in 1:length(sppCorr)){
	
		# Look up taxonomic classification
		# run tryCatch to be able to use same subsetting regardless of match find
		t.classification <- tryCatch( # begin NCBI
		{
			t.id <- taxize::get_uid(sppCorr[i], ask=FALSE, verbose=FALSE) # get taxonomic serial number (NCBI)
			classification(t.id, verbose=FALSE)[[1]] # search for classification
		},
			error=function(cond){ # NCBI error function
				tryCatch( # begin ITIS
				{
					t.id <- taxize::get_tsn(sppCorr[i], ask=FALSE, verbose=FALSE) # get taxonomic serial number (ITIS)
					taxize::classification(t.id, verbose=FALSE)[[1]] # search for classification
				},
				error=function(cond){ # ITIS error function
					tryCatch( # begin EOL
					{
						t.id <- taxize::get_eolid(sppCorr[i], ask=FALSE, verbose=FALSE) # get serial number (EOL)
						taxize::classification(t.id, verbose=FALSE)[[1]] # search for classification
					}, 
					error=function(cond){ # EOL error function
						as.character(NA)
					} # end EOL error function
					) # end EOL tryCatch
				} # end ITIS error function
				) # end ITIS tryCatch
		} # end NCBI error function
		) # end NCBI tryCatch
	
		# The next steps can only be taken if t.classification was sucessful (no NA)
		if(any(!is.na(t.classification))){ # if an NA was returned from the classification 
			t.taxLvl0 <- tail(t.classification[,2], 1) # 2nd column contains level of classification, tail(,1) to get most specific
		
			# Organize extracted classification
			t.c1 <- tolower(t.classification[,2]) # column 2 of classification (taxnomic level), in lower case
			t.c2 <- t.classification[,1] # column 1 of classification, which is latin name
			t.c1.ind <- t.c1%in%class.names # index of which levels of classification should be extracted
	
			# Store classification in data.table
			t.taxLvl1 <- data.table(sppCorr=sppCorr[i], taxLvl=t.taxLvl0) # create a data table
			t.taxLvl1[,(class.names):=NA] # create empty columns for classification
			t.taxLvl1[,t.c1[t.c1.ind]:=as.list(t.classification[t.c1.ind,1])] # fill in empty classification columns where found
	
		# If the temporary classification is NA, 
		}else{ # else, if the call to classification() or get_tsn() failed, then
			t.taxLvl0 <- NA # leave taxLvl as NA
			t.taxLvl1 <- data.table(sppCorr=sppCorr[i], taxLvl=t.taxLvl0) # record corrected spp name, and NA tax lvl
			t.taxLvl1[,(class.names):=NA] # and the classification will be left as NA
		}
	
		# Accumulate classifications
		if(!exists("taxLvl1")){
			taxLvl1 <- t.taxLvl1 # create the spp.cmmn1 data.table
		}else{
			taxLvl1 <- rbind(taxLvl1, t.taxLvl1) # or accumulate the spp.cmmn1 entries
		}
	
		# Update progress bar
		if(length(sppCorr)>1){
			setTxtProgressBar(tlvl.pb, i)
		}
		
	}
	if(length(sppCorr)>1){
		close(tlvl.pb) # close progress bar
	}
	
	# Store data.table for output
	updated.taxLvl <- taxLvl1 # renaming for consistency with Case 2 where needs to rbind() with oldTax
	
	return(updated.taxLvl)
} # end function
