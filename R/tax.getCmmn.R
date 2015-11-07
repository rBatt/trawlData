#' Get Common
#' 
#' Get species' common names
#' 
#' @param u.sppCorr character vector of Latin names to lookup
#' 
#' Uses rfishbase (this is new from original implementation), and only uses taxize if rfishbase fails to find a match (but taxize is very slow sometimes). Furture versions should use the sealifebase implementation of rfishbase, which I haven't figured out/ tested yet.
#' 
#' @return a data.table with two columns: the species name searched, and the common name
#' 
#' @example
#' # sometimes it works well, sometimes not
#' getCmmn(c("Paralichthys dentatus", "Pomatomus saltatrix","Gadus morhua"))
#' 
#' @export
getCmmn <- function(u.sppCorr){
	library(rfishbase)
	library(taxize)
	
	use.pb <- length(u.sppCorr)>1
		
	# Loop through species names
	if(use.pb){
		cmmn.pb <- txtProgressBar(min=1, max=length(u.sppCorr), style=3) # create progress bar
		
	}
	for(i in 1:length(u.sppCorr)){
		
		# Look up common names
		t.spp.cmmn00 <- tryCatch(
			{
				rfb.check <- sci_to_common(u.sppCorr[i])[[1]]
				stopifnot(length(rfb.check)>0)
				rfb.check
			}, error=function(cond){
				tryCatch( # first try looking in ncbi b/c gives english
					{ncbi.check <- sci2comm(u.sppCorr[i], db="ncbi", ask=FALSE, verbose=FALSE)[[1]][1][[1]]
						stopifnot(!is.null(ncbi.check))
						ncbi.check
					}, 
					error=function(cond){ # if ncbi fails, ...
						tryCatch( # next try finding the common name in itis
							{sci2comm(u.sppCorr[i], db="itis", ask=FALSE, verbose=FALSE)[[1]]},
							error=function(cond){ # if ncbi and itis fail, ...
								tryCatch( # next look in eol
									{sci2comm(u.sppCorr[i], db="eol", ask=FALSE, verbose=FALSE)[[1]]},
									error=function(cond){NA} # if all of ncbi and itis and eol fail, return NA
								)
							} # end 3rd error function
						) # end 3rd try catch
					} # end 2nd error function 
				) # end 2nd try catch
			} # end 1st error function
		) # end 1st try catch
			
		
			
	
		# Remove non-english characters
		t.spp.cmmn0 <- t.spp.cmmn00[grepl("[a-zA-Z]", t.spp.cmmn00)][1] # only match common names with english chars
		
		# Turn results [i] into a data.table
		t.spp.cmmn1 <- data.table(sppCorr=u.sppCorr[i], common=t.spp.cmmn0) # turn the common match into a data table w/ sppCorr and common as column names
		
		# Accumulate common names
		if(i==1){
			spp.cmmn1 <- t.spp.cmmn1 # create the spp.cmmn1 data.table
		}else{
			spp.cmmn1 <- rbind(spp.cmmn1, t.spp.cmmn1) # or accumulate the spp.cmmn1 entries
		}
		
		if(use.pb){
			# Update progress bar
			setTxtProgressBar(cmmn.pb, i)
		}
		
	}
	if(use.pb){
		close(cmmn.pb) # close progress bar
	}
	
	
	return(spp.cmmn1) # return
	
} # end function

