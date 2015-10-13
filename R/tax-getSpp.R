

getSpp <- function(uspp, oldSpp=NULL){
	# Description:
	# lookup species names using taxize, with option to only lookup names not in a reference data.table of previously-checked names
	
	# Usage:
	# getSpp(uspp, oldSpp=NULL)
	
	# Arguments:
	# uspp: character vector of unique species names to be checked
	# oldSPP: a data.table with a column "spp" for original (unchecked) names, and "sppCorr" for previously checked names
	
	# Details:
	# requires packages data.table and taxize
	# requires a name checking function written by RDB called grb.spp1()
	# currently looks up 1 species name at a time, but function could be modified to look up chunks
	
	# Value:
	# A data.table with 2 columns; "spp" column contains unchecked species names, "sppCorr" contains corrected (checked) species names
	
	# Do some quick checks to make sure required functions and packages are loaded
	require(data.table)
	require(taxize)
	if(!exists("grb.spp1")){
		stop("Need grb.spp1() function to be loaded")
	}
	
	# First case: no looked-up names provided, start from scratch
	if(is.null(oldSpp)){
		print("Old spp names not supplied, looking up all spp names")
		flush.console()
		
		# Break unique species names into chunks (currently trivial)
		uspp.chunks <- as.character(cut(seq_along(uspp), length(uspp))) # right now breaking into chunks of length 1
		u.uspp.chunks <- unique(uspp.chunks) # unique chunks
		
		# Loop through species to look up
		spp.pb <- txtProgressBar(min=1, max=length(u.spp.chunks), style=3) # create progress bar for lookup process
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
		
		setnames(spp.corr1, c("submitted_name", "matched_name2"), c("spp", "sppCorr"))
		
		setkey(spp.corr1, spp)
		# save(spp.corr1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")
	
	} # end first case
	
	
	# Second case: previously-checked names provided alongside list of unique names that might need to be checked
	if(!is.null(oldSpp)){ 
		
		# Check for unique species names to be checked that haven't already been checked
		new.spp0 <- !uspp%in%oldSpp[,spp] # are there any species that we haven't searched yet?
		
		# Case 2a: previously checked names supplied, but not all new names have been checked before
		if(any(new.spp0)){
			print(paste("Old spp names supplied, looking up spp names for ", sum(new.spp0), " new spp", sep=""))
			flush.console()
			new.spp <- uspp[new.spp0] # define the new.spp as the unique species names that haven't been searched
		
			new.spp.chunks <- as.character(cut(seq_along(new.spp), length(new.spp))) # could just use unique(), but this is here if we want to search for more than 1 new.spp at a time (i.e., several new.spp would be passed to gnr_resolve at once)
			u.new.spp.chunks <- unique(new.spp.chunks) # the unique groups of species that will be saerched
		
			spp.pb <- txtProgressBar(min=1, max=length(u.new.spp.chunks), style=3) # progress bar for looping through names
			for(s in seq_along(u.new.spp.chunks)){ # for each group of new species to search (current just each unique species)
		
				t.chunk <- u.new.spp.chunks[s] # get current chunk for names
				t.new.spp <- new.spp[new.spp.chunks==t.chunk] # turn chunks into spp names
				
				t.spp.corr2.names <- gnr_resolve(t.new.spp, stripauthority=TRUE, http="get", resolve_once=TRUE) # check names
				t.spp.corr2 <- data.table(grb.spp1(t.spp.corr2.names)) # format checked names into a data.table
				
				# Accumulate (newly) checked names
				if(s==1){
					spp.corr2 <- t.spp.corr2
				}else{
					spp.corr2 <- rbind(spp.corr2, t.spp.corr2)	
				}
				
				setTxtProgressBar(spp.pb, s) # update progress bar
			
			} # end looping through names to check
			close(spp.pb) # close progress bar
			
			# format accumulated data.table of checked names
			setnames(spp.corr2, c("submitted_name", "matched_name2"), c("spp", "sppCorr"))
			setkey(spp.corr2, spp) # set key for spp.corr2 (should already be set for spp.corr1)

			# Combine newly-checked w/ previously-checked names
			spp.corr1 <- rbind(oldSpp, spp.corr2)
		
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
				spp.corr1 <- spp.corr1[!is.na(sppCorr),]
		
			# Save the new spp.corr1 file, which has been updated with new species
			# save(spp.corr1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")
		
		}else{ # Case 2b: previously checked names supplied, and all current names have already been checked
			print("Previously checked spp names supplied, all current names have been checked previously")
			flush.console()
		} # end case 2b
		
	} # end case 2 (total)
	
	return(spp.corr1) # return checked names as a data.table
}