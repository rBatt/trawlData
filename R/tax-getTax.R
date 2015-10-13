
getTax <- function(sppCorr2, oldTax=NULL, class.names=c("species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom")){


	# Case 1: old taxonomic classification not supplied, starting from scratch
	if(is.null(oldTax)){
		sppCorr3 <- sppCorr2 # just renaming to be consistent w/ Case 2
	
		# Print status indicating Case 1
		print("File of taxonomic levels not found, searching for all")
		flush.console()
	
		# Prepare to loop through each species and look up its taxonomic classification
		tlvl.pb <- txtProgressBar(min=1, max=length(sppCorr3), style=3) # initiate progress bar
		for(i in 1:length(sppCorr3)){
		
			# Look up taxonomic classification
			# run tryCatch to be able to use same subsetting regardless of match find
			t.classification <- tryCatch( # begin NCBI
			{
				t.id <- get_uid(sppCorr3[i], ask=FALSE, verbose=FALSE) # get taxonomic serial number (NCBI)
				classification(t.id, verbose=FALSE)[[1]] # search for classification
			},
				error=function(cond){ # NCBI error function
					tryCatch( # begin ITIS
					{
						t.id <- get_tsn(sppCorr3[i], ask=FALSE, verbose=FALSE) # get taxonomic serial number (ITIS)
						classification(t.id, verbose=FALSE)[[1]] # search for classification
					},
					error=function(cond){ # ITIS error function
						tryCatch( # begin EOL
						{
							t.id <- get_eolid(sppCorr3[i], ask=FALSE, verbose=FALSE) # get serial number (EOL)
							classification(t.id, verbose=FALSE)[[1]] # search for classification
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
				t.taxLvl1 <- data.table(sppCorr=sppCorr3[i], taxLvl=t.taxLvl0) # create a data table
				t.taxLvl1[,(class.names):=NA] # create empty columns for classification
				t.taxLvl1[,t.c1[t.c1.ind]:=as.list(t.classification[t.c1.ind,1])] # fill in empty classification columns where found
		
			# If the temporary classification is NA, 
			}else{ # else, if the call to classification() or get_tsn() failed, then
				t.taxLvl0 <- NA # leave taxLvl as NA
				t.taxLvl1 <- data.table(sppCorr=sppCorr3[i], taxLvl=t.taxLvl0) # record corrected spp name, and NA tax lvl
				t.taxLvl1[,(class.names):=NA] # and the classification will be left as NA
			}
		
			# Accumulate classifications
			if(!exists("taxLvl1")){
				taxLvl1 <- t.taxLvl1 # create the spp.cmmn1 data.table
			}else{
				taxLvl1 <- rbind(taxLvl1, t.taxLvl1) # or accumulate the spp.cmmn1 entries
			}
		
			# Update progress bar
			setTxtProgressBar(tlvl.pb, i)
		}
		close(tlvl.pb) # close progress bar
	
		# Format data.table
		setkey(taxLvl1, sppCorr)
	
		# Store data.table for output
		updated.taxLvl <- taxLvl1 # renaming for consistency with Case 2 where needs to rbind() with oldTax
	
	}

	# Case 2: Old taxonomic classifications supplied
	if(!is.null(oldTax)){ # if previous classfication is supplied ...

		# Format old data.table
		setkey(oldTax, sppCorr)
	
		# Identify which species need to be classified
		new.sppCorr0 <- !sppCorr2%in%oldTax[,sppCorr] & !is.na(sppCorr2)
	
		# Case 2a: old classification supplied, and new species to be classified
		if(any(new.sppCorr0)){
		
			# Print indicator of Case 2
			print(paste("Old taxonomic levels supplied, looking up taxonomic level for ", sum(new.sppCorr0), " new spp", sep=""))
			flush.console()
		
			# Get species that need to be classified
			sppCorr3 <- sppCorr2[new.sppCorr0]
		
			# Prepare to loop through species that need to be classified
			tlvl.pb <- txtProgressBar(min=1, max=length(sppCorr3), style=3) # initiate progress bar
			for(i in 1:length(sppCorr3)){ # begin to loop through each species
			
				# Look up taxonomic classification
				t.classification <- tryCatch( # begin NCBI
				{
					t.id <- get_uid(sppCorr3[i], ask=FALSE, verbose=FALSE) # get taxonomic serial number (NCBI)
					classification(t.id, verbose=FALSE)[[1]] # search for classification
				},
					error=function(cond){ # NCBI error function
						tryCatch( # begin ITIS
						{
							t.id <- get_tsn(sppCorr3[i], ask=FALSE, verbose=FALSE) # get taxonomic serial number (ITIS)
							classification(t.id, verbose=FALSE)[[1]] # search for classification
						},
						error=function(cond){ # ITIS error function
							tryCatch( # begin EOL
							{
								t.id <- get_eolid(sppCorr3[i], ask=FALSE, verbose=FALSE) # get serial number (EOL)
								classification(t.id, verbose=FALSE)[[1]] # search for classification
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
					t.taxLvl1 <- data.table(sppCorr=sppCorr3[i], taxLvl=t.taxLvl0) # create a data table
					t.taxLvl1[,(class.names):=NA] # create empty columns for classification
					t.taxLvl1[,t.c1[t.c1.ind]:=as.list(t.classification[t.c1.ind,1])] # fill in empty classification columns where found
		
				# If the temporary classification is NA, 
				}else{ # else, if the call to classification() or get_tsn() failed, then
					t.taxLvl0 <- NA # leave taxLvl as NA
					t.taxLvl1 <- data.table(sppCorr=sppCorr3[i], taxLvl=t.taxLvl0) # record corrected spp name, and NA tax lvl
					t.taxLvl1[,(class.names):=NA] # and the classification will be left as NA
				}
		
				# Accumulate classifications
				if(!exists("taxLvl1")){
					taxLvl1 <- t.taxLvl1 # create the spp.cmmn1 data.table
				}else{
					taxLvl1 <- rbind(taxLvl1, t.taxLvl1) # or accumulate the spp.cmmn1 entries
				}
		
				# Update progress bar
				setTxtProgressBar(tlvl.pb, i)
			}
			close(tlvl.pb) # close progress bar
	
			# Format data.table
			setkey(taxLvl1, sppCorr)
		
			# Update classification
			updated.taxLvl <- rbind(oldTax, taxLvl1) # combine old and new classifications
		
		# Case 2b: old classification supplied, and covers all current species (nothing to do)
		}else{ # (ending case 2a, beginning 2b)
			# Indicate Case 2b has been chosen
			print("Old taxonomic levels supplied, but no new species to classify")
			flush.console()
		
			# Set output object as original classification
			updated.taxLvl <- oldTax
		} # end case 2b
	
	
	} # end case 2
} # end function