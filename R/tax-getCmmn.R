
getCmmn <- function(u.sppCorr, oldCmmn=NULL){
		
		# Case 1: previously-found common names not provided, start from scratch
		if(is.null(oldCmmn)){
			print("Previously found common names not provided, searching for all common names")
			flush.console()
			
			# Loop through species names
			cmmn.pb <- txtProgressBar(min=1, max=length(u.sppCorr), style=3) # create progress bar
			for(i in 1:length(u.sppCorr)){
				
				# Look up common names
				t.spp.cmmn00 <- tryCatch( # first try looking in ncbi b/c gives english
					{
						ncbi.check <- sci2comm(u.sppCorr[i], db="ncbi", ask=FALSE, verbose=FALSE)[[1]][1][[1]]
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
				
				# Update progress bar
				setTxtProgressBar(cmmn.pb, i)
			}
			close(cmmn.pb) # close progress bar
			
			# Format data.table
			setkey(spp.cmmn1, sppCorr)
			
			return(spp.cmmn1) # return Case 1
		}
		
		# Case 2: previously found common names supplied, only look up what is needed (new)
		if(!is.null(oldCmmn)){
			
			# Format the oldCmmn data.table			
			setkey(oldCmmn, sppCorr)
			
			new.sppCorr0 <- !u.sppCorr%in%oldCmmn[,sppCorr] & !is.na(u.sppCorr)
			
			# Case 2a: old common names supplied, some of the species don't yet have common names
			if(any(new.sppCorr0)){
				print(paste("Old common names supplied, looking up common names for ", sum(new.sppCorr0), " new spp", sep=""))
				flush.console()
				new.sppCorr <- u.sppCorr[new.sppCorr0]
				cmmn.pb <- txtProgressBar(min=1, max=length(new.sppCorr), style=3) # initialize the progress bar
				for(i in 1:length(new.sppCorr)){
					t.spp.cmmn00 <- tryCatch( # first try looking in ncbi b/c gives english
						{
							ncbi.check <- sci2comm(u.sppCorr[i], db="ncbi", ask=FALSE, verbose=FALSE)[[1]][1][[1]]
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
								} # end 2nd error function
							) # end 2nd try catch
						} # end 1st error function 
					) # end 1st try catch
					
					# Only use English characters in common names
					t.spp.cmmn0 <- t.spp.cmmn00[grepl("[a-zA-Z]", t.spp.cmmn00)][1] # only match common names with english chars
					
					# Put common names in data.table (alongside species names)
					t.spp.cmmn2 <- data.table(sppCorr=new.sppCorr[i], common=t.spp.cmmn0)
					
					# Accumulate common names
					if(i==1){
						spp.cmmn2 <- t.spp.cmmn2
					}else{
						spp.cmmn2 <- rbind(spp.cmmn2, t.spp.cmmn2)
					}
				
					setTxtProgressBar(cmmn.pb, i) # update progress bar
				}
				close(cmmn.pb) # close progress bar
				
				# Format accumulated common names
				setkey(spp.cmmn2, sppCorr) # set key for new common names
				
				# Add newly-found common names to old common names
				spp.cmmn1 <- rbind(oldCmmn, spp.cmmn2) # bind new and old common names
				
				return(spp.cmmn1) # return Case 2a

			# Case 2b
			}else{
				print("Old common names supplied, all species have looked-up common names")
				flush.console()
				return(spp.cmmn1) # return Case 2b
			}
		}

	} # end function

