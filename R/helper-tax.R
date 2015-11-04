
flag.spp <- function(Z, index){
	kill.action <- function(Z, index, code){
		if(code=="SKIP"){
			# truly skip; leave all flags as-is
		}
		if(code=="no"){
			# nada
			Z[index, flag:="fine"]
		}
		if(code=="m3"){
			Z[index & val.src=="m3",flag:="bad"]
			Z[index & val.src!="m3" | is.na(val.src),flag:="ok"]
		}
		if(code!="no" & code!="m3" & code!="SKIP"){
			num2kill <- which(seq_len(nrow(Z[index])) %in% as.integer(strsplit(code, split=" ")[[1]]))
			Z[index & cumsum(index)%in%num2kill, flag:="bad"]
			Z[index & !cumsum(index)%in%num2kill, flag:="ok"]
		}
	} # end kill.action
	
	
	kill.badKey <- function(Z, index){
		print(Z[index], nrow=Inf)
		kill.code <- readline("There's a conflict in the data entry. Enter one of the following:\n [m3]   flag all val.src==m3 as 'bad'\n [no]   no problem, flag everything as 'fine'\n [0-9]  Enter row #'s to flag as 'bad', with each # sep by a space\n [SKIP] Do nothing; leave flag as-is\n")
		
		kill.action(Z, index, kill.code)
		
	}
	
	invisible(kill.badKey(Z, index))
}



# =========================================
# = Check for and Correct Inconsistencies =
# =========================================
check.consistent <- function(Z, col2check=names(Z)[!names(Z)%in%c(by.col,not.consistent)], by.col="spp", not.consistent=c("ref","flag","val.src","tbl.row","match.src","tax.src")){
	replacementsMade <- 0
	replacementsFailed <- 0
	replacementUnneeded <- 0
	
	taxProblemSolution <- data.table(spp=character(), prob.col=character(), solution=character())
	tPS <- FALSE
	
	for(i in unique(Z[,eval(s2c(by.col))][[1]])){
		for(j in col2check){
			t.out <- Z[i,get(j)] # temporary value for a given `spp` and the j column
			if(lu(t.out)>1){
				if(lu(t.out[!is.na(t.out)])==1){ # if the 2+ values are just an NA and something else
					# then just replace the NA with the something else
					replacementsMade <- replacementsMade + 1L
					Z[i,c(j):=list(unique(t.out[!is.na(t.out)]))]
				}else{ # otherwise, if there are more than 2 non-NA unique values
					prob.spp <- i # prob. means "problem" / "problematic"
					prob.col <- j
					prob.opts <- unique(t.out[!is.na(t.out)])
			

					fix.taxProb <- function(){ # defining in function in loop for readability
						readline(paste(
							"Pick your solution. SKIP to skip, otherwise enter one of the following exactly (don't add quotes, etc.):\n ", 
							paste0(prob.opts, collapse="\n  "), "\n"
						))
					}
			
					if(tPS){ # FALSE; a prompt from when I had this in a script, was TRUE if reading in a file that had some answers
						prob.fix.t <- taxProblemSolution[spp==i & prob.col==j,solution]
						if(length(prob.fix.t)>0){
							prob.fix <- prob.fix.t
						}else{
							print(j)
							print(Z[i])
							prob.fix <- fix.taxProb()
							taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
						}
					}else{
						print(j)
						print(Z[i])
						prob.fix <- fix.taxProb()
						taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
					}
			
			
					if(prob.fix!="SKIP"){ # I've never tested the use of the SKIP response ...
						Z[i,c(j):=list(prob.fix)]
						replacementsMade <- replacementsMade + 1L
					}else{
						replacementsFailed <- replacementsFailed + 1L
					}
			
				}
			}else{
				replacementUnneeded <- replacementUnneeded + 1L
			}
		}

	}
}



# =================
# = check.and.set =
# =================
# Function to see if the corrected version of a bad spp name
# already exists in a data set; if it does,
# then the wrong version is overwritten with the content 
check.and.set <- function(wrong, corrected, Z=spp.key){
	check <- spp.key[spp!=wrong,corrected%in%spp]
	if(check){
		# if the corrected name already exists,
		# make sure to have all the rows with the wrong name match up with the corrected rows,
		# that way if we make any changes, both sets get updated
		# For example, if a name XX is wrong, and Xx is the corrected name,
		# say that we are going to set the trophic level of Xx to 42, 
		# but the current entry is 40. If we were to say 'change all rows
		# with name XX to have a TL to 42, and also switch the bad XX name to the good Xx name',
		# then we would have some rows with TL of 42 (the ones that originally had the bad name), and 
		# some rows with TL of 40 (the ones that originally had the corrected name).
		# Thus, we have to get the names and other content to match before changing anything.
		# Bottom line is that we need to be sure that all things are consistent, and that this requires
		# more care when we are switching the 'spp' of an entry to a 'spp' that is already there.
		noSet <- c("ref", "val.src", "tbl.row", "mtch.src", "website","website2","flag", "conflict", "tax.src2")
		all.but.noSet <- names(Z)[!names(Z)%in%noSet]
		stopifnot(
			all(
				sapply(
					Z[spp==corrected,eval(s2c(all.but.noSet))], 
					function(x)length(unique(x[!is.na(x)]))<=1
				)
			)
		)
		
		# Replace NA's with non-NA's in the same column,
		# where needed, and if possible
		all.vals <- Z[spp==corrected & spp!=wrong,]
		if(any(is.na(all.vals))){
			set2nonNA(Z=Z, index=Z[,spp==corrected&spp!=wrong])
		}
		
		Z[spp==wrong, c(all.but.noSet):=Z[spp==corrected,eval(s2c(all.but.noSet))]]			
	}else{
		# if the corrected name doesn't already exist,
		# then simply switch the wrong name to the corrected name,
		Z[spp==wrong, spp:=corrected]
	}
	
	
	setkey(Z, ref, spp)
	invisible(NULL)
}


# ===========
# = ref2spp =
# ===========
# Function that is useful when
# you see that you need to change the `spp` value for a certain `ref`,
# but the new `spp` value you are switching to is already in spp.key.
# In this case, you could enter everything to match what is already in spp.key,
# but this function just takes your row with ref==Ref,
# and sets its columns to the values already present in the spp==Spp rows.
# It also sets the spp column in the ref==Ref row to be Spp.
ref2spp <- function(Ref, Spp, Z=spp.key){
	check <- Z[ref!=Ref,Spp%in%spp]
	if(check){
		
		noSet <- c("ref", "val.src", "tbl.row", "mtch.src", "website", "website2", "flag", "conflict", "tax.src2")
		all.but.noSet <- names(Z)[!names(Z)%in%noSet]
		stopifnot(
			all(
				sapply(
					Z[spp==Spp & ref!=Ref,eval(s2c(all.but.noSet))],
					function(x)length(unique(x[!is.na(x)]))<=1
				)
			)
		)
		
		# Replace NA's with non-NA's in the same column,
		# where needed, and if possible
		all.vals <- Z[spp==Spp & ref!=Ref,]
		if(any(is.na(all.vals))){
			set2nonNA(Z=Z, index=Z[,spp==Spp&ref!=Ref])
		}
		
		# prepare to insert the Spp values in the Ref rows
		new.vals <- Z[spp==Spp & ref!=Ref,eval(s2c(all.but.noSet))]
		setkey(new.vals, spp)
		new.vals <- unique(new.vals)
		
		# Insert
		Z[ref==Ref, c(all.but.noSet):=new.vals]	
	}else{
		# if the corrected name doesn't already exist,
		# then simply switch the wrong name to the corrected name,
		Z[ref==Ref, spp:=Spp]
	}
	
	# Return
	setkey(Z, ref, spp)
	invisible(NULL)
}


# =============
# = set2nonNA =
# =============
# When a column of a data.frame has 2 unique values,
# an NA, and a non-NA, this function will set all NA values to the non-NA value.
# Z is the name of a full data.table, and index is a vector of TRUE/FALSE 
# indicating which rows of Z are supposed to be examined
# The use-case in mind is where Z is a full data.table,
# and index indicates a portion of the data.table that is supposed to be compared.
# It's done this way to avoid having to subset the data.table in advance,
# that way the replacements can easily be made "in place", thus not soaking up extra memory.
# Similarly, note that thus function changes the data.table passed to the Z argument ... 
# there is no output, but the original data.table will be affected.
set2nonNA <- function(Z, index){
	tn <- names(Z)
	to.set <- sapply(Z[index], function(x)any(is.na(x)) & lu(x[!is.na(x)])==1)
	c.to.set <- tn[to.set]
	if(length(c.to.set)>=1){
		for(i in 1:length(c.to.set)){
			set.vals <- Z[index, unique(eval(s2c(c.to.set[i]))[[1]])]
			set.val <- set.vals[!is.na(set.vals)]
			stopifnot(class(set.val)==Z[index,class(eval(s2c(c.to.set[i]))[[1]])])
			Z[index, c(c.to.set[i]):=list(set.val)]
		}
	}
	
}