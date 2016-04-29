flagSpp <- function(Z, index){
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
checkConsistent <- function(Z, col2check=names(Z)[!names(Z)%in%c(by.col,not.consistent)], by.col="spp", not.consistent=c("ref","flag","val.src","tbl.row","mtch.src","tax.src","website","website2","tax.src2","conflict")){
	replacementsMade <- 0
	replacementsFailed <- 0
	replacementUnneeded <- 0
	
	taxProblemSolution <- data.table(spp=character(), prob.col=character(), solution=character())
	tPS <- FALSE
	
	for(i in unique(Z[,eval(s2c(by.col))][[1]])){
		for(j in col2check){
			t_ind <- Z[,eval(s2c(by.col))[[1]]==i & !is.na(eval(s2c(by.col))[[1]])]
			t.out <- Z[t_ind,get(j)] # temporary value for a given `spp` and the j column
			if(lu(t.out)>1){
				if(lu(t.out[!is.na(t.out)])==1){ # if the 2+ values are just an NA and something else
					# then just replace the NA with the something else
					replacementsMade <- replacementsMade + 1L
					Z[t_ind,c(j):=list(unique(t.out[!is.na(t.out)]))]
				}else{ # otherwise, if there are more than 2 non-NA unique values
					prob.spp <- i # prob. means "problem" / "problematic"
					prob.col <- j
					prob.opts <- una(t.out) #unique(t.out[!is.na(t.out)])
			

					fix.taxProb <- function(){ # defining in function in loop for readability
						po_opts <- paste0(1:length(prob.opts), ": ", prob.opts)
						po_ind <- readline(paste(
							"Pick your solution. SKIP to skip, otherwise pick the number you want:\n ", 
							paste0(po_opts, collapse="\n  "), "\n"
						))
						prob.opts[as.integer(po_ind)]
					}
					
					if(tPS){ # FALSE; a prompt from when I had this in a script, was TRUE if reading in a file that had some answers
						prob.fix.t <- taxProblemSolution[eval(s2c(by.col))[[1]]==i & prob.col==j,solution]
						if(length(prob.fix.t)>0){
							prob.fix <- prob.fix.t
						}else{
							print(j)
							print(Z[eval(s2c(by.col))[[1]]==i])
							prob.fix <- fix.taxProb()
							taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
						}
					}else{
						print(j)
						print(Z[eval(s2c(by.col))[[1]]==i])
						prob.fix <- fix.taxProb()
						taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
					}
			
			
					if(prob.fix!="SKIP"){ # I've never tested the use of the SKIP response ...
						Z[eval(s2c(by.col))[[1]]==i,c(j):=list(prob.fix)]
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
check_and_set <- function(wrong, corrected, Z=spp.key){
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
	# setkey(Z, ref, spp)
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


# =========
# = check =
# =========

check <- function(X, check_ind, random=FALSE){
	pr <- function(X, index, check_num=NULL, check_tot=NULL){
		
		help_msg <- "
				others --col1-col2:     type others followed by column names to match to spp.key
				change --col1-col2 val: type change followed by column names to change to val
				clearFlag:              changes most columns to NA, sets flag to bad
				genusCheck:             genus is correct, but taxLvl, species, spp and trophic information wrong; flag as check
				splitSpp:               copy spp into species, first word of spp into genus, set taxLvl to spp, flag as check
				comNA:                  set common to NA, flag as check
				r2s --spp:              apply ref2spp function; after supplying spp name, check for it in rest of data set, and import values from other instances of spp
				undo --n:               undoes changes to past n lines (note: not past n *changes*, but *lines*!)
				z:                      undoes all changes made to current line (undo --1)
				g:                      google the 'ref' in chrome
				f:                      FIND 'ref' using gnr_resolve; matches might be outdated
				s:                      SYNONYMS of 'spp'; find most up-to-date
				c:                      continue to next line
			"
			
			stopifnot(sum(index)==1)
		
		if(is.null(check_num) | is.null(check_tot)){
			cat("\n>>> Current line to check:\n")
		}else{
			current_msg <- paste("\n>>> Current line to check (item ", check_num, " of ", check_tot, ", ", round(check_num/check_tot, 3)*100, "%):\n", sep="")
			cat(current_msg)
		}
		cat(print(X[index]))
		
		get_piece <- function(rl1){
			gsub("(?=[ --]).*", "", rl1, perl=TRUE)
		}
		
		rl1 <- readline("Declare action (type 'h' for help): ")
		piece <- get_piece(rl1)
		while(!piece%in%c("others","change","clearFlag","genusCheck","splitSpp","comNA","r2s","z","undo","g","f","s","c")){
			
			if(piece=="h"){
				cat(help_msg)
			}else{
				cat(">>> invalid action selected, type 'h' for help")
			}
			
			rl1 <- readline("Declare action (type 'h' for help): ")
			piece <- get_piece(rl1)
			
		}
		
		# while(rl1=="h"){
		# 	cat(help_msg)
		# 	rl1 <- readline("Declare action (type 'h' for help): ")
		# }
		
		piece <- get_piece(rl1)
		stopifnot(piece%in%c("others","change","clearFlag","genusCheck","splitSpp","comNA","r2s","z","undo","g","f","s","c"))
		
		return(list(rl1=rl1, piece=piece))
	}
	
	others <- function(X, index, rl1, retInd=FALSE){
		cols <- c(simplify2array(strsplit(gsub("others --", "", rl1), "-")))
		
		X_val <- X[index, eval(s2c(cols))]
		
		dt_in <- function(d1, d2, cols){
			m <- mapply("%in%", d1[,eval(s2c(cols))], d2[,eval(s2c(cols))])
			rowSums(m)==ncol(m)
		}
		X_ind <- dt_in(X, X[index], cols=cols)
		if(sum(X_ind)==0){
			cat(">>> no matches found")
		}else{
			cat(print(X[X_ind]))
		}
		
		if(retInd){
			return(X_ind)
		}else{
			invisible(NULL)
		}
		
	}
	
	change <- function(X, index, rl1){
		col_char <- gsub("(change --)|( [ a-zA-Z0-9]*$)", "", rl1)
		val <- gsub("(change --[a-zA-Z0-9-]*[a-zA-Z] )", "", rl1)
		cols <- c(simplify2array(strsplit(col_char, "-")))
		
		if(val=="NA"){
			val <- NA
		}else{
			val <- as(val, unique(sapply(X[,eval(s2c(cols))], class)))
		}
		
		X[index, c(cols):=val]
		cat("\n>>> changes made\n")
		flush.console()
		
		invisible(NULL)
	}
	
	
	clearFlag <- function(X, index){
		rl_flag <- "change --flag bad"
		rl_clear <- "change --spp-common-taxLvl-species-genus-family-order-class-superclass-subphylum-phylum-kingdom-Picture-trophicOrig-trophicDiet NA"
		rl_clear_tl <- "change --trophicLevel-trophicLevel.se NA"
		
		rlf <- change(X, index, rl_flag)
		rlc <- change(X, index, rl_clear)
		rlc_tl <- change(X, index, rl_clear_tl)
		
		invisible(NULL)
	}
	
	genusCheck <- function(X, index){
		rl_flag <- "change --flag check"
		rl_clear <- "change --spp-common-taxLvl-species-Picture-trophicOrig-trophicDiet NA"
		rl_clear_tl <- "change --trophicLevel-trophicLevel.se NA"
		
		rlf <- change(X, index, rl_flag)
		rlc <- change(X, index, rl_clear)
		rlc_tl <- change(X, index, rl_clear_tl)
		
		X[index, spp:=genus]
		X[index, taxLvl:="genus"]
		
		invisible(NULL)
	}
	
	splitSpp <- function(X, index){
		spp_name <- X[index, spp]
		genus_name <- gsub("^([A-Z][[:alnum:]]+).*$", "\\1", spp_name)
		X[index, c("taxLvl", "species", "genus", "flag"):=list("species",spp_name,genus_name,"check")]
		
		invisible(NULL)
	}
	
	comNA <- function(X, index){
		X[index, c("common","flag"):=list(NA, "check")]
		invisible(NULL)
	}
	
	r2s <- function(X, index, rl1){
		spp_name <- gsub("r2s --", "", rl1)
		Ref <- X[index, ref]
		ref2spp(Ref=Ref, spp_name, Z=X)
		
		invisible(NULL)
	}
	
	
	
	undo <- function(X, hist, rl1, r){
		count_r <- function(hist){sum(sapply(hist, function(x)!is.null(x$index)))}
		undo_n <- function(rl1){as.integer(gsub("(undo --)", "", rl1))}
		
		if(missing(r)){
			r <- count_r(hist)
		}
		if(rl1=="z"){
			n <- 1L
		}else{
			n <- undo_n(rl1)
		}
		
		undo_ind <- r:(r-n+1)
		olds <- rbindlist(lapply(hist[undo_ind], function(x)X[x$index]))
		for(h in undo_ind){
			# X[hist[[h]]$index] <- hist[[h]]$X
			
			cn <- names(X)
			X[hist[[h]]$index, c(cn):=hist[[h]]$X]
			
		}
		news <- rbindlist(lapply(hist[undo_ind], function(x)X[x$index]))
		
		# cat(print(olds))
		cat("\n>>> reverted to\n")
		cat(print(news))
		# cat(print(X[hist[[r]]$index]))
		flush.console()
		
		invisible(NULL)
	}
	
	f <- function(X, index){
		print(X[index, gnr_resolve(una(ref))])
		invisible(NULL)
	}
	
	s <- function(X, index){
		tsn <- get_tsn(X[index,una(spp)], ask=FALSE, verbose=FALSE)
		acc <- itis_acceptname(tsn)
		print(acc)
		invisible(NULL)
	}
	
	hist <- list(list(index=NULL, X=NULL))
	check_vec <- 1:sum(check_ind)
	check_tot <- length(check_vec)
	if(random){
		check_vec <- sample(check_vec, check_tot, replace=FALSE)
	}
	check_num <- 0
	
	for(r in check_vec){
		check_num <- check_num + 1
		t_ind <- cumsum(check_ind)==r & check_ind
		out <- NULL
		
		hist[[check_num]] <- list(index=t_ind, X=X[t_ind])
		
		while(is.null(out) || (!is.null(out) & out!="c")){
			t_pr <- pr(X, t_ind, check_num=check_num, check_tot=check_tot)
			out <- switch(t_pr$piece, 
				change=change(X, t_ind, t_pr$rl1), 
				others=others(X, t_ind, t_pr$rl1), 
				clearFlag=clearFlag(X, t_ind),
				genusCheck=genusCheck(X, t_ind),
				splitSpp=splitSpp(X, t_ind),
				comNA=comNA(X, t_ind),
				r2s=r2s(X, t_ind, t_pr$rl1),
				undo=undo(X, hist, rl1=t_pr$rl1, r=check_num),
				z=undo(X, hist, rl1="z", r=check_num),
				g=system("bash -l", input=paste0("google ", "'",cull(X[t_ind,ref])," itis'")),
				f=f(X, t_ind),
				s=s(X, t_ind),
				c="c"
			)
		}
	}
	
	invisible(NULL)
	
}