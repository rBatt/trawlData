
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