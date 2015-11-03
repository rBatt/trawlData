
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

