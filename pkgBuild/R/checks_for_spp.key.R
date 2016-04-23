

check <- function(X, check_ind, random=FALSE){
	pr <- function(X, index, check_num=NULL, check_tot=NULL){
		
		help_msg <- "
				others --col1-col2:     type others followed by column names to match to spp.key
				change --col1-col2 val: type change followed by column names to change to val
				clearFlag:              changes most columns to NA, sets flag to bad
				genusCheck:             genus is correct, but taxLvl, species, spp and trophic information wrong; flag as check
				undo --n:               undoes changes to past n lines (note: not past n *changes*, but *lines*!)
				z:                      undoes all changes made to current line (undo --1)
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
		while(!piece%in%c("others","change","clearFlag","genusCheck","z","undo","c")){
			
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
		stopifnot(piece%in%c("others","change","clearFlag","genusCheck","z","undo","c"))
		
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
		gsub("change --", "", rl1)
		col_char <- gsub("(change --)|( [a-zA-Z0-9]*$)", "", rl1)
		val <- gsub("(change --)|([a-zA-Z0-9-]* )", "", rl1)
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
				undo=undo(X, hist, rl1=t_pr$rl1, r=check_num),
				z=undo(X, hist, rl1="z", r=check_num),
				c="c"
			)
		}
	}
	
	invisible(NULL)
	
}



sk <- copy(spp.key)


# only one word, but is a "species"
c1 <- sk[,!grepl(" ", ref)&taxLvl=="species"&flag!="bad"]
c1 <- c1&!is.na(c1)
check(sk, c1, random=TRUE)
sk1 <- copy(sk)

# be more consistent in common names

# common name belongs to multiple spp
c2 <- sk[,(flag!="bad") & (common%in%sk[!is.na(common) & !is.na(spp),list(n_spp=lu(spp)), by="common"][n_spp>1, common])]
c2 <- c2&!is.na(c2)
check(sk, c2)
sk2 <- copy(sk)

# common contains non- A-Za-z chars
c3 <- sk[,grepl("[^a-zA-Z -]", common)]
c3 <- c3&!is.na(c3)
check(sk, c3)

# species is NA, but common name isn't, and isn't flagged
sk[is.na(species) & !is.na(common) & (is.na(flag) | flag!="bad")]


