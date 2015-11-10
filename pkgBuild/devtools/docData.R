docData <- function(x, idh=NULL){
	x.name <- gsub(".*x \\= (.*).*\\)$", "\\1", deparse(match.call()))
	x.col.names <- names(x)
	stopifnot(!is.null(x.col.names))
	stopifnot(!is.null(dim(x)) & length(dim(x))==2)
	
	nc <- ncol(x)
	nr <- nrow(x)
	if(!is.null(dim(x))){
		dim.desc <- paste("dim =", paste(dim(x),collapse=" x "))
	}else if(!is.null(length(x))){
		dim.desc <- paste("length =",length(x))
	}
	
	x.class <- class(x)
	x.col.class <- sapply(x, class)
	
	# is there an idh ("insert description here")?
	# if so, use it
	if(!is.null(idh) & length(idh)==nc){
		txt <- idh
	}else{
		if(!is.null(idh)){
			warning("idh description not the same length as number of columns; using place-holder description")
		}
		txt <- rep("insert_description_here", nc)
	}
	

	head.txt <- paste0("@format\nA ", dim.desc, " ", paste(x.class,collapse=" "), ":  \n")
	
	tab.start <-"\\tabular{rlll}{\n"
	tab.meat <- c()
	for(i in 1:nc){
		# ind <- paste("[,", formatC(i, width=nchar(nc)), "]")
		c.start <- paste0("[,", i, "]")
		tn <- x.col.names[i]
		tc <- x.col.class[i]
		c.end <- "\\cr"
		tab.meat[i] <- paste(c(paste(c(c.start, tn, paste0(tc, ""), txt[i]), collapse=" \\tab "),c.end,"\n"), collapse="")
	}
	tail.txt <- paste0("\"",x.name,"\"")
	
	txt.out <- paste(c(head.txt,tab.start, tab.meat, "}\n",tail.txt), collapse="")
	
	cat(txt.out)
	invisible(txt.out)

}