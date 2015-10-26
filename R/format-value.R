
# ====================================
# = Functions for cleaning spp names =
# ====================================
fixCase <- function(x){
	s <- paste(toupper(substring(x, 1, 1)), substring(x, 2), sep="")
	paste(substring(s, 1, 1), tolower(substring(s, 2)), sep="")
}

cullExSpace <- function(x){
	gsub("\\s+", " ", x)
}

cullSp <- function(x){
	gsub("\\s(s[p]{1,2}|unid)\\..*", "", x)
}

cullParen <- function(x){
	gsub("\\s?\\(.*\\)", "", x)
}

is.species <- function(x){
	sapply(strsplit(x, " "), length) >= 2
}


rmWhite <- function(x){
	stopifnot(is.data.table(x))
	nmx <- names(x)
	classes <- sapply(x, class)
	setClass <- classes%in%c("character","numeric","integer","logical") # leaving out factor b/c can't convert char to factor #"integer64",
	for(i in 1:ncol(x)){
		t.name <- as.name(nmx[i])
		if(setClass[i]){
			# expr <- bquote(.(t.name):=as(gsub("^\\s* | \\s*$", "", .(t.name)), Class=classes[i]))
			expr <- bquote(.(t.name):=as(str_trim(.(t.name)), Class=classes[i]))
			x[,eval(expr)]
		}else{
			# expr <- bquote(.(t.name):=gsub("^\\s* | \\s*$", "", .(t.name)))
			expr <- bquote(.(t.name):=str_trim(.(t.name)))
			x[,eval(expr)]
		}
		
	}
}






rm9s <- function(x){
	stopifnot(is.data.table(x))
	for(i in seq_along(x)){
		t.x <- x[[i]]
		t.class <- class(t.x)
		# set(x, i=which(x[[i]]==-9999L), j=i, value=as.character(NA))
		# set(x, i=which(x[[i]]==-9999.0), j=i, value=as.character(NA))
		
		set(x, i=which(t.x==-9999L | t.x==-9999.0), j=i, value=as(NA,Class=t.class))
	}
}



