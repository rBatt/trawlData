
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

cullPost2 <- function(x){
	gsub("^(\\b[A-Za-z]{1,}\\b\\s+)(\\b[A-Za-z]{1,}\\b).*", "\\1\\2", x)
}

c.all <- function(x) cullPost2(cullParen(cullSp(fixCase(cullExSpace(x)))))

is.species <- function(x){
	sapply(strsplit(x, " "), length) >= 2
}


rmWhite <- function(x){
	stopifnot(is.data.table(x))
	has.cc <- names(x)[sapply(x, is.character)]
	x[,c(has.cc):=lapply(eval(s2c(has.cc)), str_trim)]
}






rm9s <- function(x){
	stopifnot(is.data.table(x))
	for(i in seq_along(x)){
		t.x <- x[[i]]
		t.class <- class(t.x)
		# set(x, i=which(x[[i]]==-9999L), j=i, value=as.character(NA))
		# set(x, i=which(x[[i]]==-9999.0), j=i, value=as.character(NA))
		if(t.class=="integer64"){
			set(x, i=which(t.x==-9999L | t.x==-9999.0), j=i, value=as.integer64(NA))
		}else{
			set(x, i=which(t.x==-9999L | t.x==-9999.0), j=i, value=as(NA,Class=t.class))
		}
		
	}
}



