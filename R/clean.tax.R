#' Clean Taxonomy
#' 
#' Clean by fixing/ standardizing taxonomic names
#' 
#' @template X_reg
#' 
#' @details
#' relies on spp.key data file that comes with this package. In "\inst\extdata" there is also a .csv version of this file that is being used. Taxonomy corrections are an ongoing process.  
#' Also adds taxonomic classification, some ecological information (trophic level), common names, whether or not a picture is available with this package, and other miscellanious information about how the taxonomic correction was made.
#' 
#' The \code{ref} column in the output is the name of the original species name/ taxonomic identifier.
#' 
#' @export clean.tax
clean.tax <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri")){
	# load("data/spp.key.RData")
	reg <- match.arg(reg)
	
	
	clean.tax0 <- function(x){
		switch(x,
			ai = clean.tax.ai(X),
			ebs = clean.tax.ebs(X),
			gmex = clean.tax.gmex(X),
			goa = clean.tax.goa(X),
			neus = clean.tax.neus(X),
			newf = clean.tax.newf(X),
			ngulf = clean.tax.ngulf(X),
			sa = clean.tax.sa(X),
			sgulf = clean.tax.sgulf(X),
			shelf = clean.tax.shelf(X),
			wcann = clean.tax.wcann(X),
			wctri = clean.tax.wctri(X)
		)
	}
	
	
	# X <- copy(gmex)
	if(!"ref"%in%names(X)){
		setnames(X, "spp", "ref")
	}
	
	names.x <- names(X)
	rmX.names <- names.x[names.x%in%(names(spp.key)[names(spp.key)!="ref"])]
	
	setkey(spp.key, "ref")
	X <- merge(X, spp.key, by="ref", all.x=TRUE)
	
	# alternatively, I could just drop the repeated 
	# column names before the merge
	# I think I have it this way in case in the future I'd like
	# to keep both sets of names
	if(length(rmX.names)>0){
		drop.x <- function(x){
			X[,c(paste0(x,".x")):=NULL]
			setnames(X, paste0(x,".y"), x)
		}
		drop.x(rmX.names)
	}
	
	#
	# smry.tax0 <- X[,table(taxLvl[!duplicated(spp)])]
	# nst <- names(smry.tax0)
	# sum.tax <- function(x){sum(smry.tax0[nst[grepl(x,nst,ignore.case=T)]])}
	# smry.tax <- c(
	# 	kingdom = sum.tax("kingdom"),
	# 	phylum = sum.tax("phylum"),
	# 	class = sum.tax("class"),
	# 	order = sum.tax("order"),
	# 	family = sum.tax("family"),
	# 	genus = sum.tax("genus"),
	# 	species = sum.tax("species")
	# )
	
	
	
	
	
	# invisible(clean.tax0(reg))
	
	return(X)
	
}