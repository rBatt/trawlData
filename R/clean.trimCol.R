clean.trimCol <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri")){
	
	reg <- match.arg(reg)
	
	
	get.clean.trimCol <- function(x){
		switch(x,
			ai = clean.trimCol.ai(X),
			ebs = clean.trimCol.ebs(X),
			gmex = clean.trimCol.gmex(X),
			goa = clean.trimCol.goa(X),
			neus = clean.trimCol.neus(X),
			newf = clean.trimCol.newf(X),
			ngulf = clean.trimCol.ngulf(X),
			sa = clean.trimCol.sa(X),
			sgulf = clean.trimCol.sgulf(X),
			shelf = clean.trimCol.shelf(X),
			wcann = clean.trimCol.wcann(X),
			wctri = clean.trimCol.wctri(X)
		)
	}
	
	
	
}




# ======
# = AI =
# ======
clean.trimCol.ai <- function(X){
	

	

}


# =======
# = EBS =
# =======
clean.trimCol.ebs <- function(X){



}


# ========
# = GMEX =
# ========
clean.trimCol.gmex <- function(X){

	
}


# =======
# = GOA =
# =======
clean.trimCol.goa <- function(X){
	

	
}


# ========
# = NEUS =
# ========
clean.trimCol.neus <- function(X){
	

	
	
}


# ========
# = NEWF =
# ========
clean.trimCol.newf <- function(X){
	
	

	
}

# =========
# = NGULF =
# =========
clean.trimCol.ngulf <- function(X){

}


# ======
# = SA =
# ======
clean.trimCol.sa <- function(X){
	

	
}


# =========
# = SGULF =
# =========
clean.trimCol.sgulf <- function(X){
	

}

# =========
# = SHELF =
# =========
clean.trimCol.shelf <- function(X){
	

}

# ==========
# = WC ANN =
# ==========
clean.trimCol.wcann <- function(X){
	
	
	


}

# ==========
# = WC TRI =
# ==========
clean.trimCol.wctri <- function(X){
	


	
	
}







