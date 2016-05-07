# Find the taxonomic identifiers, ref, that should be checked first in spp.key
# because they are used in trawlDiversity package.


refs2check <- c()
poss_regs <- c("ebs", "ai", "goa", "wctri", "wcann", "gmex", "sa", "neus", "shelf", "newf")
for(i in 1:length(poss_regs)){
	t_reg <- poss_regs[i]
	
	# work with a fresh copy of package data set
	switch(t_reg,
		ebs = data(clean.ebs, package="trawlData"),
		ai = data(clean.ai, package="trawlData"),
		goa = data(clean.goa, package="trawlData"),
		wctri = data(clean.wctri, package="trawlData"),
		wcann = data(clean.wcann, package="trawlData"),
		gmex = data(clean.gmex, package="trawlData"),
		sa = data(clean.sa, package="trawlData"),
		neus = data(clean.neus, package="trawlData"),
		shelf = data(clean.shelf, package="trawlData"),
		newf = data(clean.newf, package="trawlData")
	)
	
	# rename the package data set
	X.t <- switch(t_reg,
		ebs = copy(clean.ebs),
		ai = copy(clean.ai),
		goa = copy(clean.goa),
		wctri = copy(clean.wctri),
		wcann = copy(clean.wcann),
		gmex = copy(clean.gmex),
		sa = copy(clean.sa),
		neus = copy(clean.neus),
		shelf = copy(clean.shelf),
		newf = copy(clean.newf)
	)
	
	# Reduce the number of columns
	clean.trimCol(X.t, c.add=c("flag", "row_flag"))
	
	# Trim year
	if(t_reg == "ai"){
		X.t <- X.t[(year)>1900,]
	}
	if(t_reg == "ebs"){
		X.t <- X.t[(year)>1983,]
	}
	if(t_reg == "gmex" | t_reg == "neus"){
		X.t <- X.t[(year)!=2015,]
	}
	if(t_reg == "gmex"){
		X.t <- X.t[(year)>1983 & (year)<2001]
	}
	if(t_reg == "neus"){
		X.t <- X.t[(year)>1981 & (year)<2014]
	}
	if(t_reg == "newf"){
		X.t <- X.t[(year)>1995,]
	}
	if(t_reg == "sa"){
		X.t <- X.t[(year)>=1990]
	}
	if(t_reg == "shelf"){
		X.t <- X.t[(year)!=2011 & (year)>1950,]
	}
	if(t_reg == "wcann"){
		X.t <- X.t[(year)>2003,]
	}
	
	# Trim day of year
	# ---- constratin/ standardize time of year (day of year) sampling occurred ----
	yd <- X.t[,yday(datetime)]
	X.t <- switch(t_reg,
		ebs = X.t[yd <= 220],
		# ai = X.t[yd >= 170 & yd <= 225], # sampling periods really overlap between 175 and 225
		# goa = X.t[yd <= 210],
		# wc = X.t[yd >= 170 & yd <= 240],
		# wctri = X.t[yd >= 170 & yd <= 240],
		gmex = X.t,
		sa = X.t[yd >= 150 & yd <= 250],
		neus = X.t,
		shelf = X.t[yd >=  180 & yd <= 215],
		newf = X.t[ yd >= 250 | yd <= 28],
		X.t # just returns X.t if reg isn't matched (e.g., if reg was wcann or sgulf)
	)
	
	
	# match other flags from clean.trimRow
	ctr_flag <- c("Gear", "Haul", "Eff", "Spp1", "Surv", "Seas", "Rec", "Strat", "Set", "Date", "Type")
	cf <- paste(ctr_flag, collapse="|")
	logi <- X.t[,grepl(cf, row_flag)]
	
	match.badSpp <- function(x, value=FALSE){
		ux <- unique(x)
		badEgg <- grepl("[eE][gG]{2}", ux)
		badFish <- grepl("(?<![a-z])fish(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		badPurse <- grepl("(?<![a-z])purse(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		badLarv <- grepl("(?<![a-z])larv(a[e])?(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		badYoy <- grepl("(?<![a-z])yoy(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		missSpp <- ux=="" | is.na(ux)
		bad.x <- ux[(badEgg | badFish | badPurse | badLarv | badYoy | missSpp)]
		bad.i <- (x%in%bad.x)
		if(value){
			return(x[bad.i])
		}else{
			return(bad.i)
		}
	}
	badSpp <- X.t[,match.badSpp(ref)]
	noID <- X.t[,spp=="" | is.na(spp)]
	
	new_ref <- X.t[!badSpp & !noID & !logi, una(ref)]
	# new_ref <- X.t[, una(ref)]
	
	
	refs2check <- unique(c(refs2check, new_ref))
}




