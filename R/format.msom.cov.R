
# ==================
# = Load libraries =
# ==================
library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))


# =======================
# = Load trawl2 Dataset =
# =======================
load("./trawl/Data/trawl2.RData")


# ======================
# = Remove unknown spp =
# ======================
trawl2.veri <- trawl2[(correctSpp)&!is.na(common)&is.species(spp),]





# ===========================
# = Test Covariate Building =
# ===========================
# trawl.test <- trawl2.veri[s.reg%in%c("gmex","ai","neus") & year%in%c("1983","1986")]
#
# comD = copy(trawl.test)
# arr.dim = c("stratum", "K", "spp") # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
# fillID=c("spp","K")
# fillValue=c(0,NA) # values to fill with, for a fillID
# Rule=c("value","scope") # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# keyID=c("s.reg", "stratum","year","spp", "K") # column names whose values uniquely identify rows in the input
# keyValue="wtcpue" # the column whose values would fill the array
# gScope="s.reg" # global scope
# fScope=list("s.reg", c("s.reg","year"))
# vScope=list(c("s.reg","stratum", "K", "year"), NULL)
# redID=list(c("s.reg","year","stratum","K","spp"))
# redValue=list(c("btemp","depth"))
# arrayOut=TRUE
# aggFun=meanna
# maxOut=Inf
#
#
# setkey(trawl.test, s.reg, year, stratum, K, spp)
# cov.btemp.dat.test <- expand.data(
# 	comD = copy(trawl.test),
# 	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
# 	fillID=c("spp","K"),
# 	fillValue=c(0,NA), # values to fill with, for a fillID
# 	Rule=c("value","scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# 	keyID=c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
# 	keyValue="wtcpue", #
# 	gScope="s.reg", # global scope
# 	fScope=list("s.reg", c("s.reg","year")), #
# 	vScope=list(c("s.reg","stratum","K","year"), NULL),
# 	redID=list(c("s.reg","year","stratum","K","spp")),
# 	redValue=list(c("btemp","depth")),
# 	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
# )
#







# ==========================================
# = Prepare Temperature Covariate for MSOM =
# ==========================================
setkey(trawl2.veri, s.reg, year, stratum, K, spp)
cov.dat0 <- expand.data(
	comD = copy(trawl2.veri),
	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
	fillID=c("spp","K"),
	fillValue=c(0,NA), # values to fill with, for a fillID
	Rule=c("value","scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
	keyID=c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
	keyValue="wtcpue", #
	gScope="s.reg", # global scope
	fScope=list("s.reg", c("s.reg","year")), #
	vScope=list(c("s.reg","stratum","K","year"), NULL),
	redID=list(c("s.reg","year","stratum","K","spp")), 
	redValue=list(c("btemp","depth")),
	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
)


# fill.cov <- function(x, FUN){ # fill covariate means
# 	# Environmental variables like depth and temperature have species-specific values in the data set
# 	# E.g., temperature in a region-stratum-substratum-year for a particular species is just the average of temperatures where that species was caught
# 	# because there are multiple hauls (potentially) per substratum, and not all species are caught, this value can vary
# 	# however, we can assume that the variability among hauls is small relative to the variability among strata or years
# 	# If a species isn't caught at in a year/substratum, the temperature associated with that absence is the average of the temperatures recorded for the other species caught in that same time/ place.
# 	bother <- apply(x, c(1,2), function(x)any(is.na(x))&any(!is.na(x)))
# 	if(any(bother)){
# 		bother2 <- which(bother, arr.ind=TRUE)
# 		fm <- apply(x, c(1,2), FUN, na.rm=TRUE)
#
# 		for(i in 1:nrow(bother2)){
# 			t.bother <- bother2[i,]
# 			x.na <- is.na(x[t.bother[1], t.bother[2], ])
# 			x[t.bother[1], t.bother[2], x.na] <- fm[t.bother[1],t.bother[2]]
# 		}
# 	}
# 	return(x)
# }


# prec <- function(x, ...){
# 	if(sum(is.finite(x))==1 & max(x,...)!=0){
# 		return(1/(max(x,...)^2))
# 	}else{
# 		return( min(1E4, 1/var(x, ...)) )
# 	}
# }
# fill.cov.prec <- function(x){ # fill covariate means
# 	# Environmental variables like depth and temperature have species-specific values in the data set
# 	# E.g., temperature in a region-stratum-substratum-year for a particular species is just the average of temperatures where that species was caught
# 	# because there are multiple hauls (potentially) per substratum, and not all species are caught, this value can vary
# 	# however, we can assume that the variability among hauls is small relative to the variability among strata or years
# 	# If a species isn't caught at in a year/substratum, the temperature associated with that absence is the average of the temperatures recorded for the other species caught in that same time/ place.
# 	x[is.finite(x)] <- 1E4
# 	bother <- apply(x, c(1,2), function(x)any(is.na(x))&any(!is.na(x)))
# 	fill.but.no.sd <- apply(x, c(1,2), function(x)any(is.na(x))&(sum(!is.na(x))==1))
# 	# if(any(fill.but.no.sd)){warning("only 1 non-NA; values would be filled with mean, but not stdev")}
# 	if(any(bother)){
# 		bother2 <- which(bother, arr.ind=TRUE)
# 		fm <- apply(x, c(1,2), prec, na.rm=TRUE)
#
# 		for(i in 1:nrow(bother2)){
# 			t.bother <- bother2[i,]
# 			x.na <- is.na(x[t.bother[1], t.bother[2], ])
# 			x[t.bother[1], t.bother[2], x.na] <- fm[t.bother[1],t.bother[2]]
# 			# x[t.bother[1], t.bother[2], !x.na] <- 1E4
# 		}
# 	}
# 	return(x)
# }



cov.dat <- cov.dat0
for(i in 1:length(cov.dat0[[3]])){
	# cov.dat[[3]][[i]] <- lapply(cov.dat[[3]][[i]], fill.cov, mean)
	cov.dat[[3]][[i]] <- sapply(cov.dat0[[3]][[i]], apply, 1, mean, na.rm=TRUE)
}
#
# cv1.mu <- mean(unlist(cov.dat[[3]][[1]]), na.rm=TRUE)
# cv2.mu <- mean(unlist(cov.dat[[3]][[2]]), na.rm=TRUE)
#
# cv1.sd <- sd(unlist(cov.dat[[3]][[1]]), na.rm=TRUE)
# cv2.sd <- sd(unlist(cov.dat[[3]][[2]]), na.rm=TRUE)
#
# cov.dat.prec <- cov.dat0[[3]]
# for(i in 1:length(cov.dat.prec)){
# 	cov.dat.prec[[i]] <- sapply(cov.dat.prec[[i]], apply, 1, sd, na.rm=TRUE)
# }

# DIMS tests (does it make sense)
# min(cov.dat.prec[[2]][[275]], na.rm=TRUE)
# sqrt(1/min(cov.dat.prec[[2]][[275]], na.rm=TRUE))
# max(cov.dat.prec[[2]][[3]], na.rm=TRUE)



# ======================================
# = Change original data's NA's to 1's =
# ======================================
change.na.1 <- trawl2[is.na(wtcpue), list(s.reg=s.reg, year=year, stratum=stratum, K=K, spp=spp)]
setkey(cov.dat[[2]], s.reg, year)
md.num <- cov.dat[[2]][change.na.1[,list(s.reg,as.character(year))]][,as.numeric(num)]
for(i in 1:length(md.num)){
	t.c <- change.na.1[i,c(stratum=stratum,K=K,spp=spp)]
	tryCatch(
		{
			if(!is.na(cov.dat[[1]][[md.num[i]]][t.c[1],t.c[2],t.c[3]])){
				print("already fixed!")
			}else{
				cov.dat[[1]][[md.num[i]]][t.c[1],t.c[2],t.c[3]] <- 1
			}
		
		}, 
		error=function(cond){print(paste0(t.c,collapse=" "))} # sometimes the subset doesn't exist b/c that particular species from trawl2 was removed before the creation of cov.dat b/c it wasn't correctSpp, or it didn't have a common name, or is.species() was F, etc. For example, spp == "Gastropoda" or spp=="Trash species in catch" will not be included here. spp=="Ophichthus ocellatus" wasn't included because it doesn't have a common name (but see issue #26) 
	)
}


# =======================================
# = Function to remove unsampled strata =
# =======================================
prep.cov <- function(pc.dat, pc.cov1, pc.cov2){	
	keep.strat <- apply(pc.dat, 1, function(x)!all(is.na(x))) # which strata were never sampled?
	pc.dat2 <- pc.dat[keep.strat,,] # remove strata that were never sampled
	
	keep.strat.cov1 <- is.finite(pc.cov1) #apply(pc.cov1, 1, function(x)!all(is.na(x))) # which strata don't have covariate 1?
	keep.strat.cov2 <- is.finite(pc.cov2) #apply(pc.cov2, 1, function(x)!all(is.na(x))) # which strata don't have covariate 2?
	
	# set up empty vectors to hold the precisions of the covaraites
	pc.cov1.prec <- numeric(length(keep.strat.cov1))
	pc.cov2.prec <- numeric(length(keep.strat.cov2))
	
	# fill in missing values and define precisions for the covariates
	# covariates that have an actual observation are given a huge precision
	pc.cov1.prec[keep.strat.cov1] <- 1E4
	pc.cov2.prec[keep.strat.cov2] <- 1E4
	
	# next, define a new objected for the filled-in covariate, then fill with mean, then define the precision of the filled
	pc.cov1.2 <- pc.cov1 # new object to hold filled in covarite
	pc.cov1.2[!keep.strat.cov1] <- mean(pc.cov1[keep.strat.cov1]) # fill in missings with the mean
	pc.cov1.prec[!keep.strat.cov1] <- 1/var(pc.cov1[keep.strat.cov1]) # the precision of the filled-in value is just 1/var()
	
	pc.cov2.2 <- pc.cov2 # new object to hold filled in covarite
	pc.cov2.2[!keep.strat.cov2] <- mean(pc.cov2[keep.strat.cov2]) # fill in missings with the mean
	pc.cov2.prec[!keep.strat.cov2] <- 1/var(pc.cov2[keep.strat.cov2]) # the precision of the filled-in value is just 1/var()
	
	# sum(keep.strat)
	# sum(keep.strat.cov1)
	# sum(keep.strat.cov2)
	
	# if(!all(keep.strat == keep.strat.cov1)){
# 		toFill <- is.na(pc.cov1) & !is.na(pc.dat)
# 		pc.cov1[toFill] <- mean(pc.cov1[pc.cov1.prec>=1E4], na.rm=TRUE) # the mean of the covariate values (subset to the values that weren't previously filled in!)
# 		pc.cov1.prec[toFill] <- pmax(1/1E4, 1/var(pc.cov1[!toFill & pc.cov1.prec>=1E4], na.rm=TRUE)) # precision as 1/variance, or if variance undefined, 1/1E4
# 		# any(is.na(pc.cov1.prec) & !is.na(pc.cov1))
# 		# any(is.na(pc.cov1.prec) & !is.na(pc.dat))
# 		# any(is.na(pc.cov1) & !is.na(pc.dat))
#
# 	}
# 	pc.cov1.2 <- apply(pc.cov1[keep.strat.cov1,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled
# 	pc.cov1.prec.2 <- apply(pc.cov1.prec[keep.strat.cov1,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled
#
#
# 	if(!all(keep.strat == keep.strat.cov2)){
# 		toFill <- is.na(pc.cov2) & !is.na(pc.dat)
# 		pc.cov2[toFill] <- mean(pc.cov1[pc.cov2.prec>=1E4], na.rm=TRUE) # the mean of the covariate values (subset to the values that weren't previously filled in!)
# 		pc.cov2.prec[toFill] <- pmax(1/1E4, 1/var(pc.cov2[!toFill & pc.cov1.prec>=1E4], na.rm=TRUE)) # precision as 1/variance, or if variance undefined, 1/1E4
# 	}
# 	pc.cov2.2 <- apply(pc.cov2[keep.strat.cov2,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled
# 	pc.cov2.prec.2 <- apply(pc.cov2.prec[keep.strat.cov2,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled


	pc.cov1.2 <- pc.cov1.2[keep.strat]
	pc.cov2.2 <- pc.cov2.2[keep.strat]
	pc.cov1.prec <- pc.cov1.prec[keep.strat]
	pc.cov2.prec <- pc.cov2.prec[keep.strat]
	
	
	
		

	# If there are NA's 
	orig.na.TF <- apply(pc.dat2, c(1,3), function(x){all(is.na(x))}) # NA's can indicate presence but unknown biomass
	if(any(orig.na.TF)){
		stop("A species was NA for all reps in a stratum.")
	}
	

	return(list(pc.dat2, pc.cov1.2, pc.cov2.2, pc.cov1.prec, pc.cov2.prec))
	# return(list(pc.dat2, pc.cov1.2, pc.cov2.2))

}


# ===================================
# = Prep Data for Bayesian Richness =
# ===================================
# t.dat, t.cov1, and t.cov2 should all be the same length
prepd.cov.dat <- vector("list", length(cov.dat[[1]]))
prepd.cov1 <- vector("list", length(cov.dat[[3]][[1]]))
prepd.cov2 <- vector("list", length(cov.dat[[3]][[2]]))
prepd.cov1.prec <- vector("list", length(cov.dat.prec[[1]]))
prepd.cov2.prec <- vector("list", length(cov.dat.prec[[2]]))


for(i in 1:length(cov.dat[[1]])){
	t.prep <- prep.cov(
		pc.dat=cov.dat[[1]][[i]],
		pc.cov1=cov.dat[[3]][[1]][[i]], 
		pc.cov2=cov.dat[[3]][[2]][[i]]#,
		# pc.cov1.prec=cov.dat.prec[[1]][[i]],
		# pc.cov2.prec=cov.dat.prec[[2]][[i]]
	)
	
	prepd.cov.dat[[i]] <- t.prep[[1]]
	prepd.cov1[[i]] <- t.prep[[2]]
	prepd.cov2[[i]] <- t.prep[[3]]
	prepd.cov1.prec[[i]] <- t.prep[[4]]
	prepd.cov2.prec[[i]] <- t.prep[[5]]
	
}



# save the covariate names (don't want to have to bother loading cov.dat just to get cov.dat[[2]] in richness.cov)
prepd.cov.names <- cov.dat[[2]]

# ========
# = Save =
# ========
save(prepd.cov.dat, prepd.cov1, prepd.cov2, prepd.cov1.prec, prepd.cov2.prec, prepd.cov.names, file="./trawl/Data/MSOM/prepd.msom.cov.RData", compress="xz")
save(cov.dat, file="./trawl/Data/MSOM/cov.dat.RData", compress="xz")
# save(cov.dat.prec, file="./trawl/Data/MSOM/cov.dat.prec.RData", compress="xz")


