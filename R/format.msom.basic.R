
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


# ==================================
# = Example of how to prepare MSOM =
# ==================================
setkey(trawl2.veri, s.reg, year, stratum, K, spp)
basic.dat <- expand.data(
	comD = copy(trawl2.veri),
	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
	fillID=c("spp","K"),
	fillValue=c(0,NA), # values to fill with, for a fillID
	Rule=c("value","scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
	keyID=c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
	keyValue="wtcpue", # the column whose values would fill the array
	gScope="s.reg", # global scope
	fScope=list("s.reg", c("s.reg","year")), #
	vScope=list(c("s.reg","stratum","K","year"), NULL),
	redID=NULL, 
	redValue=NULL,
	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
)


# ========
# = Save =
# ========
save(basic.dat, file="./trawl/Data/MSOM/basic.dat.RData")


