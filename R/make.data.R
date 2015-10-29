# preparation to be deleted
library(data.table)
library(LaF)
library(stringr)
library(bit64)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)


setwd("~/Documents/School&Work/pinskyPost/trawlData/")

source("./R/read.trawl.R")
source("./R/clean.names.R")
source("./R/clean.format.R")
source("./R/clean.columns.R")
source("./R/helper-file.R")
source("./R/helper-misc.R")
source("./R/format-value.R")
source("./R/format-strat.R")
source("./R/tax-getSpp.R")
source("./R/tax-grb.spp1.R")
source("./R/tax-getTL.R")
source("./R/tax-getCmmn.R")

regions <- c("ai", "ebs", "gmex", "goa", "neus", "newf", "sa", "sgulf", "shelf", "wcann", "wctri")



ai <- read.trawl("ai")
clean.names(ai, "ai")
clean.format(ai, "ai")
clean.columns(ai, "ai")

ebs <- read.trawl("ebs")
clean.names(ebs, "ebs")
clean.format(ebs, "ebs")
clean.columns(ebs, "ebs")

gmex <- read.trawl("gmex")
clean.names(gmex, "gmex")
clean.format(gmex, "gmex")
clean.columns(gmex, "gmex")

goa <- read.trawl("goa")
clean.names(goa, "goa")
clean.format(goa, "goa")
clean.columns(goa, "goa")

neus <- read.trawl("neus")
clean.names(neus, "neus")
clean.format(neus, "neus")
clean.columns(neus, "neus")

newf <- read.trawl("newf")
clean.names(newf, "newf")
clean.format(newf, "newf")
clean.columns(newf, "newf")

sa <- read.trawl("sa")
clean.names(sa, "sa")
clean.format(sa, "sa")
clean.columns(sa, "sa")

sgulf <- read.trawl("sgulf")
clean.names(sgulf, "sgulf")
clean.format(sgulf, "sgulf")
clean.columns(sgulf, "sgulf")

shelf <- read.trawl("shelf")
clean.names(shelf, "shelf")
clean.format(shelf, "shelf")
clean.columns(shelf, "shelf")

wcann <- read.trawl("wcann")
clean.names(wcann, "wcann")
clean.format(wcann, "wcann")
clean.columns(wcann, "wcann")

wctri <- read.trawl("wctri")
clean.names(wctri, "wctri")
clean.format(wctri, "wctri")
clean.columns(wctri, "wctri")


cnames <- unique(c(names(ai), names(ebs), names(gmex), names(goa), names(neus), names(wcann), names(wctri)))


# read in raw
for(i in 1:length(regions)){
	nm <- paste0("raw.", regions[i])
	assign(nm, read.trawl(regions[i]))
	save(list=nm, file=paste0("data/",nm,".RData"), compress="xz")
}

# load raw
pb <- txtProgressBar(min=1, max=length(regions), style=3)
for(i in 1:length(regions)){
	nm <- paste0("raw.", regions[i])
	load(file=paste0("data/",nm,".RData"))
	setTxtProgressBar(pb, i)
}

# clean up column names
pb <- txtProgressBar(min=1, max=length(regions), style=3)
for(i in 1:length(regions)){
	nm <- paste0("raw.", regions[i])
	assign(regions[i], copy(get(nm)))
	clean.names(get(regions[i]), regions[i])
	setTxtProgressBar(pb, i)
}

# format column values
pb <- txtProgressBar(min=1, max=length(regions), style=3)
for(i in 1:length(regions)){
	nm <- regions[i]
	clean.format(get(nm), nm)
	setTxtProgressBar(pb, i)
}

# clean column content, add columns
pb <- txtProgressBar(min=1, max=length(regions), style=3)
for(i in 1:length(regions)){
	nm <- regions[i]
	clean.columns(get(nm), nm)
	setTxtProgressBar(pb, i)
}

# save clean
pb <- txtProgressBar(min=1, max=length(regions), style=3)
for(i in 1:length(regions)){
	nm <- paste0("clean.", regions[i])
	assign(nm, copy(get(regions[i])))
	save(list=nm, file=paste0("data/",nm,".RData"), compress="xz")
	setTxtProgressBar(pb, i)
}

# load clean
pb <- txtProgressBar(min=1, max=length(regions), style=3)
for(i in 1:length(regions)){
	nm <- paste0("clean.", regions[i])
	load(file=paste0("data/",nm,".RData"))
	setTxtProgressBar(pb, i)
}

# rename clean, rm old
pb <- txtProgressBar(min=1, max=length(regions), style=3)
for(i in 1:length(regions)){
	nm <- paste0("clean.", regions[i])
	assign(regions[i], copy(get(nm)))
	rm(list=nm)
	setTxtProgressBar(pb, i)
}

cnames <- sort(unique(c(names(ai), names(ebs), names(gmex), names(goa), names(neus), names(newf), names(sa), names(sgulf), names(shelf), names(wcann), names(wctri))))


spp <- sort(unique(c(ai[,unique(spp)], ebs[,unique(spp)], gmex[,unique(spp)], goa[,unique(spp)], neus[,unique(spp)], newf[,unique(spp)], sa[,unique(spp)], sgulf[,unique(spp)], shelf[,unique(spp)], wcann[,unique(spp)], wctri[,unique(spp)]))) # use this in creat.spp.key.R

