

# ==================
# = Load Libraries =
# ==================
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)


# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# =============
# = Read Data =
# =============
neus.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NEFSC/2014-03-23/"

neus.strata <- fread(paste(neus.start, "neusStrata.csv", sep=""), select=c('StratumCode', 'Areanmi2')) # Need neusStrata.csv file from Malin (18-Aug-2014)
local({ # create a local environment to read in .RData, to ensure that other objects aren't overwritten

	load(paste(neus.start, "station.RData", sep="")) # station
	load(paste(neus.start, "Survdat.RData", sep="")) # survdat
	load(paste(neus.start, "SVSPP.RData", sep="")) # spp
	
	# assign variables in global environment (if not found in local environment)
	# neus.station <<- station
	# neus.survdat <<- survdat
	# neus.spp <<- spp
	
	# assign variables in global environment
	assign("neus.station", station, envir=.GlobalEnv)
	assign("neus.survdat.raw", survdat, envir=.GlobalEnv)
	assign("neus.spp", spp, envir=.GlobalEnv)
	
}) # end expressions to be carried out in new local environment
rmWhite(neus.station)
rmWhite(neus.survdat.raw)
rmWhite(neus.spp)

# ==================
# = Trim/ fix data =
# ==================
# based largely on Malin's code

# make changes to neus.spp
neus.spp[,c('ITISSPP', 'COMNAME', 'AUTHOR') := NULL] # remove some columns from spp data.table
neus.spp[,SVSPP:=as.character(SVSPP)]

# make changes to neus.strata
setkey(neus.strata, StratumCode)
setnames(neus.strata, "StratumCode", "STRATUM") # updates the key, too; this was done b/c data.table:::merege.data.table does not accept by.x and by.y

# make changes to neus.survdat.raw
setkey(neus.survdat.raw, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

# begin creating neus from neus.survdat.raw
neus000 <- unique(neus.survdat.raw) # drops length data
neus000[, c('LENGTH', 'NUMLEN') := NULL] # remove length columns
neus000[,c("SEASON", "SVSPP"):=list(as.character(SEASON), as.character(SVSPP))]

setkey(neus000, SEASON)
neus00 <- neus000["SPRING"]

# neus00[,lu(STATION),by=c("CRUISE6","STRATUM")][,plot(table(V1))]


neus0 <- neus00[j=lapply(list(BIOMASS=BIOMASS, ABUNDANCE=ABUNDANCE), FUN=sumna), by=list(YEAR, SEASON, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM, SVSPP)] # sum different sexes of same spp together; also, as a byproduct, this drops the key from neus000
# setnames(neus00, 'V1', 'wtcpue')
setkey(neus0, SEASON, SVSPP, YEAR, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM) # resetting key that was lost during combining sexes


# Merge spp and strata into neus data.table
neus <- merge(neus0, neus.spp, by="SVSPP") # add species names
neus <- merge(neus, neus.strata, by="STRATUM", all.x=TRUE)

# ====================================================
# = Create Haul ID, merge station info (temperature) =
# ====================================================
neus[,haulid:=paste(formatC(CRUISE6, width=6, flag=0), formatC(STATION, width=3, flag=0), formatC(STRATUM, width=4, flag=0), sep='-')]

neus.station[,haulid:=paste(formatC(CRUISE6, width=6, flag=0), formatC(STATION, width=3, flag=0), formatC(STRATUM, width=4, flag=0), sep='-')]
neus.station <- neus.station[,list(haulid, SURFTEMP, BOTTEMP)]

setkey(neus, haulid)

neus <- merge(neus, neus.station, all.x=TRUE)



# =============
# = Set Names =
# =============
setnames(neus, c("YEAR", "SCINAME", "LAT", "LON", "DEPTH", "STRATUM", "SURFTEMP", "BOTTEMP", "BIOMASS", "ABUNDANCE"), c("year", "spp", "lat", "lon", "depth", "stratum", "stemp", "btemp", "wtcpue", "cntcpue"))


# ==============
# = Fix Strata =
# ==============
neus <- makeStrat(neus, regName="neus")


# =================================
# = Stratum Area (malin line 181) =
# =================================
neus$stratumarea <- neus$Areanmi2 * 3.429904 # convert square nautical miles to square kilometers
neus[,stratumarea:=Areanmi2*3.429904]


# ==================
# = Remove bad spp =
# ==================
setkey(neus, spp)
neus <- neus[!is.na(spp)]
neus.spp.bad <- c("",'UNIDENTIFIED FISH', 'ILLEX ILLECEBROSUS EGG MOPS', 'LOLIGO PEALEII EGG MOPS')
neus <- neus[!.(neus.spp.bad)]


# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================
i <- sapply(neus, is.factor)
if(any(i)){
	neus[i] <- lapply(neus[i], as.character)
}



# =============
# = Aggregate =
# =============
neus[,datetime:=paste(as.character(year),substr(CRUISE6,5,6),"01",sep="-")]
# setkey(neus, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# neus2 <- neus[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=sumna), by=key(neus)]
# neus2 <- neus[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(neus)] # I think cpue should be avgd


# neus[,lu(STATION),by=c("CRUISE6","STRATUM")][,plot(table(V1))]
# =================================
# = # ===========================
# = # TODO GOOD IDEA HERE!! =
# =========================== =
# =================================
# neus[,lu(paste0(roundGrid(lat,0.25),roundGrid(lon,0.25))),by=c("year","stratum")][,plot(table(V1))] # this works like a charm! and will be applicable to all regions!!! instead of calling a "replicate" (K) a haul, or a unique day, etc, just define it spatially – substratum! So round the latitude and longitude of a haul to the nearest 1/3 of a º, and you can't have more than 9 reps!

setkey(neus, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
neus2 <- neus[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(neus)]


# ==============
# = Add region =
# ==============
neus2[,region:="NEFSC_NEUSSpring"]
neus2[,s.reg:="neus"]

# ========
# = Save =
# ========
save(neus2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/neus2.RData")


