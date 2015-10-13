

# =================
# = Load packages =
# =================
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)


# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# =====================
# = Set preliminaries =
# =====================
# String to begin directory
shelf.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/DFO_ScotianShelf/"

# ======================
# = Read in catch data =
# ======================
shelf.catch0 <- fread(paste(shelf.start, "gscat_adj_pinsky.csv", sep=""), colClasses=c(rep("character",4),rep("numeric",3), rep("character",3)), drop=c("SAMPWGT","MARKET","CALWT", "REMARKS", "SIZE_CLASS"))
shelf.catch <- shelf.catch0
setnames(shelf.catch, c("ADJ_TOTWGT", "ADJ_TOTNO"), c("wtcpue", "cntcpue"))
setkey(shelf.catch, MISSION, SETNO)


# ====================
# = Read in Set Data =
# ====================
shelf.set0 <- fread(paste(shelf.start, "gsinf_pinsky.csv", sep=""), colClasses=c(rep("character",5), rep("numeric",15),rep("character",3),"numeric",rep("character",2), rep("numeric",5)), drop=c("TIME","ELAT","ELONG","AREA","DUR","DIST","HOWS", "AUX", "SPEED","WIND","FORCE","CURNT","GEAR", "ETIME", "REMARKS", "START_DEPTH","END_DEPTH", "BOTTOM_SALINITY"))

# get good depth for shelf, based on what's available (malin line 112)
shelf.set0[is.na(DEPTH), DEPTH:=(DMIN+DMAX)/2]
shelf.set0[is.na(DEPTH)&is.na(DMIN)&!is.na(DMAX), DEPTH:=DMAX]
shelf.set0[is.na(DEPTH)&is.na(DMAX)&!is.na(DMIN), DEPTH:=DMIN]

setnames(shelf.set0, c("STRAT","DEPTH", "SURFACE_TEMPERATURE", "BOTTOM_TEMPERATURE"), c("stratum","depth", "stemp", "btemp"))

shelf.set <- shelf.set0[,list(MISSION, SETNO, SDATE, stratum, SLAT, SLONG, TYPE, depth, stemp, btemp)]


# ==================
# = Read in Strata =
# ==================
shelf.strata0 <- fread(paste(shelf.start, "ScotianShelf_strata.csv", sep=""), drop=c("depthzone_fathoms"))
shelf.strata0[,stratumarea:=stratarea_nmi2*1.852^2]
shelf.strata <- shelf.strata0[,list(stratum, stratumarea)]
shelf.strata[,stratum:=as.character(stratum)]
setkey(shelf.strata, stratum)


# ===================
# = Read in Species =
# ===================
shelf.spp <- fread(paste(shelf.start, "species list.csv", sep=""))
setnames(shelf.spp, "CODE", "SPEC")
shelf.spp[,SPEC:=as.character(SPEC)]
setkey(shelf.spp, SPEC)



# ============================
# = First Merge: Catch & Set =
# ============================
shelf.raw000 <- merge(shelf.catch, shelf.set, all.x=TRUE, all.y=FALSE)

# =============================
# = Also merge in stratumarea =
# =============================
shelf.raw000 <- merge(shelf.raw000, shelf.strata, all.x=TRUE, by="stratum")


# ================================
# = Add Dates and Haul to raw000 =
# ================================
# Add Dates to raw000
shelf.raw000[,SDATE:=gsub("-", "/", SDATE)]
shelf.raw000[,datetime:=as.POSIXct(SDATE, format="%y/%m/%d", tz="GMT")]
shelf.raw000[,month:=as.numeric(format.Date(datetime, format="%m"))]
shelf.raw000[,year:=as.numeric(format.Date(datetime, format="%Y"))]
# need to set datetime back to character so it doesn't conflict with other regions in combine.trawl.R
shelf.raw000[,datetime:=as.character(datetime)]

# Add Haul to raw000
shelf.raw000[,haulid:=paste(MISSION, formatC(SETNO, width=3, flag=0))]


# =====================================
# = Trim TYPE and month, create raw00 =
# =====================================
# malin trimmed TYPE on line 40, and trimmed month on line 76
cols2keep <- quote(list(year, month, datetime, haulid, stratum, stratumarea, SLAT, SLONG, depth, SPEC, stemp, btemp, wtcpue, cntcpue))
# shelf.raw00 <- shelf.raw000[TYPE==1 & month>=6 & month<= 8, list(year, month, datetime, haulid, stratum, SLAT, SLONG, depth, SPEC, stemp, btemp, wtcpue, cntcpue)] 
shelf.raw00 <- shelf.raw000[TYPE==1 & month>=6 & month<= 8, eval(cols2keep)] 


# ===============
# = Fix lat/lon =
# ===============
# malin line 119 & 120
shelf.raw00[,lat:=as.numeric(substr(SLAT,1,2))+as.numeric(substr(SLAT,3,4))/60]
shelf.raw00[,lon:= -as.numeric(substr(SLONG,1,2))-as.numeric(substr(SLONG,3,4))/60]


# ==============
# = Fix Strata =
# ==============
shelf.raw0 <- makeStrat(shelf.raw00, regName="shelf")


# =========================
# = Remove 9999 and -9999 =
# =========================
# replaces either of those values with NA; similar to malin line 133
rm9s(shelf.raw0)


# =======================================
# = Add in species, then begin trimming =
# =======================================
shelf.raw0 <- merge(shelf.raw0, shelf.spp, all.x=TRUE, by="SPEC")

# Note that Malin looked for some duplicates (year-lat-lon, same haulid), but I won't do this removal
# which(duplicated(shelf.raw0[,list(year, lat, lon)]) & !duplicated(shelf.raw0$haulid)) # Malin line 176
# Malin line 198 - 218 basically loop through rows with
# duplicated year-lat-lon, but different haulid,
# and pick a haulid to keep based on the number of non-na temp (s and b) readings it has
# I will eventually just sum (for wt/ cnt and temps) based on location/time/spp combinations


# do a check for duplicated tows
# shelf.raw0[,sum(grepl(",", haulid))] # malin line 222, but just a check. There are 0 cases where this is true.


shelf.raw <- shelf.raw0[!grepl("UNIDENT| UNID|^UNID", spp) & !grepl("EGGS|PURSE", spp) & !grepl("EGGS", common) & !is.na(spp) & spp!="" & haulid!="NED2010027-201",]
# they had eggs, unid, purses (for skates), so I removed those "spp"


# shelf.raw[,unique(common)]
# shelf.raw[,unique(spp)]
# need to watch out for the following
	# spp:
		# c("LOLIGINIDAE,OMMASTREPHIDAE F.", "PROTOBRANCHIA, HETERODONTA", "LITHODES/NEOLITHODES","POLYCHAETA C.,LARGE")
		# also, lots of these end in "F.", which I'm assuming means "family", which should maybe be removed
		# similarly, "ECHINODERMATA P.", but there are a lot that end in ".P"
		# watch out for "S.O.", "O.", "C.", "F.", "P.", "S.C."
		# if a genus, can end in "spp.", "SP", "SP.", "SPP." ... maybe "(NS)" means "no species"
	# common:
		# c("SEAWEED,(ALGAE),KELP", "LEMONWEED", "SEA GRAPES", "BRILL/WINDOWPANE", "BLUE ANTIMORA/HAKE", "SPIDER/(QUEEN,SNOW)UNID")
		# I'm assumign that a lot of the vague names are due to poorly classified spp
		# there are also a lot of corals, I'm not sure that they should be included
		
		
bad.shelf.spp <- c("LOLIGINIDAE,OMMASTREPHIDAE F.", "PROTOBRANCHIA, HETERODONTA", "LITHODES/NEOLITHODES","POLYCHAETA C.,LARGE", "SEA CORALS (NS)")
vague.shelf.pat <- "\\sS\\.[OC]\\.|\\s[OCFP]\\." #"\\sS\\.O\\.|\\sO\\.|\\sC\\.|\\F\\.|"
shelf <- shelf.raw[!grepl(vague.shelf.pat, spp) & !spp%in%bad.shelf.spp, list(year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, stemp, btemp, wtcpue, cntcpue)]

# shelf[,unique(spp)]
# dim(shelf) # 85569, 13

setkey(shelf, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
shelf2 <- shelf[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(shelf)]
# dim(shelf2) # 85553, 13 (16 duplicate rows). I think Malin trimmed his to ~61k, but he was only interested in spp present every year

# ===============
# = Add regions =
# ===============
shelf2[,region:="DFO_ScotianShelf"]
shelf2[,s.reg:="shelf"]


# ========
# = Save =
# ========
save(shelf2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/shelf2.RData")

