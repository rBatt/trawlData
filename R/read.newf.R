
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
newf.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/DFO_Newfoundland/"


# =====================================
# = Read in Tables (spp, strata, etc) =
# =====================================
# Read in Species
newf.spp <- fread(paste(newf.start, "Tables/", "GFSPCY.CODE_2012-07-05.csv", sep=""), drop="V1")


# =======================
# = Read in Survey Info =
# =======================
newf.surv1 <- fread(paste(newf.start, "Tables/", "surveys_table.csv", sep=""), colClasses=rep("character", 16), drop=c("Comment"))
newf.surv2 <- fread(paste(newf.start, "Tables/", "surveys_table2009-2011.csv", sep=""), colClasses=rep("character", 7))


# ========================
# = Read in Strata Files =
# ========================
		
# Add strata areas
strat1 <- read.fwf(paste(newf.start, "Tables/", "stratum_areas.ver1.fwf", sep=""), 
	widths=c(
		3, #stratum
		4, # area in square nautical miles
		4, # max depth
		3  # NAFO division
	), 
	col.names = c('stratum', 'area', 'maxdepth', 'nafo'), 
	stringsAsFactors=FALSE
)
	
strat2 <- read.fwf(paste(newf.start, "Tables/", "stratum_areas.ver2.fwf", sep=""), widths=c(3, 4, 4, 3), col.names = c('stratum', 'area', 'maxdepth', 'nafo'), stringsAsFactors=FALSE)

strat3 <- read.fwf(paste(newf.start, "Tables/", "stratum_areas.ver3.fwf", sep=""), widths=c(3, 4, 4, 3), col.names = c('stratum', 'area', 'maxdepth', 'nafo'), stringsAsFactors=FALSE)

strat4 <- read.fwf(paste(newf.start, "Tables/", "stratum_areas.ver4.fwf", sep=""), widths=c(3, 4, 4, 3), col.names = c('stratum', 'area', 'maxdepth', 'nafo'), stringsAsFactors=FALSE)
	
# Convert square nautical miles to square meters
strat1$aream2 <- strat1$area*3429904
strat2$aream2 <- strat2$area*3429904
strat3$aream2 <- strat3$area*3429904
strat4$aream2 <- strat4$area*3429904

# Trim out spaces in NAFO division names
strat1$nafo <- gsub(" ", "", strat1$nafo)
strat2$nafo <- gsub(" ", "", strat2$nafo)
strat3$nafo <- gsub(" ", "", strat3$nafo)
strat4$nafo <- gsub(" ", "", strat4$nafo)




# ======================
# = Read in Data Files =
# ======================
newf.files <- list.files(path=paste(newf.start, "Data/", sep=""), pattern = "199[23456789]|200[0123456789]|201[012]")

n <- numeric(0)
ch <- numeric(0)
newf.raw00 <- data.frame(recordtype = n, vessel = n, trip = n, set = n, yearl=n, monthl = n, dayl = n, settype = n, stratum = n, nafo = ch, unitarea = ch, light = n, winddir = n, windforce = n, sea = n, bottom = n, timel = n, duration = n, distance = n, operation = n, depth = n, depthmin = n, depthmax = n, depthbottom = n, surftemp = n, bottemp = n, latstart = n, lonstart = n, posmethod = n, gear = n, sppcode = n, num = n, wgt = n, latend = n, lonend = n, bottempmeth = n, geardevice = n)


for(i in 1:length(newf.files)){ # for each file
	if(i == 1) print(length(newf.files)) # 45 files
	print(i)
	indata <- read.fwf(file=paste(newf.start, "Data/", newf.files[i], sep=''), widths=c(
	1, # record type
	2, # vessel
	3, # trip
	3, # set
	2, # year
	2, # mo
	2, # day
	2, # set type
	3, # stratum
	2, # nafo
	3, # unit
	3, # light
	1, # winddir
	1, # wind force
	1, # sea
	1, # bottom type
	4, # time
	3, # duration
	3, # distance 
	1, # operation
	4, # depth mean
	4, # depth min
	4, # depth max
	4, # depth bottom
	3, # temp surf
	3, # temp bot
	5, # lat start
	5, # lon start
	1, # pos meth
	4, # gear
	4, # sppcode
	6, # number
	7, # wgt 
	5, # lat end
	5, # lon end
	2, # bot temp device
	2), # gear mon device
	header= FALSE, stringsAsFactors = FALSE)

	names(indata) <- c('recordtype', 'vessel', 'trip', 'set', 'yearl', 'monthl', 'dayl', 'settype', 'stratum', 'nafo', 'unitarea', 'light', 'winddir', 'windforce', 'sea', 'bottom', 'timel', 'duration', 'distance', 'operation', 'depth', 'depthmin', 'depthmax', 'depthbottom', 'surftemp', 'bottemp', 'latstart', 'lonstart', 'posmethod', 'gear', 'sppcode', 'num', 'wgt', 'latend', 'lonend', 'bottempmeth', 'geardevice')

	newf.raw00 <- rbind(newf.raw00, indata)
	print(dim(newf.raw00))
}
newf.raw0 <- newf.raw00


# ========================
# = Subset data based on =
# ========================
ss1 <- newf.raw0$operation %in% c(1,2) & newf.raw0$recordtype == 6 # 6 is biological data, 5 is set information
newf.raw0 <- newf.raw0[ss1,]

ss2 <- newf.raw0$gear == 61 & !is.na(newf.raw0$gear) # CAMPELEN 1800 SHRIMP TRAWL--LINED
newf.raw0 <- newf.raw0[ss2,]

ss3 <- newf.raw0$settype == 1
newf.raw0 <- newf.raw0[ss3,]


# =========================
# = Convert to data.table =
# =========================
newf.raw <- data.table(newf.raw0)


# ========================
# = Create unique haulid =
# ========================
newf.raw[,haulid:=paste(formatC(vessel, width=2, flag=0), formatC(trip, width=3, flag=0), formatC(set, width=3, flag=0, format='d'), sep='-')]


# =============
# = Fix years =
# =============
setnames(newf.raw, "yearl", "year")
newf.raw[,year:=year+1900]
newf.raw[newf.raw$year<1950, year:=year+100]


# ============
# = Add date =
# ============
newf.raw[,datetime:=as.POSIXct(paste(year, monthl, dayl, sep="-"), tz="Canada/Newfoundland")]
# newf.raw[,datetime:=format.Date(datetime, tz="GMT")]
newf.raw[,julian:=as.integer(format.Date(datetime, format="%j"))]
# change back to character so doesn't cause conflict in combine.trawl.R
newf.raw[,datetime:=as.character(datetime)]


# ==================
# = Format lat/lon =
# ==================
# first, fix latitude
lat.1 <- newf.raw[,(latstart>0&latend>0)]
lat.2 <- newf.raw[,(latstart>0&latend==0)]
# lat.3 <- newf.raw[,(latstart==0&latend>0)] # no instances of this case

newf.raw[lat.1,lat:=(as.numeric(substr(latstart, 1, 2)) + as.numeric(substr(latstart, 3, 5))/600 + as.numeric(substr(latend, 1, 2)) + as.numeric(substr(latend, 3, 5))/600)/2]

newf.raw[lat.2, lat:=as.numeric(substr(latstart, 1, 2)) + as.numeric(substr(latstart, 3, 5))/600]

# fix longitude
lon.1 <- newf.raw[,(lonstart>0&lonend>0)]
lon.2 <- newf.raw[,(lonstart>0&lonend==0)]

newf.raw[lon.1, lon:=-(as.numeric(substr(lonstart, 1, 2)) + as.numeric(substr(lonstart, 3, 5))/600 + as.numeric(substr(lonend, 1, 2)) + as.numeric(substr(lonend, 3, 5))/600)/2]

newf.raw[lon.2, lon:=-(as.numeric(substr(lonstart, 1, 2)) + as.numeric(substr(lonstart, 3, 5))/600)]

# ================================
# = Standardize count and weight =
# ================================
newf.raw[,area:=distance/10 * 1852 * 55.25 * 0.3048]

area.out <- newf.raw[,area]

bad15 <- newf.raw$duration == 15 & (area.out==0 | is.na(area.out))
area.out[bad15] = mean(area.out[newf.raw$duration==15 & area.out > 0 & !is.na(area.out) & newf.raw$year %in% newf.raw$year[bad15]])

bad25 <- newf.raw$duration == 25 & (area.out==0 | is.na(area.out))
area.out[bad25] = mean(area.out[newf.raw$duration==25 & area.out > 0 & !is.na(area.out) & newf.raw$year %in% newf.raw$year[bad25]])

bad30 <- newf.raw$duration == 30 & (area.out==0 | is.na(area.out))
area.out[bad30] = mean(area.out[newf.raw$duration==30 & area.out > 0 & !is.na(area.out) & newf.raw$year %in% newf.raw$year[bad30]])

meanarea <- mean(area.out, na.rm=TRUE)

newf.raw[,area:=area.out]
newf.raw[,wtcpue:=(wgt/100)*(meanarea/area)]
newf.raw[,numcpue:=num*(meanarea/area)]

# newf.raw[,list(sum(is.na(wtcpue)), sum(is.na(numcpue)))] # malin got 27, 448, I get 27, 451.


# ====================
# = Fix temperatures =
# ====================
# Change column class to numeric
newf.raw[,surftemp:=as.numeric(surftemp)]
newf.raw[,bottemp:=as.numeric(bottemp)]

# Fix the surface temp
fixT.surf <- newf.raw[,surftemp >= 900 & !is.na(surftemp)]
newf.raw[fixT.surf, surftemp:= -(surftemp - 900)/10]

fixT.surf2 <- newf.raw[,surftemp < 900 & surftemp > 0 & !is.na(surftemp)]
newf.raw[fixT.surf2, surftemp:=surftemp/10]
summary(newf.raw$surftemp) # 379,007 NAs (of 383,710 rows): nearly all missing # Ryan gets 379,007 NA's too

# Fix the bottom temp
fixT.bot <- newf.raw[,bottemp >= 900 & !is.na(bottemp)]
newf.raw[fixT.bot, bottemp:= -(bottemp - 900)/10]

fixT.bot2 <- newf.raw[,bottemp < 900 & bottemp > 0 & !is.na(bottemp)]
newf.raw[fixT.bot2, bottemp:=bottemp/10]
summary(newf.raw$bottemp) # only 6459 NAs


# =====================
# = Add Species Names =
# =====================
setkey(newf.raw, sppcode)
setkey(newf.spp, sppcode)
newf.raw <- merge(newf.raw, newf.spp, all.x=TRUE)


# =======================
# = Rename some columns =
# =======================
setnames(newf.raw, c("vessel", "trip", "set"), c("svvessel", "cruise", "tow"))


# ====================================
# = Identifying Spring/ Fall Surveys =
# ====================================
fallseries <- c(as.character(newf.surv1$CRUISE[newf.surv1$Series %in% c('2GH - Stratified Random Bottom Trawl Survey - Campelen 1800', 'Fall - Stratified Random Bottom Trawl Survey - Campelen 1800')]), as.character(newf.surv2$cruise[newf.surv2$season=='fall']))

springseries <- c(as.character(newf.surv1$CRUISE[newf.surv1$Series %in% c('Annual 3P - Stratified Random Bottom Trawl Survey - Campelen 1800', 'Spring 3LNO - Stratified Random Bottom Trawl Survey - Campelen 1800')]), as.character(newf.surv2$cruise[newf.surv2$season == 'spring']))

cruiseid <- paste(newf.raw$svvessel, formatC(newf.raw$cruise, width=3, flag=0), sep='')

is.fall <- cruiseid %in% fallseries
is.spring <- cruiseid %in% springseries
newf.season <- rep(NA, nrow(newf.raw))
newf.season[is.fall] <- "fall"
newf.season[is.spring] <- "spring"

bad.series <- is.na(newf.season)

newf.raw[, season:=newf.season]
setkey(newf.raw, season)


# =========================
# = Fix fall spring dates =
# =========================
# Set dates for spring
newf.raw["spring",yearsurv:=year]
newf.raw["spring",juliansurv:=julian] # might not need this ...


# Fix years for spring and fall (fall might cover 2 calendar years per 1 survey)
newf.raw["fall", yearsurv:=year]

fallMonth4 <- newf.raw["fall",][,monthl<4] # logical index for fall season when the month is <4
fallYear1 <- newf.raw["fall", ][,yearsurv] # record the original fall years
fallYear2 <- fallYear1 # set a new fall year to the original fall years
fallYear2[fallMonth4] <- fallYear1[fallMonth4] - 1 # change the new fall years by subtracting one when when their month<4
newf.raw["fall",yearsurv:=fallYear2] # In the data.table, replace the old fall years with the new fall years (old[month<4]-1)
newf.raw["fall",juliansurv:=julian] # might not need this ...


# ==================
# = Trim to Season =
# ==================
setkey(newf.raw, season)
newf00 <- newf.raw["fall"]


# =======================
# = Trim wrong duration =
# =======================
# newf00[,sum(duration>60)] # 26 of them, just like in Malin line 349
newf0 <- newf00[duration<=60,]


# =====================
# = Duplicated Hauls? =
# =====================
blah <- newf0[duplicated(paste(haulid,spp, common, season)) | duplicated(paste(haulid,spp, common, season), fromLast=TRUE),]
setkey(blah, haulid)
print(blah[, list(haulid, year, season, numcpue, wtcpue, spp, common)], nrow=Inf)


# ========================
# = Remove bad spp names =
# ========================
newf <- newf0
# datafal$spp = as.character(datafal$spp)
tab <- table(newf$spp, newf$year);# write.csv(tab, 'Output/sppbyyearfal.csv') # for checking by eye

# i <- newf$spp %in% c('ARTEDIELLUS ATLANTICUS', 'ARTEDIELLUS UNCINATUS')
# newf$spp[i] <- 'ARTEDIELLUS  SP.'

# i <- newf$spp %in% c('BUCCINUM  SP.', 'BUCCINUM TOTTENI', 'BUCCINUM UNDATUM')
# newf$spp[i] <- 'BUCCINIDAE' # didn't search for all included genera (many!)

i <- newf$spp %in% c('CHIONOECETES OPILIO FEMALE', 'CHIONOECETES OPILIO MALE')
newf$spp[i] <- 'CHIONOECETES OPILIO'

i <- newf$spp %in% c('EUALUS GAIMARDII BELCHERI', 'EUALUS GAIMARDII GAIMARDII')
newf$spp[i] <- 'EUALUS GAIMARDII'

i <- newf$spp %in% c('EUMICROTREMUS SPINOSUS VARIABILIS')
newf$spp[i] <- 'EUMICROTREMUS SPINOSUS'

# i <- newf$spp %in% c('GAIDROPSARUS ARGENTATUS', 'GAIDROPSARUS ENSIS')
# newf$spp[i] <- 'GAIDROPSARUS  SP.'

# i <- newf$spp %in% c('GONATUS FABRICII')
# newf$spp[i] <- 'GONATUS  SP.'

# i <- newf$spp %in% c('GORGONOCEPHALUS ARCTICUS', 'GORGONOCEPHALUS SP.')
i <- newf$spp %in% c('GORGONOCEPHALUS SP.')
newf$spp[i] <- 'GORGONOCEPHALUS ARCTICUS'

# i <- newf$spp %in% c('HYAS ARANEUS', 'HYAS COARCTATUS')
# newf$spp[i] <- 'HYAS  SP.'

# i <- newf$spp %in% c('LIPARIS ATLANTICUS', 'LIPARIS FABRICII', 'LIPARIS GIBBUS', 'LIPARIS LIPARIS', 'LIPARIS TUNICATUS')
# newf$spp[i] <- 'LIPARIDAE'

i <- newf$spp %in% c('LITHODES  SP.', 'LITHODES  SP.', 'NEOLITHODES  SP.', 'NEOLITHODES GRIMALDII')
newf$spp[i] <- 'LITHODIDAE'

# i <- newf$spp %in% c('LYCENCHELYS PAXILLUS', 'LYCENCHELYS SARSI', 'LYCENCHELYS VERRILLI')
# newf$spp[i] <- 'LYCENCHELYS  SP.'

# i <- newf$spp %in% c('NOTACANTHUS NASUS')
# newf$spp[i] <- 'NOTACANTHIDAE'

i <- newf$spp %in% c('PANDALUS BOREALIS(FEE 1ST W/HR)', 'PANDALUS BOREALIS(FEE 1ST W/O HR)', 'PANDALUS BOREALIS(FEE 1ST)', 'PANDALUS BOREALIS(FEE OVIG.)', 'PANDALUS BOREALIS(FEE(1+) W/HR)', 'PANDALUS BOREALIS(FEE(1+) W/O HR)', 'PANDALUS BOREALIS(MALE)', 'PANDALUS BOREALIS(TRANS. W/HR)', 'PANDALUS BOREALIS(TRANS. W/O HR)')
newf$spp[i] <- 'PANDALUS BOREALIS'

i <- newf$spp %in% c('PANDALUS MONTAGUI(FEE 1ST W/HR)', 'PANDALUS MONTAGUI(FEE 1ST W/O HR)', 'PANDALUS MONTAGUI(FEE OVIG.)', 'PANDALUS MONTAGUI(FEE(1+) W/HR)', 'PANDALUS MONTAGUI(FEE(1+) W/O HR)', 'PANDALUS MONTAGUI(MALE)', 'PANDALUS MONTAGUI(TRANS. W/HR)', 'PANDALUS MONTAGUI(TRANS.W/O HR))')
newf$spp[i] <- 'PANDALUS MONTAGUI'

i <- newf$spp %in% c('ANOTOPTERIDAE', 'PARALEPIS  SP.') #c('PARALEPIS  SP.', 'PARALEPIS BREVIS (ATLANTICA)', 'PARALEPIS COREGONOIDES BOREALIS', 'ANOTOPTERIDAE', 'ANOTOPTERUS PHARAO', 'NOTOLEPIS RISSOI KROYERI') # see http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=162532: Anotopteridae should be Paralepididae
# rdb changed to maintain more specific tax info to be checked by gnr_resolve
newf$spp[i] <- 'PARALEPIDIDAE' # most observations seem to be at family level

# i <- newf$spp %in% c('PASIPHAEA MULTIDENTATA', 'PASIPHAEA TARDA')
# newf$spp[i] <- 'PASIPHAEA  SP.'

i <- newf$spp %in% c('SCOPELOSAURUS  SP.')
newf$spp[i] <- 'SCOPELOSAURIDAE' # according to ITIS, this should be 	Scopelosauridae

# i <- newf$spp %in% c('SIMENCHELYS PARASITICUS')
# newf$spp[i] <- 'SIMENCHELYIDAE'

# i <- newf$spp %in% c('TRIGLOPS MURRAYI', 'TRIGLOPS NYBELINI', 'TRIGLOPS PINGELI')
# newf$spp[i] <- 'TRIGLOPS  SP.'

# remove unidentified spp
i <- !(newf$spp %in% c('EGGS, FISH(SPAWN)', 'EGGS, INVERTEBRATE', 'EGGS, SKATE CASES', 'EGGS, UNIDENTIFIED', 'OFFAL, OTHER', 'PLANT MATERIAL', 'SHELLS', 'STONE', 'UNIDENTIFIED FISH', 'UNIDENTIFIED MATERIAL'))
newf <- newf[i,]


# =======================================
# = Trim down to known spp, and to fall =
# =======================================
dim(newf) # 378070, 50
newf[,sum(spp=="", na.rm=TRUE)] # 454
newf[,sum(is.na(spp))] # 120

newf <- newf["fall"] # trim to only fall
dim(newf) # 257486, 50
newf[,sum(spp=="", na.rm=TRUE)] # 234
newf[,sum(is.na(spp))] # 26

setkey(newf, spp)
newf <- newf[spp!=""&!is.na(spp),]
dim(newf) # 257226, 50


# ================
# = Trim columns =
# ================
# setkey(neus, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
newf <- newf[,list(yearsurv, datetime, spp, haulid, stratum, area, lat, lon, depth, bottemp, surftemp, wtcpue, numcpue)]
setnames(newf, old=c("yearsurv", "area", "bottemp", "surftemp", "numcpue"), new=c("year", "stratumarea", "btemp", "stemp", "cntcpue"))


# ==============
# = Fix Strata =
# ==============
newf <- makeStrat(newf, regName="newf")


# =============
# = Aggregate =
# =============
# setkey(newf, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# newf2 <- newf[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=sumna), by=key(newf)] # ~5500 fewer rows after summing
# newf2 <- newf[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(newf)] # I think cpue should be averaged

setkey(newf, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
newf2 <- newf[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(newf)]

# ==============
# = Add Region =
# ==============
newf2[,region:="DFO_Newfoundland_Fall"] # malin's version of the region name
newf2[,s.reg:="newf"] # my short region (s.reg) name

save(newf2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/newf2.RData")



