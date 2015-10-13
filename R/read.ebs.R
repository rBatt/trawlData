
require(bit64)
require(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# ====================
# = Read in Raw Data =
# ====================
ebsStrata <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/ebsStrata.csv", select=c("StratumCode","Areakm2")) # ntoe that the last row is basically blank, except the 2nd column contains NA and the 3rd column contains the sum area (sum of 3rd column)
setnames(ebsStrata, "StratumCode", "STRATUM")
ebsStrata[,STRATUM:=as.character(STRATUM)]
setkey(ebsStrata, STRATUM)

ebs.files <- list.files("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/")
ebs.files <- ebs.files[!ebs.files%in%"ebsStrata.csv"]
n.ebs <- length(ebs.files)

for(i in 1:n.ebs){ # loop through data files and combine them. Assumes that column headers match
	t.ebs.file <- ebs.files[i]
	t.ebs.name <- paste("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/", t.ebs.file, sep="")
	if(i==1){
		ebs.raw <- fread(t.ebs.name)
		setnames(ebs.raw, names(ebs.raw), gsub("^\\s* | \\s*$", "", names(ebs.raw)))
	}else{
		t.ebs.raw <- fread(t.ebs.name)
		setnames(t.ebs.raw, names(t.ebs.raw), gsub("^\\s* | \\s*$", "", names(t.ebs.raw)))
		ebs.raw <- rbind(ebs.raw, t.ebs.raw)
	}
}

rmWhite(ebs.raw) # remove whitespace in the elements of each column
rm9s(ebs.raw) # check each column for 9999, and replace with NA
ebs.raw[,STRATUM:=as.character(STRATUM)]
setkey(ebs.raw, STRATUM)


ebs <- merge(ebs.raw, ebsStrata, all.x=TRUE)


ebs[,haulid:=paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE, width=3, flag=0), formatC(HAUL, width=3, flag=0), sep='-')]


# =============
# = Fix names =
# =============
setnames(ebs, c("STRATUM", "YEAR", "LATITUDE", "LONGITUDE", "BOT_DEPTH", "SCIENTIFIC", "WTCPUE", "Areakm2", "BOT_TEMP", "SURF_TEMP", "DATETIME"), c("stratum", "year", "lat", "lon", "depth", "spp", "wtcpue", "stratumarea", "btemp", "stemp", "datetime"))


# ==============
# = Fix Strata =
# ==============
ebs <- makeStrat(ebs, regName="ebs")



# ==================
# = Delete bad spp =
# ==================
setkey(ebs, spp)
ebs.spp.bad <- c("","Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")
ebs <- ebs[!.(ebs.spp.bad)]


# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================
i <- sapply(ebs, is.factor)
if(any(i)){
	ebs[i] <- lapply(ebs[i], as.character)
}



ebs[.(c('Hippoglossoides elassodon', 'Hippoglossoides robustus')), spp:='Hippoglossoides sp.']; setkey(ebs, spp)
ebs[.(c('Atheresthesevermanni', 'Atheresthesstomias')), spp:='Atheresthessp.']; setkey(ebs, spp)
ebs[.(c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')), spp:='Lepidopsettasp.']; setkey(ebs, spp)
ebs[.(c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')), spp:='Myoxocephalussp.']; setkey(ebs, spp)
ebs[.(c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajebsnterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')), spp:='Bathyrajasp.']; setkey(ebs, spp)


# =============
# = Aggregate =
# =============
# setkey(ebs, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# ebs2 <- ebs[j=lapply(list(wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=sumna), by=key(ebs)]
# ebs2 <- ebs[j=lapply(list(wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=meanna), by=key(ebs)] # I think cpue should be avgd

setkey(ebs, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
ebs2 <- ebs[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=meanna), by=key(ebs)]


# ==============
# = Add region =
# ==============
ebs2[,region:="AFSC_EBS"]
ebs2[,s.reg:="ebs"]


# ========
# = Save =
# ========
save(ebs2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/ebs2.RData")

