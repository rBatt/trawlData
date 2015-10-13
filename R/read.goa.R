

# ==================
# = Load Libraries =
# ==================
# NOTE: using the data.table code took 0.790 seconds, whereas the original code took 5.567 seconds
library(bit64)
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
goaStrata <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/goaStrata.csv", header=TRUE, sep=",", select=c("StratumCode","Areakm2"))
goaStrata[,StratumCode:=as.character(StratumCode)]
setkey(goaStrata, StratumCode)
setnames(goaStrata, "StratumCode", "STRATUM")


goa.files <- list.files("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/")
goa.files <- goa.files[!goa.files%in%"goaStrata.csv"]
n.goa <- length(goa.files)

for(i in 1:n.goa){ # loop through data files and combine them. Assumes that column headers match
	t.goa.file <- goa.files[i]
	t.goa.name <- paste("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/", t.goa.file, sep="")
	if(i==1){
		# goa.raw <- fread(t.goa.name, header=TRUE, sep=",", quote="")
		goa.raw <- fread(t.goa.name)
		setnames(goa.raw, names(goa.raw), gsub("^\\s* | \\s*$", "", names(goa.raw)))
	}else{
		t.goa.raw <- fread(t.goa.name)
		setnames(t.goa.raw, names(t.goa.raw), gsub("^\\s* | \\s*$", "", names(t.goa.raw)))
		goa.raw <- rbind(goa.raw, t.goa.raw)
	}
	
}

goa.raw[,STRATUM:=as.character(STRATUM)]
rmWhite(goa.raw) # remove whitespace in the elements of each column
rm9s(goa.raw) # check each column for 9999, and replace with NA


setkey(goa.raw, STRATUM)

goa <- merge(goa.raw, goaStrata, all.x=TRUE)

goa[,haulid:=paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE, width=3, flag=0), formatC(HAUL, width=3, flag=0), sep='-')]


# =============
# = Set Names =
# =============
setnames(goa, c("STRATUM", "YEAR", "LATITUDE", "LONGITUDE", "BOT_DEPTH", "SCIENTIFIC", "WTCPUE", "Areakm2", "BOT_TEMP", "SURF_TEMP", "DATETIME"), c("stratum", "year", "lat", "lon", "depth", "spp", "wtcpue", "stratumarea", "btemp", "stemp", "datetime"))



# ==============
# = Fix Strata =
# ==============
goa <- makeStrat(goa, regName="goa")



# ==================
# = Delete bad spp =
# ==================
setkey(goa, spp)
goa.spp.bad <- c("","Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")
goa <- goa[!.(goa.spp.bad)]




# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================
i <- sapply(goa, is.factor)
if(any(i)){
	goa[i] <- lapply(goa[i], as.character)
}


goa[.(c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')), spp:='Lepidopsettasp.']; setkey(goa, spp)
goa[.(c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')), spp:='Myoxocephalussp.']; setkey(goa, spp)
goa[.(c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajgoanterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')), spp:='Bathyrajasp.']; setkey(goa, spp)



# =============
# = Aggregate =
# =============
# setkey(goa, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# goa2 <- goa[j=lapply(list(wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=sumna), by=key(goa)]
# goa2 <- goa[j=lapply(list(wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=meanna), by=key(goa)] # I think cpue should be avgd

setkey(goa, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
goa2 <- goa[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=meanna), by=key(goa)]


# ==============
# = Add region =
# ==============
goa2[,region:="AFSC_GOA"]
goa2[,s.reg:="goa"]

# ========
# = Save =
# ========
save(goa2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/goa2.RData")








