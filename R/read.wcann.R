

library(data.table)
library(bit64)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)


# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


wcann.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NWFSC/2014-02-11/"

wcann.fish <- fread(paste(wcann.start,"wcann2003_2012fish.csv", sep=""))
wcann.haul <- fread(paste(wcann.start,"wcann2003_2012haul.csv", sep=""))
wcann.invert <- fread(paste(wcann.start,"wcann2003_2012invert.csv", sep=""))

# Change names of the data.tables to make.names()
setnames(wcann.fish, names(wcann.fish), make.names(names(wcann.fish)))
setnames(wcann.haul, names(wcann.haul), make.names(names(wcann.haul)))
setnames(wcann.invert, names(wcann.invert), make.names(names(wcann.invert)))

# Remove leading and trailing whitespace
rmWhite(wcann.fish)
rmWhite(wcann.haul)
rmWhite(wcann.invert)

# merge fish and inverts
wcann.invert[,Individual.Average.Weight..kg.:=as.numeric(NA)]
wcann.catch <- rbind(wcann.fish[, names(wcann.invert), with=FALSE], wcann.invert)

# make Trawl.Id a character for catch and haul
wcann.catch[,Trawl.Id:=as.character(Trawl.Id)]
wcann.haul[,Trawl.Id:=as.character(Trawl.Id)]

# merge catch and haul by Trawl.Id
wcann <- merge(wcann.catch, wcann.haul, by=intersect(names(wcann.catch), names(wcann.haul)), all.x=TRUE)

# ===============
# = Add haul id =
# ===============
wcann[,haulid:=Trawl.Id]


# ================
# = Extract year =
# ================
wcann[,year:=as.numeric(gsub('Cycle ', '', Survey.Cycle))]


# ================
# = Name columns =
# ================
setnames(wcann, c("Best.Latitude..dd.", "Best.Longitude..dd.", "Best.Depth..m.", "Species", "Temperature.At.the.Gear..degs.C.", "Trawl.Start.Time"), c("lat", "lon", "depth", "spp", "btemp", "datetime"))


# ==============
# = Fix Strata =
# ==============
wcann[,stratum:=paste(floor(lat)+0.5, floor(lon/100)*100+50, sep="-")]
wcann <- makeStrat(wcann, regName="wcann")



# ================
# = Stratum area =
# ================
wcann[,stratumarea:=calcarea(cbind(lon, lat)), by=stratum]




# ============
# = Add cpue =
# ============
wcann[,wtcpue:=Haul.Weight..kg./Area.Swept.by.the.Net..hectares.]
wcann[,cntcpue:=(Haul.Weight..kg./Individual.Average.Weight..kg.)/Area.Swept.by.the.Net..hectares.]



# ==================
# = Remove bad spp =
# ==================
setkey(wcann, spp)
wcann.spp.bad <- c("","Apristurus brunneus egg case", "gastropod eggs", "Selachimorpha egg case")
wcann <- wcann[!.(wcann.spp.bad)]


# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================
i <- sapply(wcann, is.factor)
if(any(i)){
	wcann[i] <- lapply(wcann[i], as.character)
}


wcann[.(c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')), spp:='Lepidopsettasp.']; setkey(wcann, spp)
wcann[.(c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajwcannnterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')), spp:='Bathyrajasp.']; setkey(wcann, spp)



# =============
# = Aggregate =
# =============
wcann[,stemp:=as.numeric(NA)]
# setkey(wcann, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# wcann2 <- wcann[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=sumna), by=key(wcann)]
# wcann2 <- wcann[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(wcann)] # I think cpue should be avgd

setkey(wcann, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
wcann2 <- wcann[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(wcann)]


# ==============
# = Add region =
# ==============
wcann2[,region:="NWFSC_WCAnn"]
wcann2[,s.reg:="wcann"]


# ========
# = Save =
# ========
save(wcann2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/wcann2.RData")

