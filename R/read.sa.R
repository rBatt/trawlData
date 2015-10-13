


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


# =====================
# = Set preliminaries =
# =====================
# String to begin directory
sa.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/SEAMAP-SA/"


# ======================
# = Read in Catch Data =
# ======================
sa.catch.class <- c(rep("character",12), rep("numeric",4), rep("character",2), "numeric", "character",rep("numeric",2),"character","numeric", rep("character",7), rep("numeric",11), rep("character",2))
sa.catch.drop <- c("EVENTNAME", "VESSELNAME", "GEARNAME", "SPECIESCODE", "MRRI_CODE", "SPECIESSUBWEIGHT", "SPECIESWGTPROCESSED", "WEIGHTMETHODDESC", "ORGWTUNITS", "CATCHSUBSAMPLED", "CATCHWEIGHT", "CATCHSUBWEIGHT", "TIMESTART", "DURATION", "TOWTYPETEXT", "ACCSPGRIDCODE", "EVENTTYPEDESCRIPTION", "SALINITYSURFACE", "SALINITYBOTTOM", "SDO", "BDO", "TEMPAIR", "SPECSTATUSDESCRIPTION", "LASTUPDATED")
sa.catch00 <- fread(paste(sa.start, "malinpinsky.Coastalbiomass.csv", sep=""), colClasses=sa.catch.class, drop=sa.catch.drop)


# ===============================
# = Read in Strata info (depth) =
# ===============================
sa.strata <- fread(paste(sa.start, "malinpinsky.CoastalEvent.csv", sep=""), select=c("COLLECTIONNUMBER","DEPTHSTART","DEPTHEND"))
sa.strata <- sa.strata[COLLECTIONNUMBER!="",]
setnames(sa.strata, "COLLECTIONNUMBER", "haulid")


# =====================================================
# = Fix terrible ="asd" convention used in 10 columns =
# =====================================================
# Fix catch first
# names(sa.catch00)[grepl("[\"=]", head(sa.catch00)[1,])]
sa.catch00[,PROJECTNAME:=gsub("[\"=]", "", PROJECTNAME)]
sa.catch00[,PROJECTAGENCY:=gsub("[\"=]", "", PROJECTAGENCY)]
sa.catch00[,COLLECTIONNUMBER:=gsub("[\"=]", "", COLLECTIONNUMBER)]
sa.catch00[,GEARCODE:=gsub("[\"=]", "", GEARCODE)]
sa.catch00[,SPECIESSCIENTIFICNAME:=gsub("[\"=]", "", SPECIESSCIENTIFICNAME)]
sa.catch00[,SPECIESCOMMONNAME:=gsub("[\"=]", "", SPECIESCOMMONNAME)]
sa.catch00[,LOCATION:=gsub("[\"=]", "", LOCATION)]
sa.catch00[,REGION:=gsub("[\"=]", "", REGION)]
sa.catch00[,DEPTHZONE:=gsub("[\"=]", "", DEPTHZONE)]
sa.catch00[,STATIONCODE:=gsub("[\"=]", "", STATIONCODE)]

# Fix strata column
sa.strata[,haulid:=gsub("[\"=]", "", haulid)]
setkey(sa.strata, haulid)


# ======================================================
# = Figure out of some columns contain anything useful =
# ======================================================
# project name
# sa.catch00[,unique(PROJECTNAME)] # not all are Coastal Survey
# sa.catch00[,sum(PROJECTNAME=="")] # 8 are ""

# project agency
# sa.catch00[,unique(PROJECTAGENCY)]
# sa.catch00[,sum(PROJECTAGENCY=="")] # same case for the agency

# In fact, where project name =="", all columns are NA for those rows
setnames(sa.catch00, c("DATE", "COLLECTIONNUMBER", "SPECIESSCIENTIFICNAME", "SPECIESCOMMONNAME", "NUMBERTOTAL", "SPECIESTOTALWEIGHT", "EFFORT", "STATIONCODE", "TEMPSURFACE", "TEMPBOTTOM"), c("datetime", "haulid", "spp", "common", "cnt", "wt", "effort", "stratum", "stemp", "btemp"))


# ===========================================
# = Trim to inner strata and non-empty rows =
# ===========================================
sa.catch0 <- sa.catch00[PROJECTNAME!=""&DEPTHZONE=="INNER", list(datetime, spp, common, haulid, stratum, LATITUDESTART, LATITUDEEND, LONGITUDESTART, LONGITUDEEND, stemp, btemp, cnt, wt, effort)]

sa.catch0 <- sa.catch0[effort!=0 | is.na(effort)] # remove rows where the effort was reported as 0 (alternatively, these could be changed to NA, which might still be useful b/c you'd know that a species was observed, just not abundance or biomass)


# ================================
# = Fix lat/lon, add stratumarea =
# ================================
# sa.catch0[LONGITUDESTART < -100 | LONGITUDEEND < -100]
# sa.catch0[,summary(abs(abs(LONGITUDESTART)-abs(LONGITUDEEND)))]
# this was weird, so I took a guess
sa.catch0[LONGITUDESTART > -782.000 & LONGITUDESTART < -780.000, LONGITUDESTART:=-79.0]
sa.catch0[LONGITUDEEND > -782.000 & LONGITUDEEND < -780.000, LONGITUDEEND:=-78.988]
sa.catch0[LONGITUDESTART > -802.000 & LONGITUDESTART < -800.000, LONGITUDESTART:= -81.006+0.01]

# sa.catch0[LATITUDEEND>40]
# sa.catch0[,summary(abs(abs(LATITUDESTART)-abs(LATITUDEEND)))]
sa.catch0[LATITUDEEND>40, LATITUDEEND:=LATITUDESTART+0.009]


sa.catch0[,year:=substr(haulid, 1, 4)]
sa.catch0[,lat:=(LATITUDESTART+LATITUDEEND)/2]
sa.catch0[,lon:=(LONGITUDESTART+LONGITUDEEND)/2]

sa.catch0[,stratumarea:=calcarea(cbind(lon, lat)), by=stratum] # I think this is right ... copied from how malin did gmex


# ================
# = Trim columns =
# ================
sa.catch <- sa.catch0[,list(year, datetime, spp, common, haulid, stratum, stratumarea, lat, lon, stemp, btemp, cnt, wt, effort)]
setkey(sa.catch, haulid)


# ===================================
# = Combine w/ strat data for Depth =
# ===================================
sa.raw0 <- merge(sa.catch, sa.strata, all.x=TRUE)


# ==============
# = Calc Depth =
# ==============
sa.raw0[,depth:=(DEPTHSTART+DEPTHEND)/2]
# sa.raw[,sum(is.na(depth))] # is 0, so don't worry about other cases where only 1 of start/end is not NA


# =============
# = Calc cpue =
# =============
sa.raw0[,cntcpue:=cnt/effort]
sa.raw0[,wtcpue:=wt/effort]


# ================
# = Trim columns =
# ================
# Note: might want to consider saving the common names here, and compare them to those grabbed by taxize
sa.raw0 <- sa.raw0[,list(year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, stemp, btemp, cntcpue, wtcpue)]


# =================
# = Reformat Year =
# =================
sa.raw0[,year:=as.integer(year)]


# ==============
# = Fix Strata =
# ==============
sa.raw <- makeStrat(sa.raw0, regName="sa")


# ==================
# = Trim a bad spp =
# ==================
sa <- sa.raw[spp!="MISCELLANEOUS INVERTEBRATES",]


# =============
# = Aggregate =
# =============
setkey(sa, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
sa2 <- sa[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(sa)]


# ==============
# = Add Region =
# ==============
sa2[,region:="SEAMAP_SA"]
sa2[,s.reg:="sa"]


# ========
# = Save =
# ========
save(sa2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/sa2.RData")


