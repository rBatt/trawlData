


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


# ====================
# = Read in raw data =
# ====================

gmex.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/SEAMAP-GMex/2014-06-25/"

gmex.bio000 <- fread(paste(gmex.start, "BGSREC_ryan.csv", sep=""), sep=",", colClasses=c(rep("integer",7), rep("character",3), rep("integer",2), "numeric", "numeric", "character", rep("character",3)), drop=c("SAMPLE_BGS", "NODC_BGS", "IS_SAMPLE", "TAXONID", "CNT"))
rmWhite(gmex.bio000)

gmex.sta <- fread(paste(gmex.start, "STAREC_ryan.csv", sep=""), select=c('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR', "TEMP_SSURF", "TEMP_BOT", 'VESSEL_SPD', 'COMSTAT'))
rmWhite(gmex.sta)

gmex.tow <- fread(paste(gmex.start, "INVREC_ryan.csv", sep=""), colClasses=c(rep("integer",7), "character", "numeric","character", "integer", rep("character",3), rep("numeric", 12)), select=c('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP'))
rmWhite(gmex.tow)

gmex.spp0 <- fread(paste(gmex.start, "NEWBIOCODESBIG_ryan.csv", sep=""), colClasses=c("integer", "character", "character", "integer", "integer", "character", "character"))
rmWhite(gmex.spp0)

gmex.cruises <- fread(paste(gmex.start, "CRUISES_ryan.csv", sep=""), select=c("CRUISEID", "VESSEL", "TITLE"))
rmWhite(gmex.cruises)

# gmex.glf <- fread(paste(gmex.start, "GLFREC_ryan.csv", sep="")) # has length data

# ================================
# = Subset and clean up gmex.bio =
# ================================
# setkey(gmex.bio00, BGSCODE, GENUS_BGS)
gmex.bio00 <- gmex.bio000[BGSCODE!="T" & GENUS_BGS!="UNKNOWN"] # this is actually faster than using a key
# gmex.bio0 <- gmex.bio00[!"T"] # [!.(unique(BGSCODE), "UNKNOWN")] note that I can't find any of the unknown in there
# dim(gmex.bio00[!"T"] [!.(unique(BGSCODE), "UNKNOWN")])

setkey(gmex.bio00, CRUISEID, STATIONID, VESSEL, CRUISE_NO, P_STA_NO, GENUS_BGS, SPEC_BGS, BGSCODE, BIO_BGS, SELECT_BGS)
gmex.bio0 <- unique(gmex.bio00)
gmex.bio0 <- setkey(gmex.bio0, NULL)

# ===================
# = Fix up gmex.spp =
# ===================
bad.gmex.CODE <- names(gmex.spp0[,table(CODE)][gmex.spp0[,table(CODE)] > 1])
good.gmex.CODE <- names(gmex.spp0[,table(CODE)][gmex.spp0[,table(CODE)] <= 1])

setkey(gmex.spp0, CODE)
gmex.spp <- gmex.spp0[good.gmex.CODE]


newspp <- data.table(Key1 = c(503L,5770L), TAXONOMIC = c('ANTHIAS TENUIS AND WOODSI', 'MOLLUSCA AND UNID.OTHER #01'), CODE=bad.gmex.CODE, TAXONSIZECODE=as.integer(NA), isactive=-1L, common_name=c('threadnose and swallowtail bass', 'molluscs or unknown'), tsn=as.character(NA), key="CODE") # redefine the many-species codes

gmex.spp <- rbind(gmex.spp, newspp, use.names=TRUE) # add the redefined codes back to gmex.spp

gmex.spp <- gmex.spp[,list(CODE, TAXONOMIC)]

# ===================
# = Subset gmex.tow =
# ===================
gmex.tow <- gmex.tow[GEAR_TYPE=="ST"]

# =============
# = Form gmex =
# =============
gmex0000 <- merge(gmex.bio0, gmex.tow, by=intersect(names(gmex.bio0), names(gmex.tow)), all.x=TRUE)

gmex000 <- merge(gmex0000, gmex.sta, by=intersect(names(gmex0000), names(gmex.sta)), all.x=TRUE)

setkey(gmex000, BIO_BGS)
setnames(gmex.spp, "CODE", "BIO_BGS")
setkey(gmex.spp, BIO_BGS)
gmex00 <- merge(gmex000, gmex.spp[,list(BIO_BGS, TAXONOMIC)], all.x=TRUE) # SOURCE OF THE PROBLEM, LINE 112 & LINE 117 MALIN'S CODE
# gmex00[,unique(BIO_BGS)][!gmex00[,unique(BIO_BGS)]%in%gmex.spp[,unique(BIO_BGS)]] # species (BIO_BGS) in gmex00 that are not in gmex.spp


gmex0 <- merge(gmex00, gmex.cruises, by=intersect(names(gmex00), names(gmex.cruises)), all.x=TRUE)

gmex <- gmex0[TITLE %in% c('Summer SEAMAP Groundfish Survey', 'Summer SEAMAP Groundfish Suvey') & GEAR_SIZE==40 & MESH_SIZE == 1.63 & !is.na(MESH_SIZE) & OP %in% c(''),] # # Trim to high quality SEAMAP summer trawls, based off the subset used by Jeff Rester's GS_TRAWL_05232011.sas

gmex[,haulid:=paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE_NO, width=3, flag=0), formatC(P_STA_NO, width=5, flag=0, format='d'), sep='-')]

# ============
# = Add year =
# ============
gmex[,year:=as.numeric(unlist(strsplit(as.character(MO_DAY_YR), split='-'))[seq(1,by=3,length=nrow(gmex))])]



# ===========================================================
# = Calculate decimal lat and lon, depth in m, where needed =
# ===========================================================
gmex$S_LATD[gmex$S_LATD == 0] <- NA
gmex$S_LOND[gmex$S_LOND == 0] <- NA
gmex$E_LATD[gmex$E_LATD == 0] <- NA
gmex$E_LOND[gmex$E_LOND == 0] <- NA
gmex$lat <- rowMeans(cbind(gmex$S_LATD + gmex$S_LATM/60, gmex$E_LATD + gmex$E_LATM/60), na.rm=TRUE) # mean of start and end positions, but allow one to be NA (missing)
gmex$lon <- -rowMeans(cbind(gmex$S_LOND + gmex$S_LONM/60, gmex$E_LOND + gmex$E_LONM/60), na.rm=TRUE) # need negative sign since western hemisphere
gmex$depth <- gmex$DEPTH_SSTA*1.8288 # convert fathoms to meters


# ===============================
# = Trim years (malin line 173) =
# ===============================
# gmex <- gmex[!(gmex$year %in% c(1982, 1983)),] # 1982 and 1983 didn't sample many strata


# ===========================
# = Add strata where needed =
# ===========================
gmex[,stratum:=paste(floor(lat)+0.5, floor(lon)+0.5, floor(depth/100)*100+50, sep="-")]


# ==============
# = Fix Strata =
# ==============
gmex <- makeStrat(gmex, regName="gmex")



# =================================================
# = Trim/ fix speed and duration (malin line 176) =
# =================================================
gmex <- gmex[gmex$MIN_FISH<=60 & gmex$MIN_FISH > 0 & !is.na(gmex$MIN_FISH),]
gmex$VESSEL_SPD[gmex$VESSEL_SPD==30] <- 3 # fix typo according to Jeff Rester: 30 = 3
gmex <- gmex[gmex$VESSEL_SPD <= 5 & gmex$VESSEL_SPD > 0  & !is.na(gmex$VESSEL_SPD),] # trim out vessel speeds 0, unknown, or >5 (need vessel speed to calculate area trawled)


# ===============================================
# = Calculate stratum area (malin line 189-190) =
# ===============================================
gmex[,stratumarea:=calcarea(cbind(lon, lat)), by=stratum]


# =============
# = Fix names =
# =============
setnames(gmex, c("TAXONOMIC", "TEMP_SSURF", "TEMP_BOT", "MO_DAY_YR"), c("spp", "stemp", "btemp", "datetime"))


# ============
# = Get CPUE =
# ============
gmex[,wtcpue:=SELECT_BGS*1E4/(VESSEL_SPD*1.85200*1E3*MIN_FISH/60*GEAR_SIZE*0.3048)]
gmex[,cntcpue:=CNTEXP*1E4/(VESSEL_SPD*1.85200*1E3*MIN_FISH/60*GEAR_SIZE*0.3048)]



# ==========================================
# = Remove duplicate tows (malin line 252) =
# ==========================================
# dups <- which(duplicated(as.data.frame(gmex)[,c('year', 'lat', 'lon')]) & !duplicated(gmex$haulid)) # identify duplicate tows at same year/lat/lon
# duplicated(as.data.frame(gmex)[,c('year', 'lat', 'lon')]) # data.frame style, slow
# duplicated(gmex, by=c("year", "lat", "lon")) # this is a faster version of the above
setkey(gmex, year, lat, lon)
dups <- duplicated(gmex, by=c("year", "lat", "lon")) & !duplicated(gmex, by="haulid")
dupped <- gmex[gmex[dups, list(year, lat, lon)],]
badhaul <- dupped[,unique(haulid[grep("PORT", COMSTAT)])] # malin line 255, after %in%

setkey(gmex, haulid)
gmex <- gmex[!.(badhaul)]


# ==================
# = Remove bad spp =
# ==================
setkey(gmex, spp)
gmex <- gmex[!is.na(spp)]
gmex.spp.bad <- c("",'UNID CRUSTA', 'UNID OTHER', 'UNID.FISH', 'CRUSTACEA(INFRAORDER) BRACHYURA', 'MOLLUSCA AND UNID.OTHER #01', 'ALGAE', 'MISCELLANEOUS INVERTEBR', 'OTHER INVERTEBRATES')
gmex <- gmex[!.(gmex.spp.bad)]


# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================

i <- sapply(gmex, is.factor)
if(any(i)){
	gmex[i] <- lapply(gmex[i], as.character)
}




i = gmex$GENUS_BGS == 'PELAGIA' & gmex$SPEC_BGS == 'NOCTUL'; gmex$spp[i] = 'PELAGIA NOCTILUCA'; gmex$BIO_BGS[i] = 618030201
i = gmex$GENUS_BGS == 'MURICAN' & gmex$SPEC_BGS == 'FULVEN'; gmex$spp[i] = 'MURICANTHUS FULVESCENS'; gmex$BIO_BGS[i] = 308011501
setkey(gmex, spp) # gotta put the key back where you found it!

gmex[.(c('APLYSIA BRASILIANA', 'APLYSIA WILLCOXI')), spp:='APLYSIA']; setkey(gmex, spp)
gmex[.(c('AURELIA AURITA')), spp:='AURELIA']; setkey(gmex, spp)
gmex[.(c('BOTHUS LUNATUS', 'BOTHUS OCELLATUS', 'BOTHUS ROBINSI')), spp:='BOTHUS']; setkey(gmex, spp)
gmex[.(c('CLYPEASTER PROSTRATUS', 'CLYPEASTER RAVENELII')), spp:='CLYPEASTER']; setkey(gmex, spp)
gmex[.(c('CONUS AUSTINI', 'CONUS STIMPSONI')), spp:='CONUS']; setkey(gmex, spp)
gmex[.(c('CYNOSCION ARENARIUS', 'CYNOSCION NEBULOSUS', 'CYNOSCION NOTHUS')), spp:='CYNOSCION']; setkey(gmex, spp)
gmex[.(c('ECHINASTER SENTUS', 'ECHINASTER SERPENTARIUS')), spp:='ECHINASTER']; setkey(gmex, spp)
gmex[.(c('ECHINASTER SENTUS', 'ECHINASTER SERPENTARIUS')), spp:='ECHINASTER']; setkey(gmex, spp)
gmex[.(c('OPISTOGNATHUS AURIFRONS', 'OPISTOGNATHUS LONCHURUS')), spp:='OPISTOGNATHUS']; setkey(gmex, spp)
gmex[.(c('OPSANUS BETA', 'OPSANUS PARDUS', 'OPSANUS TAU')), spp:='OPSANUS']; setkey(gmex, spp)
gmex[.(c('ROSSIA BULLISI')), spp:='ROSSIA']; setkey(gmex, spp)
gmex[.(c('SOLENOCERA ATLANTIDIS', 'SOLENOCERA NECOPINA', 'SOLENOCERA VIOSCAI')), spp:='SOLENOCERA']; setkey(gmex, spp)
gmex[.(c('TRACHYPENEUS CONSTRICTUS', 'TRACHYPENEUS SIMILIS')), spp:='TRACHYPENEUS']; setkey(gmex, spp)


# =============
# = Aggregate =
# =============
# setkey(gmex, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# gmex2 <- gmex[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=sumna), by=key(gmex)]
# gmex2 <- gmex[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(gmex)] # I think cpue should be avgd

# gmex[, rLat:=roundGrid(lat, 1)]
# gmex[, rLon:=roundGrid(lon, 1)]
#
# # Create gmex K by first aggregating by datetime
# # for some stratum-year combinations, many unique hauls made in same stratum (e.g., 56)
# gmexK <- gmex
# setkey(gmexK, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
# gmexK <- gmeK[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(gmexK)]
#
#
#
# gmex[, K:=as.integer(as.factor(datetime)), by=c("year", "rLat", "rLon")]
# gmex[, K2:=as.integer(as.factor(haulid)), by=c("year", "rLat", "rLon", "datetime")]
#
# gmex[stratum=="29.5--88.5-50 -88.5 29.5"&year=="1985"]
# gmex[stratum=="29.5--88.5-50 -88.5 29.5"&year=="1985"&datetime=="1985-07-17"]
#
# # gmex[,length(unique(haulid))] == gmex[,length(unique(paste(year, rLat, rLon, K)))]
# gmex[,plot(table(.SD[,list(K=max(K)),by=c("year","rLat","rLon")][,K]))]
# gmex[,plot(table(.SD[,list(K=max(K2)),by=c("year","rLat","rLon")][,K]))]

setkey(gmex, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
gmex2 <- gmex[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(gmex)]


# ==============
# = Add region =
# ==============
gmex2[,region:="SEFSC_GOMex"]
gmex2[,s.reg:="gmex"]


# ========
# = Save =
# ========
save(gmex2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/gmex2.RData")

# gmex3 <- gmex2
# setkey(gmex3, year, spp, datetime, stratum)
# gmex3 <- gmex3[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue, rLat=roundGrid(lat), rLon=roundGrid(lon), depth=depth), FUN=meanna), by=key(gmex3)]
#
# gmex3[, rLat:=roundGrid(lat, 1)]
# gmex3[, rLon:=roundGrid(lon, 1)]
# gmex3[, K:=as.integer(as.factor(datetime)), by=c("year", "rLat", "rLon")]
# gmex3[, K:=as.integer(as.factor(datetime)), by=c("year", "stratum")]
# gmex3[,plot(table(.SD[,list(K=max(K)),by=c("year","rLat","rLon")][,K]))]