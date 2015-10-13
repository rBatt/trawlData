

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


# ====================
# = Read in Raw Data =
# ====================
# read in main ai data
ai1.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/ai1983_2000.csv")
setnames(ai1.raw, names(ai1.raw), gsub("^\\s* | \\s*$", "", names(ai1.raw)))

ai2.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/ai2002_2012.csv")
setnames(ai2.raw, names(ai2.raw), gsub("^\\s* | \\s*$", "", names(ai2.raw)))

ai.raw <- rbind(ai1.raw, ai2.raw)

setkey(ai.raw, STRATUM)

# read in ai strata
aiStrata <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/aiStrata.csv", select=c("StratumCode", "Areakm2"))
setnames(aiStrata, names(aiStrata), gsub("^\\s* | \\s*$", "", names(aiStrata)))
setnames(aiStrata, "StratumCode", "STRATUM")
setkey(aiStrata, STRATUM)

# merge ai with strata
ai <- merge(ai.raw, aiStrata, all.x=TRUE)

# ====================
# = Clean up ai data =
# ====================
rmWhite(ai) # remove whitespace in the elements of each column
rm9s(ai) # check each column for 9999, and replace with NA


# ==================
# = Add ai haul ID =
# ==================

ai[,haulid:=paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE, width=3, flag=0), formatC(HAUL, width=3, flag=0), sep='-')]

# =============
# = Fix names =
# =============
setnames(ai, c("STRATUM", "YEAR", "LATITUDE", "LONGITUDE", "BOT_DEPTH", "SCIENTIFIC", "WTCPUE", "Areakm2", "BOT_TEMP", "SURF_TEMP", "DATETIME"), c("stratum", "year", "lat", "lon", "depth", "spp", "wtcpue", "stratumarea", "btemp", "stemp", "datetime"))


# ================
# = Fix Lon/ Lat =
# ================
# Calculate a corrected longitude for Aleutians (all in western hemisphere coordinates)
ai$lon[ai$lon>0] <- ai$lon[ai$lon>0] - 360

# ==============
# = Fix Strata =
# ==============
ai <- makeStrat(ai, regName="ai")


# ==================
# = Remove bad spp =
# ==================

setkey(ai, spp)
ai.spp.bad <- c("","Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")
ai <- ai[!.(ai.spp.bad)]



# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================

i <- sapply(ai, is.factor)
if(any(i)){
	ai[i] <- lapply(ai[i], as.character)
}


ai[.(c('Atheresthesevermanni', 'Atheresthesstomias')), spp:='Atheresthessp.']; setkey(ai, spp)
ai[.(c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')), spp:='Lepidopsettasp.']; setkey(ai, spp)
ai[.(c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')), spp:='Myoxocephalussp.']; setkey(ai, spp)
ai[.(c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajainterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')), spp:='Bathyrajasp.']; setkey(ai, spp)


# =============
# = Aggregate =
# =============
# ai[, rLat:=roundGrid(lat)]
# ai[, rLon:=roundGrid(lon)]
# ai[, K:=as.integer(as.factor(haulid)), by=c("year", "rLat", "rLon")]

# ai[,length(unique(haulid))] == ai[,length(unique(paste(year, rLat, rLon, K)))] # in this case, these should match

# setkey(ai, year, datetime, spp, haulid, stratum, K, stratumarea, lat, lon, depth)
setkey(ai, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
ai2 <- ai[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=meanna), by=key(ai)] # 1 duplicate entry




# ==============
# = Add region =
# ==============
ai2[,region:="AFSC_Aleutians"]
ai2[,s.reg:="ai"]


# ========
# = Save =
# ========
save(ai2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/ai2.RData")

# ai2[,length(unique(stratum))] # 42
# ai2[,length(unique(year))] # 11
# ai2[,length(unique(datetime))] # 4154
# ai2[,length(unique(haulid))] # 4217
#
# dev.new(height=8.5, width=7)
# par(mfrow=c(4,3), mar=c(1.5,1.5,0.5,0.5), ps=8, cex=1, mgp=c(1,0.15,0), tcl=-0.15)
# ai2[,K:=length(unique((datetime))), by=c("year","stratum")]
# maxK <- ai2[,max(K)]
#
# ai2[,plot(table(lu(haulid),stratum)), by="year"]
#
# test <- ai2[,unique(datetime)[1:4]]
#
# ai2[,
# 	j={
# 		plot(table(.SD[,list(K=max(K)),by=c("stratum")][,K]),xlab="", ylab="", xlim=c(0,maxK))
#
# 	},
# 	by="year"
# ]
#
#
# heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(maxK)
# ai3 <- ai2
# setkey(ai3, stratum, year)
# ai3 <- unique(ai3)
# xl <- ai3[,range(roundGrid(lon))]
# yl <- ai3[,range(roundGrid(lat))]
# ai3[,
# 	j={
# 		# plot(roundGrid(lon),roundGrid(lat), bg=heat.cols[K], pch=21, xlim=xl, ylim=yl, xlab="", ylab="")
# 		# legend("top", legend=unique(year))
# 		plot(table())
# 	}
#
# 	,by="year"
# ]
#
