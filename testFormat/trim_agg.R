library(rbLib)


ebs.t <- trawlTrim("ebs")

# aggregate to make sure a haul
# doesn't have duplicates of individuals
# this aggregation uses the biological sum
ebs.agg1 <- trawlAgg(
	X = ebs.t,
	bioFun = sumna,
	envFun = meanna,
	bio_lvl = "spp",
	space_lvl = "haulid",
	time_lvl = "haulid",
	bioCols= c("wtcpue"),
	envCols = c("stemp","btemp","depth","lon","lat"),
	metaCols = c("reg","datetime","season","year","lon","lat","stratum","common","species"),
	meta.action = "unique1"
)

# drop new "time" column
# change name of agg to indicate step
# add a 1 and 1/10 degree grid
ebs.agg1[,time_lvl:=NULL]
setnames(ebs.agg1, "nAgg", "nAgg1")
ebs.agg1[,stratum:=ll2strat(lon, lat, gridSize=1)]
ebs.agg1[,aggStrat:=ll2strat(lon, lat, gridSize=0.1)]

# ========================
# = Give Abundance Value =
# ========================
ebs.agg1[,abund:=as.integer(wtcpue>0)]


# ==========================================================
# = Aggregate to level where detection process is constant =
# ==========================================================
# Aggregate at a 1/10 degree grid and
# the same month (for each species)
# This aggregation uses the biosum, too
# Note, consideration needs to be given here, 
# I keep flipping around on how I use this
ebs.agg2 <- trawlAgg(
	X = ebs.agg1,
	bioFun = sumna,
	envFun = meanna,
	bio_lvl = "spp",
	space_lvl = "aggStrat",
	time_lvl = "month",
	bioCols= c("wtcpue","abund"),
	envCols = c("stemp","btemp","depth","lon","lat"),
	metaCols = c("reg","datetime","season","year","lon","lat","stratum","common","species"),
	meta.action="FUN",
	metaFun=list(
		reg=una,
		datetime=function(x)una(x, na.rm=TRUE)[1],
		season=function(x)una(x, na.rm=TRUE)[1],
		year=function(x)una(x, na.rm=TRUE)[1],
		stratum=function(x)paste(una(x, na.rm=TRUE), collapse=","),
		common=function(x)una(x, na.rm=TRUE)[1],
		species=function(x)una(x, na.rm=TRUE)[1]
	)
)


# rename agg to indicate step
# make a new haulid (combo of most specific time and space)
# now haul id is day-year-(1/10 degree)
setnames(ebs.agg2, "nAgg", "nAgg2")
ebs.agg2[,haulid:=paste(aggStrat,time_lvl)]

ebs.agg2[,j={
	sll <- t(simplify2array(strsplit(unique(stratum), " ")))
	s.lon <- as.numeric(sll[,1])
	s.lat <- as.numeric(sll[,2])
	plot(s.lon, s.lat, pch=20, col="black", cex=0.5)
}]


# ============
# = Define K =
# ============
# K is the number of haulid values in a reg-year-stratum
# potential for K>1 because:
#  finest temporal level is day
#  finest spatial level is 1/10 degree (stratum is 1 degree)
# Using "reg" is probably redundant with stratum in this case
ebs.agg2[,K:=as.integer(as.factor(haulid)), by=c("reg","year","stratum")]
ebs.agg2[,Kmax:=max(K), by=c("reg","year","stratum")]


# =============================================
# = Make Temperature Constant within a haulid =
# =============================================
# ebs.agg2[,lu(btemp),by=c("haulid")][,(V1)]


# ===================
# = Plot K and Kmax =
# ===================
par(mfrow=auto.mfrow(ebs.agg2[,lu(reg)]))
ebs.agg2[,j={
		print(unique(year))
		plot(.SD[,list(Kmax=mean(Kmax, na.rm=T)),keyby="year"], main=unique(reg), type="l")
	}, by="reg"
]

# ebs.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]
# plot((ebs.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]))

(nKmax.reg <- ebs.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax, reg)])
par(mfrow=auto.mfrow(ebs.agg2[,lu(reg)]), mar=c(0.5,0.5,1,0), ps=9)
for(i in 1:ncol(nKmax.reg)){
	plot(nKmax.reg[,i], main=colnames(nKmax.reg)[i], type="h")
}


# ========================================
# = Convert to Mini Data Set for Testing =
# ========================================
set.seed(1337)
ind <- mpick(ebs.agg2, p=c(spp=5, stratum=3, year=1), weight=TRUE, limit=60)
logic <- expression(
	spp%in%spp[ind]
	& stratum%in%stratum[ind]
	& as.integer(year)%in%(as.integer(unique(year[ind])) + (-1:1))
)
ebs.a <- ebs.agg2[eval(logic)]


# ============================
# = Save for Trawl Diversity =
# ============================
save(ebs.agg2, file="trawl/trawlDiversity/data/ebs.agg2.RData")
save(ebs.a, file="trawl/trawlDiversity/data/ebs.a.RData")



# ========
# = Cast =
# ========
ebs.c <- trawlCast(ebs.a, stratum~K~spp~year, valueName="abund")

ebs.btemp.c <- trawlCast(ebs.a, 
	stratum~K~year, 
	valueName="btemp", 
	fixAbsent=FALSE, 
	fun.aggregate=meanna, 
	valFill=NA_real_, 
	grandNamesOut=c("stratum","K","year")
)

ebs.stemp.c <- trawlCast(ebs.a, 
	stratum~K~year, 
	valueName="stemp", 
	fixAbsent=FALSE, 
	fun.aggregate=meanna, 
	valFill=NA_real_, 
	grandNamesOut=c("stratum","K","year")
)

ebs.depth.c <- trawlCast(ebs.a, 
	stratum~K~year, 
	valueName="depth", 
	fixAbsent=FALSE, 
	fun.aggregate=meanna, 
	valFill=NA_real_, 
	grandNamesOut=c("stratum","K","year")
)


ebs.effort.c <- trawlCast(ebs.a, 
	stratum~K~year, 
	valueName="nAgg2", 
	fixAbsent=FALSE, 
	fun.aggregate=meanna, 
	valFill=NA_real_, 
	grandNamesOut=c("stratum","K","year")
)
