all.trim <- trimData()

all.agg1 <- aggData(
	X = all.trim,
	bioFun = sumna,
	envFun = meanna,
	bio_lvl = "spp",
	space_lvl = "haulid",
	time_lvl = "haulid",
	bioCols= c("wtcpue","cntcpue"),
	envCols = c("stemp","btemp","depth","lon","lat"),
	metaCols = c("reg","datetime","season","year","lon","lat","stratum","common","species"),
	meta.action = "unique1"
)

all.agg1[,time_lvl:=NULL]
setnames(all.agg1, "nAgg", "nAgg1")
all.agg1[,stratum:=ll2strat(lon, lat, gridSize=1)]
all.agg1[,aggStrat:=ll2strat(lon, lat, gridSize=0.1)]


all.agg2 <- aggData(
	X = all.agg1,
	bioFun = sumna,
	envFun = meanna,
	bio_lvl = "spp",
	space_lvl = "aggStrat",
	time_lvl = "month",
	bioCols= c("wtcpue","cntcpue"),
	envCols = c("stemp","btemp","depth","lon","lat"),
	metaCols = c("reg","datetime","season","year","lon","lat","stratum","common","species"),
	meta.action="FUN",
	metaFun=list(
		reg=una,
		datetime=function(x)una(x, na.rm=na.rm)[1],
		season=function(x)una(x, na.rm=na.rm)[1],
		year=function(x)una(x, na.rm=na.rm)[1],
		stratum=function(x)paste(una(x, na.rm=na.rm), collapse=","),
		common=function(x)una(x, na.rm=na.rm)[1],
		species=function(x)una(x, na.rm=na.rm)[1]
	)
)



setnames(all.agg2, "nAgg", "nAgg2")
all.agg2[,haulid:=paste(aggStrat,time_lvl)] # haulid is defined as something within a 1/10th degree, same month of same year

all.agg2[,stratum:=ll2strat(lon, lat, gridSize=1)]

all.agg2[,j={
	sll <- t(simplify2array(strsplit(unique(stratum), " ")))
	s.lon <- as.numeric(sll[,1])
	s.lat <- as.numeric(sll[,2])
	plot(s.lon, s.lat, pch=20, col="black", cex=0.5)
}]



# ============
# = Define K =
# ============
# Is region-year-stratum the right definition?
all.agg2[,K:=as.integer(as.factor(haulid)), by=c("reg","year","stratum")]
all.agg2[,Kmax:=max(K), by=c("reg","year","stratum")]


# ===================
# = Plot K and Kmax =
# ===================
all.agg2[,j={
		print(unique(year))
		plot(.SD[,list(Kmax=mean(Kmax, na.rm=T)),keyby="year"], main=unique(reg), type="l",ylim=c(1,8))
	}, by="reg"
]

# all.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]
# plot((all.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]))

(nKmax.reg <- all.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax, reg)])
par(mfrow=c(4,3), mar=c(0.5,0.5,1,0), ps=9)
for(i in 1:ncol(nKmax.reg)){
	plot(nKmax.reg[,i], main=colnames(nKmax.reg)[i], type="h")
}

