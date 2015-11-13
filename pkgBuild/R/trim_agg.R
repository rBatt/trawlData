


ebs.trim <- trimData("ebs")

ebs.agg1 <- aggData(
	X = ebs.trim,
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

ebs.agg1[,time_lvl:=NULL]
setnames(ebs.agg1, "nAgg", "nAgg1")
ebs.agg1[,stratum:=ll2strat(lon, lat, gridSize=1)]
ebs.agg1[,aggStrat:=ll2strat(lon, lat, gridSize=0.1)]


ebs.agg2 <- aggData(
	X = ebs.agg1,
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
		datetime=function(x)una(x, na.rm=TRUE)[1],
		season=function(x)una(x, na.rm=TRUE)[1],
		year=function(x)una(x, na.rm=TRUE)[1],
		stratum=function(x)paste(una(x, na.rm=TRUE), collapse=","),
		common=function(x)una(x, na.rm=TRUE)[1],
		species=function(x)una(x, na.rm=TRUE)[1]
	)
)



setnames(ebs.agg2, "nAgg", "nAgg2")
ebs.agg2[,haulid:=paste(aggStrat,time_lvl)] # haulid is defined as something within a 1/10th degree, same month of same year

ebs.agg2[,stratum:=ll2strat(lon, lat, gridSize=1)]

ebs.agg2[,j={
	sll <- t(simplify2array(strsplit(unique(stratum), " ")))
	s.lon <- as.numeric(sll[,1])
	s.lat <- as.numeric(sll[,2])
	plot(s.lon, s.lat, pch=20, col="black", cex=0.5)
}]



# ============
# = Define K =
# ============
# Is region-year-stratum the right definition?
ebs.agg2[,K:=as.integer(as.factor(haulid)), by=c("reg","year","stratum")]
ebs.agg2[,Kmax:=max(K), by=c("reg","year","stratum")]


# ===================
# = Plot K and Kmax =
# ===================
ebs.agg2[,j={
		print(unique(year))
		plot(.SD[,list(Kmax=mean(Kmax, na.rm=T)),keyby="year"], main=unique(reg), type="l")
	}, by="reg"
]

# ebs.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]
# plot((ebs.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]))

(nKmax.reg <- ebs.agg2[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax, reg)])
par(mfrow=c(4,3), mar=c(0.5,0.5,1,0), ps=9)
for(i in 1:ncol(nKmax.reg)){
	plot(nKmax.reg[,i], main=colnames(nKmax.reg)[i], type="h")
}




