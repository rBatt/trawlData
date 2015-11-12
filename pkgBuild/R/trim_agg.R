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
all.agg1[,stratum:=ll2strat(lon, lat, gridSize=0.5)]


all.agg1[,j={
	sll <- t(simplify2array(strsplit(unique(stratum), " ")))
	s.lon <- as.numeric(sll[,1])
	s.lat <- as.numeric(sll[,2])
	plot(s.lon, s.lat, pch=20, col=as.factor(season), cex=0.25)
}]

all.agg1[,K:=as.integer(as.factor(haulid)), by=c("reg","year","stratum")]
all.agg1[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]
plot(cumsum(all.agg1[,list(Kmax=unique(Kmax)), by=c("reg","year","stratum")][,table(Kmax)]))
all.agg1[,j={
		print(unique(year))
		plot(.SD[,list(Kmax=mean(Kmax, na.rm=T)),keyby="year"], main=unique(reg), type="l",ylim=c(1,8))
	}, by="reg"
]

all.agg2 <- aggData(
	X = all.agg1,
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