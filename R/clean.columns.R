#' Clean Columns
#' 
#' Clean by ensuring the presence of standard columns.
#' 
#' @details
#' Add missing columns if they don't exist. These are standard values like \code{wtcpue}, \code{spp}, \code{datetime}, and \code{reg}. I will make a full list in future versions, when the decisions are finalized.
#' 
#' @template X_reg
#' 
#' @import data.table
#' @export clean.columns
clean.columns <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri")){
	
	reg <- match.arg(reg)
	
	
	clean.columns0 <- function(x){
		switch(x,
			ai = clean.columns.ai(X),
			ebs = clean.columns.ebs(X),
			gmex = clean.columns.gmex(X),
			goa = clean.columns.goa(X),
			neus = clean.columns.neus(X),
			newf = clean.columns.newf(X),
			ngulf = clean.columns.ngulf(X),
			sa = clean.columns.sa(X),
			sgulf = clean.columns.sgulf(X),
			shelf = clean.columns.shelf(X),
			wcann = clean.columns.wcann(X),
			wctri = clean.columns.wctri(X)
		)
	}
	
	clean.columns0(reg)
	
}


makeHaul <- function(X){
	haul.pt <- function(x){
		formatC(x, width=3, flag=0)
	}
	X[,haulid:=paste(haul.pt(vessel), haul.pt(cruise), haul.pt(haul), sep='-')]
	invisible(NULL)
}


# ======
# = AI =
# ======
clean.columns.ai <- function(X){
	
	# haulid
	makeHaul(X)
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(datetime, format="%m/%d/%Y %H:%M", tz="GMT")]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	# not needed
	
	# depth
	# not needed
	
	# stratum
	# not needed
	
	# stratum area
	# not needed
	
	# towarea/ effort
	# missing
	
	# __cpue
	# not needed
	
	# region name
	X[,reg:="ai"]
	
	invisible(NULL)
	
	
}


# =======
# = EBS =
# =======
clean.columns.ebs <- function(X){
	
	# haulid
	makeHaul(X)
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(datetime, format="%m/%d/%Y %H:%M", tz="GMT")]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	# not needed
	
	# depth
	# not needed
	
	# stratum
	# not needed
	
	# stratum area
	# not needed
	
	# towarea/ effort
	# missing
	
	# __cpue
	# not needed
	
	# region name
	X[,reg:="ebs"]
	
	invisible(NULL)
	
}


# ========
# = GMEX =
# ========
clean.columns.gmex <- function(X){
	
	# haulid
	X[,haulid:=paste(formatC(vessel, width=3, flag=0), formatC(cruise, width=3, flag=0), formatC(P_STA_NO, width=5, flag=0, format='d'), sep='-')]
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M", tz="GMT")]
	X[,year:=as.integer(format.Date(datetime, format="%Y"))]
	
	# season
	# just naming from the survey name
	# X[grepl("spring", survey.name, ignore.case=T), season:="spring"]
	# X[grepl("summer", survey.name, ignore.case=T), season:="summer"]
	# X[grepl("fall", survey.name, ignore.case=T), season:="fall"]
	# X[grepl("winter", survey.name, ignore.case=T), season:="winter"]
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]

	
	# lon/ lat
	X[,lat:=rowMeans(cbind(lat.deg.start + lat.min.start/60, lat.deg.end + lat.min.end/60), na.rm=TRUE)] # mean of start and end positions, but allow one to be NA (missing)
	X[,lon:=-rowMeans(cbind(lon.deg.start + lon.min.start/60, lon.deg.end + lon.min.end/60), na.rm=TRUE)] # need negative sign since western hemisphere
	
	# depth
	# not needed
	
	# stratum
	X[,stratum:=paste(floor(lat)+0.5, floor(lon)+0.5, floor(depth/100)*100+50, sep="-")]
	
	# stratum area
	X[!is.na(lon)&!is.na(lat),stratumarea:=suppressMessages(calcarea(cbind(lon, lat))), by=stratum]
	
	# towarea/ effort
	X[,towarea:=(towspeed*1.85200*1E3*towduration/60*gearsize*0.3048)]
	X[,effort:=towarea]
	
	# __cpue
	X[,wtcpue:=weight*1E4/effort]
	X[,cntcpue:=cnt*1E4/effort]
	
	# region name
	X[,reg:="gmex"]
	
	invisible(NULL)
}


# =======
# = GOA =
# =======
clean.columns.goa <- function(X){
	
	# haulid
	makeHaul(X)
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(datetime, format="%m/%d/%Y %H:%M", tz="GMT")]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	# not needed
	
	# depth
	# not needed
	
	# stratum
	# not needed
	
	# stratum area
	# not needed
	
	# towarea/ effort
	# missing
	
	# __cpue
	# not needed
	
	# region name
	X[,reg:="goa"]
	
	invisible(NULL)
}

# ========
# = NEUS =
# ========
clean.columns.neus <- function(X){
	
	# haulid
	X[,haulid:=paste(formatC(cruise, width=6, flag=0), formatC(station, width=3, flag=0), formatC(stratum, width=4, flag=0), sep='-')]
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(paste(as.character(year),substr(cruise,5,6),"01",sep="-"), format="%Y-%m-%d", tz="GMT")]
	
	# season
	# neus already has its own season definition
	# X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	# not needed
	
	# depth
	# not needed
	
	# stratum
	# not needed
	
	# stratum area
	# not needed
	
	# towarea/ effort
	# missing
	
	# __cpue
	# actually missing, but told that
	# the effort is almost constant, so
	# __ is = __cpue
	X[,cntcpue:=cnt]
	X[,wtcpue:=weight]
	
	# region name
	X[,reg:="neus"]
	
	
	invisible(NULL)
}


# ========
# = NEWF =
# ========
clean.columns.newf <- function(X){
	
	# haulid
	X[,haulid:=paste(formatC(vessel, width=2, flag=0), formatC(trip, width=3, flag=0), formatC(set, width=3, flag=0, format='d'), sep='-')]
	
	# # date, time, datetime
	X[,datetime:=as.POSIXct(paste(year, month, day, sep="-"), tz="GMT")]
	X[season=="fall" & !is.na(season) & as.numeric(month)<4, year:=year-1] # this is survey year, not calendar year
	
	# season
	# not needed
	
	# lon/ lat
	X[,lat:=(lat.start+lat.end)/2]
	X[,lon:=(lon.start+lon.end)/2]
	X[lat.end==0,lat:=lat.start]
	X[lon.end==0,lon:=lon.start]
	
	# depth
	# not needed
	
	# stratum
	# not needed
	
	# stratum area
	# missing
	
	# towarea/ effort
	# Malin's method to 
	# getting "effort" (distance)
	# in cases where no distance was
	# recorded
	# fix.area <- function(x){
	# 	bad.d <- X[,towduration==x & !is.na(towduration)]
	# 	bad <- X[,bad.d & (area==0 | is.na(area))]
	# 	val <- X[, mean(area[bad.d&area>0&!is.na(area)&year%in%year[bad]])]
	# 	X[bad, area:=val]
	# 	invisible(NULL)
	# }
	#
	# X[,sum(towduration==15 & !is.na(towduration) & (area==0 | is.na(area)))]
	# fix.area(15)
	#
	# b25 <- X[,towduration==25 & !is.na(towduration) & (area==0 | is.na(area))]
	# fix.area(25)
	#
	# b25 <- X[,towduration==30 & !is.na(towduration) & (area==0 | is.na(area))]
	# fix.area(30)
	
	
	# my method for getting missing towarea
	# and/or distance
	# is regression based, and covers
	# all of the towdurations
	dist.hat <- X[,j={
		dt <- copy(.SD)
		setkey(dt, haulid)
		dt <- unique(dt[towdistance!=0 & is.finite(towdistance) & towduration <=200 & is.finite(towduration)]) #
		setorder(dt, year)
		
		mod <- dt[,lm(towdistance~towduration*I(as.factor(year))-1)]
		
		if(any(!unique(year)%in%dt[,unique(year)])){
			message("crap; missing years")
		}
		
		dist.hat <- predict(mod, .SD[,list(towduration, year=as.factor(year))])
		dist.hat
	}]
	
	need.replaced <- X[,(towdistance==0 | is.na(towdistance)) & (towduration>0 & !is.na(towduration)) & dist.hat>0 & !is.na(dist.hat)] #
	X[need.replaced,towdistance:=dist.hat[need.replaced]]
	
	
	X[,towarea:=towdistance/10 * 1852 * 55.25 * 0.3048]
	X[,effort:=towarea/mean(towarea[!is.na(towarea)&towarea!=0])]

	# __cpue
	X[,wtcpue:=(weight/100)/effort]
	X[,cntcpue:=cnt/effort]
	
	# region name
	X[,reg:="newf"]
	
	
	invisible(NULL)
}

# =========
# = NGULF =
# =========
clean.columns.ngulf <- function(X){
	message("Function not ready yet")
}


# ======
# = SA =
# ======
clean.columns.sa <- function(X){
	
	# haulid
	# not needed
	
	# date, time, datetime
	X[,year:=as.integer(substr(haulid, 1, 4))]
	X[,datetime:=as.POSIXct(paste(date, time), format="%m/%d/%y %H:%M", tz="GMT")]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	X[,lat:=(lat.start+lat.end)/2]
	X[,lon:=(lon.start+lon.end)/2]
	
	# depth
	X[,depth:=rowMeans(cbind(depth.start,depth.end),na.rm=T)]
	
	# stratum
	# not needed
	
	# stratum area
	X[!is.na(lon)&!is.na(lat),stratumarea:=suppressMessages(calcarea(cbind(lon, lat))), by=stratum] 
	
	# towarea/ effort
	# not needed
	
	# __cpue
	X[,cntcpue:=cnt/effort]
	X[,wtcpue:=weight/effort]
	
	# region name
	X[,reg:="sa"]
	
	
	invisible(NULL)
}

# =========
# = SGULF =
# =========
clean.columns.sgulf <- function(X){
	
	
	# haulid
	X[,haulid:=paste(vessel, cruise, set, sep="-")]
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(paste(paste(year, month, day, sep="-"), time), format="%Y-%m-%d %H:%M", tz="GMT")]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	# not needed
	
	# depth
	# ...
	
	# stratum
	# not needed
	
	# stratum area
	# not needed
	
	# towarea/ effort
	# not needed
	
	# __cpue
	# not needed
	
	# region name
	X[,reg:="sgulf"]
	
	
	invisible(NULL)
}

# =========
# = SHELF =
# =========
clean.columns.shelf <- function(X){
	
	# haulid
	X[,haulid:=paste(MISSION, formatC(SETNO, width=3, flag=0))]
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(paste(date, time), tz="GMT")]
	X[,year:=as.numeric(format.Date(datetime, format="%Y"))]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	X[,lat:=lat.start]
	X[,lon:=lon.start]
	
	# depth
	invisible(X[is.na(depth), depth:=rowMeans(cbind(depth.min+depth.max),na.rm=TRUE)])
	invisible(X[is.na(depth)&is.na(depth.min)&!is.na(depth.max), depth:=depth.max])
	invisible(X[is.na(depth)&is.na(depth.max)&!is.na(depth.min), depth:=depth.min])
	
	# stratum
	# not needed
	
	# stratum area
	# not needed
	
	# towarea/ effort
	# not needed
	
	# __cpue
	# not needed
	
	# region name
	X[,reg:="shelf"]
	
	invisible(NULL)
}

# ==========
# = WC ANN =
# ==========
clean.columns.wcann <- function(X){
	
	# haulid
	# not needed
	
	# date, time, datetime
	X[,datetime:=as.POSIXct(datetime, format="%m/%d/%y %H:%M", tz="GMT")]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	# not needed
	
	# depth
	# not needed
	
	# stratum
	X[,stratum:=paste(floor(lat)+0.5, floor(lon/100)*100+50, sep="-")]
	
	# stratum area
	X[!is.na(lon)&!is.na(lat),stratumarea:=suppressMessages(calcarea(cbind(lon, lat))), by=stratum]
	
	# towarea/ effort
	X[,effort:=towarea]
	
	# __cpue
	X[,wtcpue:=weight/effort]
	X[,cnt:=(weight/Individual.Average.Weight..kg.)]
	X[,cntcpue:=cnt/effort]
	
	# region name
	X[,reg:="wcann"]
	
	
	invisible(NULL)
}

# ==========
# = WC TRI =
# ==========
clean.columns.wctri <- function(X){
	
	# haulid
	X[,haulid:=paste(formatC(vessel, width=3, flag=0), formatC(cruise, width=3, flag=0), formatC(haul, width=3, flag=0), sep='-')]
	
	# date, time, datetime
	X[,year:=as.numeric(substr(cruise, 1, 4))]
	X[,datetime:=as.POSIXct(datetime, format="%m/%d/%Y %H:%M:%S", tz="GMT")]
	
	# season
	X[!is.na(datetime),season:=getSeason(unique(datetime)),by="datetime"]
	
	# lon/ lat
	X[,lat:=rowMeans(cbind(lat.start,lat.end),na.rm=TRUE)]
	X[,lon:=rowMeans(cbind(lon.start,lon.end),na.rm=TRUE)]
	
	# depth
	# not needed
	
	# stratum
	X[,stratum:=paste(floor(lat)+0.5, floor(depth/100)*100+50, sep="-")]
	
	# stratum area
	X[!is.na(lon)&!is.na(lat),stratumarea:=suppressMessages(calcarea(cbind(lon, lat))), by=stratum]
	
	# towarea/ effort
	X[,towarea:=towdistance*1E3*gearsize/1E4]
	X[,effort:=towarea]
	
	# __cpue
	X[,wtcpue:=weight/effort]
	X[,cntcpue:=cnt/effort]
	
	# region name
	X[,reg:="wctri"]
	
	
	invisible(NULL)
}

