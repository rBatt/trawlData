clean.names <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri")){
	
	reg <- match.arg(reg)
	
	
	get.clean.names <- function(x){
		switch(x,
			ai = clean.names.ai(X),
			ebs = clean.names.ebs(X),
			gmex = clean.names.gmex(X),
			goa = clean.names.goa(X),
			neus = clean.names.neus(X),
			newf = clean.names.newf(X),
			ngulf = clean.names.ngulf(X),
			sa = clean.names.sa(X),
			sgulf = clean.names.sgulf(X),
			shelf = clean.names.shelf(X),
			wcann = clean.names.wcann(X),
			wctri = clean.names.wctri(X)
		)
	}
	
	# remove leading/ trailing whitespace in col names
	setnames(X, names(X), gsub("^\\s* | \\s*$", "", names(X)))
	
	# make valid names
	# note this is going to cause problems 
	# because I rely on the (kinda) OS-specific conversion I'm used to
	# however, only for WC regions, and probably same on most OS's
	# probably no big deal, but look out!
	# see ?make.names, read whole thing
	setnames(X, names(X), make.names(names(X)))
	
	cn <- get.clean.names(reg) # get the key to convert column names
	cn.found <- cn$old%in%names(X) # find out which column names the key has
	setnames(X, old=cn$old[cn.found], new=cn$new[cn.found]) # make recognized corrections
	
}




# ======
# = AI =
# ======
clean.names.ai <- function(X){
	
	ai.cols <- c(
		"STRATUM" = "stratum",
		"LATITUDE" = "lat",
		"LONGITUDE" = "lon",
		"STATION" = "station",
		"YEAR" = "year",
		"DATETIME" = "datetime",
		"WTCPUE" = "wtcpue",
		"NUMCPUE" = "cntcpue",
		"COMMON" = "common",
		"SCIENTIFIC" = "spp",
		"SID" = "SID",
		"BOT_DEPTH" = "depth",
		"BOT_TEMP" = "btemp",
		"SURF_TEMP" = "stemp",
		"VESSEL" = "vessel",
		"CRUISE" = "cruise",
		"HAUL" = "haul",
		"Areakm2" = "stratumarea"
	
	)
	
	return(list(old=names(ai.cols), new=unname(ai.cols)))
	

}


# =======
# = EBS =
# =======
clean.names.ebs <- function(X){

	ebs.cols <- c(
		"STRATUM" = "stratum",
		"LATITUDE" = "lat",
		"LONGITUDE" = "lon",
		"STATION" = "station",
		"YEAR" = "year",
		"DATETIME" = "datetime",
		"WTCPUE" = "wtcpue",
		"NUMCPUE" = "cntcpue",
		"COMMON" = "common",
		"SCIENTIFIC" = "spp",
		"SID" = "SID",
		"BOT_DEPTH" = "depth",
		"BOT_TEMP" = "btemp",
		"SURF_TEMP" = "stemp",
		"VESSEL" = "vessel",
		"CRUISE" = "cruise",
		"HAUL" = "haul",
		"Areakm2" = "stratumarea"
	
	)
	
	return(list(old=names(ebs.cols), new=unname(ebs.cols)))

}


# ========
# = GMEX =
# ========
clean.names.gmex <- function(X){
	gmex.cols <- c(
		"CRUISEID" = "cruise",
		"VESSEL" = "vessel",
		"BIO_BGS" = "SID",
		"STATIONID" = "station",
		# "CRUISE_NO" = "cruise", # don't know how this is different from CRUISEID
		# "P_STA_NO" = "station", # don't know how it's different from stationid
		"BGSID" = "BGSID",
		"CATEGORY" = "unknown",
	
		"GENUS_BGS" = "genus",
		"SPEC_BGS" = "species",
		"BGSCODE" = "BGSCODE",
	
		"CNTEXP" = "cnt",
		"SELECT_BGS" = "weight",
	
		"INVRECID" = "towID",
	
		"GEAR_SIZE" = "gearsize",
		"GEAR_TYPE" = "geartype",
		"MESH_SIZE" = "meshsize",
		# "OP" = "status of the tow; blank is good, so is a '.'; all else bad (see filedef.doc)",
		"MIN_FISH" = "towduration",
	
		"TIME_ZN"="timezone",
		"TIME_MIL"="time",
	
	
		"S_LATD"= "lat.deg.start", #paste("start", "lat.deg",sep="."),
		"S_LATM"= "lat.min.start", #paste("start", "lat.min",sep="."),
		"S_LOND"= "lon.deg.start", # paste("start", "lon.deg",sep="."),
		"S_LONM"= "lon.min.start", # paste("start", "lon.min",sep="."),
	
		"DEPTH_SSTA" = "depth", # paste("start", "depth2",sep="."),
		"MO_DAY_YR" = "date",
	
		"E_LATD"= "lat.deg.end", # paste("end", "lat.deg",sep="."),
		"E_LATM"= "lat.min.end", # paste("end", "lat.min",sep="."),
		"E_LOND"= "lon.deg.end", # paste("end", "lon.deg",sep="."),
		"E_LONM"= "lon.min.end", # paste("end", "lon.min",sep="."),
	
	
		"TEMP_SSURF" = "stemp",
		"TEMP_BOT" = "btemp",
	
		"VESSEL_SPD"="towspeed",
		"COMSTAT"= "station.comment",
		"TAXONOMIC" = "spp",
		"TITLE" = "survey.name"
	)
	
	return(list(old=names(gmex.cols), new=unname(gmex.cols)))
	
}


# =======
# = GOA =
# =======
clean.names.goa <- function(X){
	
	goa.cols <- c(
		"STRATUM" = "stratum",
		"LATITUDE" = "lat",
		"LONGITUDE" = "lon",
		"STATION" = "station",
		"YEAR" = "year",
		"DATETIME" = "datetime",
		"WTCPUE" = "wtcpue",
		"NUMCPUE" = "cntcpue",
		"COMMON" = "common",
		"SCIENTIFIC" = "spp",
		"SID" = "SID",
		"BOT_DEPTH" = "depth",
		"BOT_TEMP" = "btemp",
		"SURF_TEMP" = "stemp",
		"VESSEL" = "vessel",
		"CRUISE" = "cruise",
		"HAUL" = "haul",
		"Areakm2" = "stratumarea"
	
	)
	
	return(list(old=names(goa.cols), new=unname(goa.cols)))
	
}


# ========
# = NEUS =
# ========
clean.names.neus <- function(X){
	
	
	neus.cols <- c(
		"STRATUM" = "stratum",
		"SVSPP" = "SID",
		"YEAR" = "year",
		"SEASON" = "season",
		"LAT" = "lat",
		"LON" = "lon",
		"DEPTH" = "depth",
		"CRUISE6" = "cruise",
		"STATION" = "station",
		"BIOMASS" = "weight", # corrected for certain gear changes, but not necessarily for total area hauled
		"ABUNDANCE" = "cnt", # same as biomass – linear correction factors applied to account for gear/ method changes, but not necessarily area trawled (effort only partially accounted for, perhaps)
		"SCINAME" = "spp",
		"Areanmi2" = "stratumarea",
		
		# below here not in ocean adapt
		"SVVESSEL" = "vessel",
		"SURFTEMP" = "stemp",
		"SURFSALIN" = "ssalin",
		"BOTTEMP" = "btemp",
		"BOTSALIN" = "bsalin",
		"LENGTH" = "length",
		"COMNAME" = "common"
	
	)
	
	return(list(old=names(neus.cols), new=unname(neus.cols)))
	
	
}


# ========
# = NEWF =
# ========
clean.names.newf <- function(X){
	
	
	newf.cols <- c(
		# "sppcode",
# 		"recordtype",
# 		"vessel",
# 		"trip",
# 		"set",
		"yearl" = "year",
		"monthl" = "month",
		"dayl" = "day",
# 		"settype",
# 		"stratum",
# 		"nafo",
# 		"unitarea",
# 		"light",
# 		"winddir",
# 		"windforce",
# 		"sea",
# 		"bottom",
		"timel" = "time",
		"duration" = "towduration", 
		"distance" = "towdistance", 
		# "operation",
# 		"depth",
		"depthmin" = "depth.min",
		"depthmax" = "depth.max",
# 		"depthbottom",
		"surftemp" = "stemp", 
		"bottemp" = "btemp", 
		"latstart" = "lat.start", 
		"lonstart" = "lon.start", 
		# "posmethod",
		"gear" = "geartype", 
		"num" = "cnt", 
		"wgt" = "weight", 
		"latend" = "lat.end", 
		"lonend" = "lon.end"#, 
		# "bottempmeth",
# 		"geardevice",
# 		"common",
# 		"spp",
# 		"season"
	)
	
	return(list(old=names(newf.cols), new=unname(newf.cols)))
	
}

# =========
# = NGULF =
# =========
clean.names.ngulf <- function(X){
	message("Function not ready yet")
	
	return(list(old=NULL, new=NULL))
}


# ======
# = SA =
# ======
clean.names.sa <- function(X){
	
	sa.cols <- c(
		"COLLECTIONNUMBER" = "haulid", 
		# "PROJECTNAME",
		# "PROJECTAGENCY",
		"DATE" = "date", 
		# "EVENTNAME",
		"VESSELNAME" = "vessel", 
		# "GEARNAME",
# 		"GEARCODE",
# 		"SPECIESCODE",
# 		"MRRI_CODE",
		"SPECIESSCIENTIFICNAME" = "spp", 
		"SPECIESCOMMONNAME" = "common", 
		"NUMBERTOTAL" = "cnt", 
		"SPECIESTOTALWEIGHT" = "weight", 
		# "SPECIESSUBWEIGHT", 
		# "SPECIESWGTPROCESSED",
		# "WEIGHTMETHODDESC", 
		# "ORGWTUNITS", 
		"EFFORT" = "effort", 
		# "CATCHSUBSAMPLED", 
		# "CATCHWEIGHT", 
		# "CATCHSUBWEIGHT", 
		"TIMESTART" = "time", 
		"DURATION" = "towduration", 
		# "TOWTYPETEXT",
		# "LOCATION",
		# "REGION",
		# "DEPTHZONE",
		# "ACCSPGRIDCODE",
		# "STATIONCODE",
		# "EVENTTYPEDESCRIPTION",
		"TEMPSURFACE" = "stemp", 
		"TEMPBOTTOM" = "btemp", 
		"SALINITYSURFACE" = "ssalin", 
		"SALINITYBOTTOM" = "bsalin", 
		"SDO" = "sdo", 
		"BDO" = "bdo", 
		"TEMPAIR" = "airtemp", 
		"LATITUDESTART" = "lat.start", 
		"LATITUDEEND" = "lat.end", 
		"LONGITUDESTART" = "lon.start", 
		"LONGITUDEEND" = "lon.end", 
		# "SPECSTATUSDESCRIPTION",
		# "LASTUPDATED",
		# "LIGHTPHASE",
		"TIMEZONE" = "timezone", 
		"DEPTHSTART" = "depth.start", 
		"DEPTHEND" = "depth.end", 
		# "PRESSURE",
		# "WINDSPEED",
		# "WINDDIRECTION",
		# "WAVEHEIGHT",
		# "PRECIPITATION",
		# "ESTIMATEDLOC",
		# "SEDSIZEDESC",
		# "BTMCOMPDESC",
		# "WEATHERDESC",
		# "WATERLVLDESC",
		# "ALTERATIONDESC",
		# "ACTIVITYDESC",
		# "NBRREP",
		"COMMENTS" = "comment"
	)
	
	return(list(old=names(sa.cols), new=unname(sa.cols)))
	
}


# =========
# = SGULF =
# =========
clean.names.sgulf <- function(X){
	
	sgulf.cols <- c(
		# "stratum",
		# "vessel",
		# "cruise",
		# "set",
		# "year",
		"species" = "species.code", 
		"catch" = "cntcpue", 
		"biomass" = "wtcpue", 
		# "month",
	# 	"day",
	# 	"expt",
	# 	"time",
	# 	"temperature",
		"depthst" = "depth.start", 
		"depthend" = "depth.end", 
		# "dtow",
		# "depth",
		"latitude" = "lat", 
		"longitude" = "lon", 
		"latin_name" = "spp", 
		"name" = "common", 
		"N" = "cnt", 
		"kg" = "weight", 
		"t_surface" = "stemp", 
		"salin_bottom" = "bsalin", 
		"t_bottom" = "btemp", 
		# "trawlableunits",
		"stratarea" = "stratumarea"
	)
	
	return(list(old=names(sgulf.cols), new=unname(sgulf.cols)))
}

# =========
# = SHELF =
# =========
clean.names.shelf <- function(X){
	
	shelf.cols <- c(
		# "CODE",
		# "stratum",
		# "MISSION",
		# "SETNO",
		# "MARKET",
		# "SAMPWGT",
		"ADJ_TOTWGT" = "wtcpue",
		"ADJ_TOTNO" = "cntcpue",
		# "CALWT",
		"REMARKS" = "comment",
		# "SIZE_CLASS",
		"SDATE" = "date",
		"TIME" = "time",
		"SLAT" = "lat.start",
		"SLONG" = "lon.start",
		"ELAT" = "lat.end",
		"ELONG" = "lon.end",
		"AREA" = "towarea",
		"DUR" = "towduration",
		"DIST" = "towdistance",
		# "HOWD",
		"SPEED" = "towspeed",
		# "HOWS",
		"DMIN" = "depth.min",
		"DMAX" = "depth.max",
		# "WIND",
# 		"FORCE",
# 		"CURNT",
# 		"TYPE",
		# "GEAR" = "geartype",
		# "AUX",
		"DEPTH" = "depth",
		"ETIME" = "date.end",
		"START_DEPTH" = "depth.start",
		"END_DEPTH" = "depth.end",
		"SURFACE_TEMPERATURE" = "stemp",
		"BOTTOM_TEMPERATURE" = "btemp",
		"BOTTOM_SALINITY" = "bsalin",
		# "depthzone_fathoms",
		"stratarea_nmi2" = "stratumarea"
		# "spp",
		# "common"
	)
	
	return(list(old=names(shelf.cols), new=unname(shelf.cols)))
}

# ==========
# = WC ANN =
# ==========
clean.names.wcann <- function(X){
	
	
	

	wcann.cols <- c(
		"Trawl.Id" = "haulid", 
		"Species" = "spp", 
		"Haul.Weight..kg." = "weight",
		# "Individual.Average.Weight..kg." = "average individual weight (weight/count)",
		# "Survey" = "Indicates the survey time series within which a trawl sample was conducted; either the West Coast Slope Bottom Trawl Survey or the West Coast Slope/Shelf Bottom Trawl Survey",
		"Survey.Cycle" = "year", 
		"Vessel" = "vessel", 
		# "Cruise.Leg" = "The West Coast Groundfish Bottom Trawl Surveys have been conducted coastwide from the U.S. / Canada border to a southern boundary that has expanded southward from Point Conception to the U.S. - Mexico border over time (see accompanying data annotations document).  The full north to south extent is traversed in two passes each survey season.  Each pass is divided into four (Slope survey) to five (Slope/Shelf combined survey) continuous segments separated by rest periods in ports along the coast.  Cruise leg indicates the sequential segment (1-5) in which a trawl operation was conducted.",
		# "Trawl.Performance" = "All trawl operations are reviewed and rated as to the quality of their execution and their applicability to formal fishery assessment analysis products.  Trawl Performance indicates these determinations.  See the accompanying data annotations document for a more detailed description.",
		"Trawl.Date" ="date", 
		"Trawl.Start.Time" = "datetime", 
		"Best.Latitude..dd." = "lat", 
		"Best.Longitude..dd." = "lon", 
		# "Best.Position.Type" = "The best trawl position (Best Latitude, Best Longitude) available has been provided in decimal degrees North Latitude and annotated with its type (Best Position Type).  Preference is given to any recorded gear position, usually an estimate of the onbottom midpoint: [(start_lat+end_lat)/2,  (start_lon+end_lon)/2].  If no acceptable onbottom gear position can be determined, a vessel position is provided, usually a similarly determined trawl midpoint estimate for the vessel.  If no acceptable vessel position can be determined, position data recorded for the defined station is provided.",
		"Best.Depth..m." = "depth", 
		# "Best.Depth.Type" = "how depth was determined",
		"Trawl.Duration..min." = "towduration", 
		"Area.Swept.by.the.Net..hectares." = "areaswept", 
		"Temperature.At.the.Gear..degs.C." = "btemp"
	)
	
	return(list(old=names(wcann.cols), new=unname(wcann.cols)))
}

# ==========
# = WC TRI =
# ==========
clean.names.wctri <- function(X){
	


	wctri.cols <- c(
		"SPECIES_CODE" = "SID", # no unit
		# "CRUISEJOIN" = "cruise", # no unit
		# "HAULJOIN" = "haul", # no unit
		"VESSEL" = "vessel", # no unit
		"CRUISE"= "cruise", # no unit
		"HAUL" = "haul", # no unit; note that it's somewhat different from 'hauljoin', but it's just one is to join and the other is ...
		# "CATCHJOIN" = "database id for the catch record",
		"WEIGHT" = "weight",
		"NUMBER_FISH" = "cnt",
		# "HAUL_TYPE" = "type of haul",
		# "PERFORMANCE" = "performance code of the haul",
		"START_TIME" = "time",
		"DURATION" = "towduration", # hours?
		"DISTANCE_FISHED" = "towdistance", #"distance fished", # in km
		"NET_WIDTH" = "gearsize", # m?
		"STRATUM" = "stratum",
		"START_LATITUDE" = "lat.start",
		"END_LATITUDE" = "lat.end",
		"START_LONGITUDE" = "lon.start",
		"END_LONGITUDE" = "lon.end",
		"STATIONID" = "station", # no unit
		"BOTTOM_DEPTH" = "depth",
		"SURFACE_TEMPERATURE" = "stemp",
		"GEAR_TEMPERATURE" = "btemp",
		"SPECIES_NAME" = "spp",
		"COMMON_NAME" = "common",
		"GEAR" = "geartype",
		"time" = "datetime"
	)
	
	return(list(old=names(wctri.cols), new=unname(wctri.cols)))
	
}







