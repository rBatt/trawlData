clean.trimRow <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri")){
	
	reg <- match.arg(reg)
	
	
	get.clean.trimRow <- function(x){
		switch(x,
			ai = clean.trimRow.ai(X),
			ebs = clean.trimRow.ebs(X),
			gmex = clean.trimRow.gmex(X),
			goa = clean.trimRow.goa(X),
			neus = clean.trimRow.neus(X),
			newf = clean.trimRow.newf(X),
			ngulf = clean.trimRow.ngulf(X),
			sa = clean.trimRow.sa(X),
			sgulf = clean.trimRow.sgulf(X),
			shelf = clean.trimRow.shelf(X),
			wcann = clean.trimRow.wcann(X),
			wctri = clean.trimRow.wctri(X)
		)
	}
	
	
	match.badSpp <- function(x, value=FALSE){
		
		ux <- unique(x)
		badEgg <- grepl("[eE][gG]{2}", ux)
		badFish <- grepl("(?<![a-z])fish(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		badLarv <- grepl("(?<![a-z])larv(a[e])?(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		badYoy <- grepl("(?<![a-z])yoy(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		missSpp <- ux=="" | is.na(ux)
		bad.x <- ux[(badEgg | badFish | badLarv | badYoy | missSpp)]
		bad.i <- (x%in%bad.x)
		if(value){
			return(x[bad.i])
		}else{
			return(bad.i)
		}
		
	}
	
	badSpp <- X[,match.badSpp(ref)]
	noID <- X[,spp=="" | is.na(spp)]
	
	
	missSpecies <- X[,species=="" | is.na(species)]
	missGenus <- X[,genus=="" | is.na(genus)]
	
	
	
}




# ======
# = AI =
# ======
clean.trimRow.ai <- function(X){
	
	setkey(ai, spp)
	ai.spp.bad <- c("","Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")
	ai <- ai[!.(ai.spp.bad)]
	
	
}


# =======
# = EBS =
# =======
clean.trimRow.ebs <- function(X){

	# none!

}


# ========
# = GMEX =
# ========
clean.trimRow.gmex <- function(X){
	
	
	gmex.bio00 <- gmex.bio000[BGSCODE!="T" & GENUS_BGS!="UNKNOWN"] 
	
	bad.gmex.CODE <- names(gmex.spp0[,table(CODE)][gmex.spp0[,table(CODE)] > 1])
	good.gmex.CODE <- names(gmex.spp0[,table(CODE)][gmex.spp0[,table(CODE)] <= 1])
	setkey(gmex.spp0, CODE)
	gmex.spp <- gmex.spp0[good.gmex.CODE]
	
	
	gmex.tow <- gmex.tow[GEAR_TYPE=="ST"]
	
	
	
	gmex <- gmex0[TITLE %in% c('Summer SEAMAP Groundfish Survey', 'Summer SEAMAP Groundfish Suvey') & GEAR_SIZE==40 & MESH_SIZE == 1.63 & !is.na(MESH_SIZE) & OP %in% c(''),] # # Trim to high quality SEAMAP summer trawls, based off the subset used by Jeff Rester's GS_TRAWL_05232011.sas
	
	
	gmex <- gmex[gmex$MIN_FISH<=60 & gmex$MIN_FISH > 0 & !is.na(gmex$MIN_FISH),]
	gmex$VESSEL_SPD[gmex$VESSEL_SPD==30] <- 3 # fix typo according to Jeff Rester: 30 = 3
	gmex <- gmex[gmex$VESSEL_SPD <= 5 & gmex$VESSEL_SPD > 0  & !is.na(gmex$VESSEL_SPD),] # trim out vessel speeds 0, unknown, or >5 (need vessel speed to calculate area trawled)
	
	
	setkey(gmex, year, lat, lon)
	dups <- duplicated(gmex, by=c("year", "lat", "lon")) & !duplicated(gmex, by="haulid")
	dupped <- gmex[gmex[dups, list(year, lat, lon)],]
	badhaul <- dupped[,unique(haulid[grep("PORT", COMSTAT)])] # malin line 255, after %in%
	setkey(gmex, haulid)
	gmex <- gmex[!.(badhaul)]
	
	
	
	setkey(gmex, spp)
	gmex <- gmex[!is.na(spp)]
	gmex.spp.bad <- c("",'UNID CRUSTA', 'UNID OTHER', 'UNID.FISH', 'CRUSTACEA(INFRAORDER) BRACHYURA', 'MOLLUSCA AND UNID.OTHER #01', 'ALGAE', 'MISCELLANEOUS INVERTEBR', 'OTHER INVERTEBRATES')
	gmex <- gmex[!.(gmex.spp.bad)]
	
	
}


# =======
# = GOA =
# =======
clean.trimRow.goa <- function(X){
	
	
	setkey(goa, spp)
	goa.spp.bad <- c("","Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")
	goa <- goa[!.(goa.spp.bad)]
	
	
}


# ========
# = NEUS =
# ========
clean.trimRow.neus <- function(X){
	
	setkey(neus000, SEASON)
	neus00 <- neus000["SPRING"]
	
	
	setkey(neus, spp)
	neus <- neus[!is.na(spp)]
	neus.spp.bad <- c("",'UNIDENTIFIED FISH', 'ILLEX ILLECEBROSUS EGG MOPS', 'LOLIGO PEALEII EGG MOPS')
	neus <- neus[!.(neus.spp.bad)]
	
	
}


# ========
# = NEWF =
# ========
clean.trimRow.newf <- function(X){
	
	
	ss1 <- newf.raw0$operation %in% c(1,2) & newf.raw0$recordtype == 6 # 6 is biological data, 5 is set information
	newf.raw0 <- newf.raw0[ss1,]

	ss2 <- newf.raw0$gear == 61 & !is.na(newf.raw0$gear) # CAMPELEN 1800 SHRIMP TRAWL--LINED
	newf.raw0 <- newf.raw0[ss2,]

	ss3 <- newf.raw0$settype == 1
	newf.raw0 <- newf.raw0[ss3,]
	
	
	
	setkey(newf.raw, season)
	newf00 <- newf.raw["fall"]
	
	
	
	newf0 <- newf00[duration<=60,]
	
	
	
	
	dim(newf) # 378070, 50
	newf[,sum(spp=="", na.rm=TRUE)] # 454
	newf[,sum(is.na(spp))] # 120

	newf <- newf["fall"] # trim to only fall
	dim(newf) # 257486, 50
	newf[,sum(spp=="", na.rm=TRUE)] # 234
	newf[,sum(is.na(spp))] # 26

	setkey(newf, spp)
	newf <- newf[spp!=""&!is.na(spp),]
	dim(newf) # 257226, 50
	
	
	
	
	
	
}

# =========
# = NGULF =
# =========
clean.trimRow.ngulf <- function(X){

}


# ======
# = SA =
# ======
clean.trimRow.sa <- function(X){
	
	sa.strata <- fread(paste(sa.start, "malinpinsky.CoastalEvent.csv", sep=""), select=c("COLLECTIONNUMBER","DEPTHSTART","DEPTHEND"))
	sa.strata <- sa.strata[COLLECTIONNUMBER!="",]
	setnames(sa.strata, "COLLECTIONNUMBER", "haulid")
	
	
	sa.catch0 <- sa.catch00[PROJECTNAME!=""&DEPTHZONE=="INNER", list(datetime, spp, common, haulid, stratum, LATITUDESTART, LATITUDEEND, LONGITUDESTART, LONGITUDEEND, stemp, btemp, cnt, wt, effort)]

	sa.catch0 <- sa.catch0[effort!=0 | is.na(effort)] # remove rows where the effort was reported as 0 (alternatively, these could be changed to NA, which might still be useful b/c you'd know that a species was observed, just not abundance or biomass)
	
	
	
	
}


# =========
# = SGULF =
# =========
clean.trimRow.sgulf <- function(X){
	
	sgulf.set <- sgulf.set0[expt==1,list(year, datetime, haulid, stratum, stemp, btemp)]
	
	
	
}

# =========
# = SHELF =
# =========
clean.trimRow.shelf <- function(X){
	
	cols2keep <- quote(list(year, month, datetime, haulid, stratum, stratumarea, SLAT, SLONG, depth, SPEC, stemp, btemp, wtcpue, cntcpue))
	# shelf.raw00 <- shelf.raw000[TYPE==1 & month>=6 & month<= 8, list(year, month, datetime, haulid, stratum, SLAT, SLONG, depth, SPEC, stemp, btemp, wtcpue, cntcpue)] 
	shelf.raw00 <- shelf.raw000[TYPE==1 & month>=6 & month<= 8, eval(cols2keep)] 
	
	
	shelf.raw <- shelf.raw0[!grepl("UNIDENT| UNID|^UNID", spp) & !grepl("EGGS|PURSE", spp) & !grepl("EGGS", common) & !is.na(spp) & spp!="" & haulid!="NED2010027-201",]
	
	
	
	bad.shelf.spp <- c("LOLIGINIDAE,OMMASTREPHIDAE F.", "PROTOBRANCHIA, HETERODONTA", "LITHODES/NEOLITHODES","POLYCHAETA C.,LARGE", "SEA CORALS (NS)")
	vague.shelf.pat <- "\\sS\\.[OC]\\.|\\s[OCFP]\\." #"\\sS\\.O\\.|\\sO\\.|\\sC\\.|\\F\\.|"
	shelf <- shelf.raw[!grepl(vague.shelf.pat, spp) & !spp%in%bad.shelf.spp, list(year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, stemp, btemp, wtcpue, cntcpue)]
	
		
	
}

# ==========
# = WC ANN =
# ==========
clean.trimRow.wcann <- function(X){
	
	
	setkey(wcann, spp)
	wcann.spp.bad <- c("","Apristurus brunneus egg case", "gastropod eggs", "Selachimorpha egg case")
	wcann <- wcann[!.(wcann.spp.bad)]
	
	


}

# ==========
# = WC TRI =
# ==========
clean.trimRow.wctri <- function(X){
	
	setkey(wctri, spp)
	wctri.spp.bad <- c("","Apristurus brunneus egg case", "fish eggs unident.", "Raja binoculata egg case", "Raja sp. egg case", "Rajiformes egg case", "Shark egg case unident.")
	wctri <- wctri[!.(wctri.spp.bad)]
	
	
	
}







