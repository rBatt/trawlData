#' Clean by Indicating Rows to Trim
#' 
#' Cleans by indicating which rows should be dropped
#' 
#' @template X_reg
#' 
#' @details
#' Recommended rows to drop according to Malin's original scripts and what's in the OceanAdapt repo. Rows are not actually dropped; rather, a column called \code{keep.row} is added to the data.table; when \code{keep.row} is \code{FALSE}, it is recommended that the row be dropped.
#' 
#' In SA, \code{c("Anchoa hepsetus","Anchoa lyolepis","Anchoa mitchilli","Anchoa cubana")} were only ID'd to genus correctly. Rows corresponding to these species are flagged for being dropped, but if desired for an analysis at genus level, can be recovered by examining the \code{spp} column for these species and the \code{row_flag} column for "Spp1".
#' 
#' @template clean_seeAlso_template
#' 
#' @export clean.trimRow
clean.trimRow <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri")){
	
	reg <- match.arg(reg)
	
	# first, set the default action to keeping all rows
	# such that if nothing else is done, all will be kept
	X[,keep.row:=TRUE]
	
	# define region-specific function
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
		badPurse <- grepl("(?<![a-z])purse(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		badLarv <- grepl("(?<![a-z])larv(a[e])?(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		badYoy <- grepl("(?<![a-z])yoy(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
		missSpp <- ux=="" | is.na(ux)
		bad.x <- ux[(badEgg | badFish | badPurse | badLarv | badYoy | missSpp)]
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
	
	spp.i <- !(badSpp | noID | missSpecies | missGenus)
	
	X[,row_flag:=""]
	get.clean.trimRow(reg) # do the region-specific checks on which rows to trim out
	X[,keep.row:=(keep.row&spp.i)]
	X[!spp.i,row_flag:=paste(row_flag, "Spp2")]
	
	invisible(NULL)
}




# ======
# = AI =
# ======
clean.trimRow.ai <- function(X){
	
	# only trimmings in original read file were related to bad spp
	invisible(NULL)
	
}


# =======
# = EBS =
# =======
clean.trimRow.ebs <- function(X){

	# none!
	invisible(NULL)
}


# ========
# = GMEX =
# ========
clean.trimRow.gmex <- function(X){
	
	spp.i <- X[,BGSCODE!="TRUE"]
	# "TRUE" is young of year records
	
	
	survey.i <- X[, survey.name %in% c('Summer SEAMAP Groundfish Survey')]
	
	
	gear.i <- X[, 
		gearsize==40 & 
		meshsize == 1.63 & 
		!is.na(meshsize) & 
		OP %in% c('') & 
		geartype=="ST"
	]
	# "ST" is shrimp trawl
	# Other are in accordance with Jeff Rester's GS_TRAWL_05232011.sas
	
	
	tow.i <- X[,
		towduration<=60 & 
		towduration>0 & 
		!is.na(towduration) & 
		towspeed<=5 & 
		towspeed>0 & 
		!is.na(towspeed)
	]
	# trim out vessel speeds 0, unknown, or >5 (need vessel speed to calculate area trawled)
	
	
	haul.i <- !X[
		i=, # yes, having nothing after the i= is intentional; i= isn't need; there for clarity
		j=list(nhid=lu(haulid)>1, station.comment, haulid),
		by=c("year","lat","lon")
	][
		i=(nhid), 
		j=X[,haulid]%in%haulid[grepl("PORT", station.comment)]
	]
	# In cases where there are multiple hauls in the same place on teh same date,
	# if one of those hauls has the word "PORT" in the comments,
	# drop those hauls. The "PORT" seem to be problematic,
	# according to Malin's interpretation of the comments.
	
	
	keep.row.i <- spp.i & survey.i & gear.i & tow.i & haul.i
	X[,keep.row:=keep.row.i]
	
	X[!gear.i,row_flag:=paste(row_flag, "Gear")]
	X[!haul.i,row_flag:=paste(row_flag, "Haul")]
	X[!spp.i,row_flag:=paste(row_flag, "Spp1")]
	X[!survey.i,row_flag:=paste(row_flag, "Surv")]
	X[!tow.i,row_flag:=paste(row_flag, "Tow")]
	
	invisible(NULL)
}


# =======
# = GOA =
# =======
clean.trimRow.goa <- function(X){
	
	
	# only trimmings in original read file were related to bad spp
	
	invisible(NULL)
}


# ========
# = NEUS =
# ========
clean.trimRow.neus <- function(X){
	
	season.i <- X[,season=="spring"]
	
	keep.row.i <- season.i
	X[,keep.row:=keep.row.i]
	
	X[!season.i,row_flag:=paste(row_flag, "Seas")]
	
	
	# some trimmings in original read file were related to bad spp
	
	invisible(NULL)
}


# ========
# = NEWF =
# ========
clean.trimRow.newf <- function(X){
	
	record.i <- X[,recordtype==6] # 6 is biological data, 5 is set info
	
	haul.i <- X[,operation%in%c(1,2)]
	
	gear.i <- X[,geartype==61 & !is.na(geartype) & settype==1]
	
	tow.i <- X[,towduration<=60]
		
	season.i <- X[,season=="fall"]
		
	keep.row.i <- record.i & haul.i & gear.i & tow.i & season.i
	X[,keep.row:=keep.row.i]
	
	X[!gear.i,row_flag:=paste(row_flag, "Gear")]
	X[!haul.i,row_flag:=paste(row_flag, "Haul")]
	X[!record.i,row_flag:=paste(row_flag, "Rec")]
	X[!season.i,row_flag:=paste(row_flag, "Seas")]
	X[!tow.i,row_flag:=paste(row_flag, "Tow")]
	
	invisible(NULL)
}

# =========
# = NGULF =
# =========
clean.trimRow.ngulf <- function(X){
	message("this function not ready yet")
	invisible(NULL)
}


# ======
# = SA =
# ======
clean.trimRow.sa <- function(X){
	
	haul.i <- X[,!is.na(haulid) & haulid!=""]
	
	strat.i <- X[, DEPTHZONE=="INNER"]
	
	survey.i <- X[, PROJECTNAME!=""]
	
	effort.i <- X[,effort!=0 | is.na(effort)] # just don't let it be 0 ...
	
	gear.i <- X[!is.na(wtcpue)] # a lot of the missing weights are long-line data, according to JWM
	
	spp.i <- X[,!spp%in%c("Anchoa hepsetus","Anchoa lyolepis","Anchoa mitchilli","Anchoa cubana")] # these anchovy spp were really only ID to genus
	
	keep.row.i <- haul.i & strat.i & survey.i & effort.i & spp.i & gear.i
	X[,keep.row:=keep.row.i]
	
	X[!effort.i,row_flag:=paste(row_flag, "Eff")]
	X[!gear.i,row_flag:=paste(row_flag, "Gear")]
	X[!haul.i,row_flag:=paste(row_flag, "Haul")]
	X[!spp.i,row_flag:=paste(row_flag, "Spp1")]
	X[!strat.i,row_flag:=paste(row_flag, "Strat")]
	X[!survey.i,row_flag:=paste(row_flag, "Surv")]
	
	invisible(NULL)
	
}


# =========
# = SGULF =
# =========
clean.trimRow.sgulf <- function(X){
		
	set.i <- X[,expt==1]
	
	keep.row.i <- set.i
	X[,keep.row:=keep.row.i]
	
	X[!set.i,row_flag:=paste(row_flag, "Set")]
	
	invisible(NULL)
}

# =========
# = SHELF =
# =========
clean.trimRow.shelf <- function(X){
	
	type.i <- X[, TYPE==1]
	
	if(exists("month", where="package:data.table")){
		# the `month` function is listed as experimental
		# So I'm testing to make sure it exists 
		# rather than blindly relying on it
		# This method is faster, hence the motivation:
		# > system.time({months <- X[,format.Date(datetime, "%m")]
# 		+ 		date.i <- months>=6 & months<=8})
# 		   user  system elapsed
# 		  0.201   0.005   0.214
# 		> system.time({date.i <- X[,month(datetime)>=6 & month(datetime)<=8]})
# 		   user  system elapsed
# 		  0.042   0.004   0.057
		
		date.i <- X[,month(datetime)>=6 & month(datetime)<=8]
	}else{
		message("data.table:::month doesn't exist; using alternative method; see clean.trimRow.shelf()")
		months <- X[,format.Date(datetime, "%m")]
		date.i <- months>=6 & months<=8
	}
	
	
	haul.i <- X[,haulid!="NED2010027-201"]	
	
	keep.row.i <- type.i & date.i & haul.i
	X[,keep.row:=keep.row.i]
	
	X[!date.i,row_flag:=paste(row_flag, "Date")]
	X[!haul.i,row_flag:=paste(row_flag, "Haul")]
	X[!type.i,row_flag:=paste(row_flag, "Type")]
	
	invisible(NULL)
}

# ==========
# = WC ANN =
# ==========
clean.trimRow.wcann <- function(X){
	
	# only removed bad species
	invisible(NULL)
}

# ==========
# = WC TRI =
# ==========
clean.trimRow.wctri <- function(X){
	
	# only removed bad species
	invisible(NULL)
}







