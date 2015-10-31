library(taxize)
library(data.table)

load("./data/taxInfo.RData")
load("./data/spp.corr1.RData")

create.spp.key <- function(spp, taxInfo, spp.corr1){
	X.match <- match.tbl(ref=spp[-1], tbl.ref=taxInfo[,raw.spp], tbl.val=taxInfo[,spp])
	X.match[,mtch.src:=1]
	X.match2 <- match.tbl(ref=spp[-1], tbl.ref=spp.corr1[,spp], tbl.val=spp.corr1[,sppCorr])
	X.match2[,mtch.src:=2]
	
	need.search.i <- X.match[,is.na(val)] & X.match2[,is.na(val)]
	need.search <- spp[-1][need.search.i]
	match.gs <- getSpp(c.all(need.search))
	match.gs[,sppCorr:=cullPost2(sppCorr)]
	# match.gs[,spp.orig:=need.search]
	X.match3 <- data.table(
		ref = X.match[,ref],
		val = NA_character_,
		val.src = NA_character_,
		tbl.row = NA_real_,
		mtch.src = 3
	)
	X.match3[need.search.i,val:=match.gs[,sppCorr]]
	
	spp.key0 <- rbind(X.match, X.match2, X.match3)
	
	sk.agree <- spp.key0[,list(matchesAgree=(length(unique(val[!is.na(val)]))<=1L)),by=ref] # all non-NA matches should be same
	setkey(spp.key0, ref)
	conflict.key <- spp.key0[sk.agree[!(matchesAgree)]]
	setorder(spp.key0, ref, val, mtch.src, na.last=T)
	
	# spp.key <- unique(spp.key0[!is.na(val)])
	spp.key <- unique(spp.key0)
	setnames(spp.key, "val", "spp")
	setkey(spp.key, spp)
	
	
	ti2 <- copy(taxInfo)
	setkey(ti2, spp)
	ti2 <- unique(ti2)
	ti2[,raw.spp:=NULL]
	ti2[,correctSpp:=NULL]
	ti2[,isSpecies:=NULL]
	ti2[,taxLvl:=tolower(taxLvl)]
	
	spp.key <- merge(spp.key, ti2, by="spp", all.x=T)
	setcolorder(spp.key, c("ref", "val.src", "tbl.row", "mtch.src", "spp", "common", "taxLvl", "species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom", "trophicDiet", "trophicOrig", "Picture", "trophicLevel", "trophicLevel.se"))
	
	spp.key[is.na(spp), mtch.src:=NA_real_]
	
	
	
	# ===========================
	# = Search and Update Again =
	# ===========================
	# Taxonomny always seems to be a highly iterative process
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
	
	# index of things to not bother trying to find
	badSpp <- spp.key[,match.badSpp(ref)]
	noID <- spp.key[,spp=="" | is.na(spp)]
	
	# index of things to lookup for COMMON
	lookup.cmmn.i <- (!badSpp & !noID) & spp.key[,is.na(common) | common==""]
	lookup.cmmn <- spp.key[lookup.cmmn.i, spp]
	
	# index of things to lookup for CLASS
	class.names <- c("species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom")
	lookup.class.i <- (!badSpp & !noID) & rowSums(!is.na(spp.key[,eval(s2c(class.names))]))==0
	lookup.class <- spp.key[lookup.class.i, spp]
	
	# lookup both COMMON & CLASS
	find.class <- getTax(unique(lookup.class))
	find.cmmn <- getCmmn(unique(lookup.cmmn))
	
	# insert non-NA COMMON names where needed
	cn.cols <- c("taxLvl",class.names)
	for(cn in 1:length(cn.cols)){
		t.cn <- cn.cols[cn]
		t.m <- find.class[, match.tbl(spp.key[,spp], sppCorr, eval(s2c(t.cn))[[1]], exact=T)]
		setorder(t.m, ref, na.last=TRUE)
		setorder(spp.key, spp, na.last=TRUE)
		needs <- spp.key[,is.na(eval(s2c(t.cn))[[1]])]
		has <- t.m[,!is.na(val)]
		spp.key[needs&has,(t.cn):=t.m[needs&has,val]]
	}
	
	# insert non-NA taxonomnic CLASSifications where needed
	match.cmmn <- find.cmmn[, match.tbl(spp.key[,spp], sppCorr, common, exact=T)]
	needs.cmmn <- spp.key[,is.na(common)]
	has.cmmn <- match.cmmn[,!is.na(val)]
	spp.key[needs.cmmn&has.cmmn,common:=match.cmmn[needs.cmmn&has.cmmn,val]]
	
	
	
	# ======================
	# = Manual Corrections =
	# ======================	
	# noticed in GMEX
	# match.badSpp <- function(x, value=FALSE){
	#
	# 	ux <- unique(x)
	# 	badEgg <- grepl("[eE][gG]{2}", ux)
	# 	badFish <- grepl("(?<![a-z])fish(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
	# 	badLarv <- grepl("(?<![a-z])larv(a[e])?(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
	# 	badYoy <- grepl("(?<![a-z])yoy(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
	# 	missSpp <- ux=="" | is.na(ux)
	# 	bad.x <- ux[(badEgg | badFish | badLarv | badYoy | missSpp)]
	# 	bad.i <- (x%in%bad.x)
	# 	if(value){
	# 		return(x[bad.i])
	# 	}else{
	# 		return(bad.i)
	# 	}
	#
	# }
	# badSpp <- X[,match.badSpp(ref)]
	# noID <- X[,spp=="" | is.na(spp)]
	# dput(X[!badSpp&!noID&taxLvl!="species"&!is.na(taxLvl)&is.species(spp)&is.na(common),unique(spp)])
	# c("Astrea Orbicella", "Bathynectes superba", "Centropristes ocyurus", "Glyphocrangon aculeata", "Lycoteuthis diadema", "Moira atropus", 	"Mustellus canis", "Phenacoscorpius nebris", "Synagrops bella", "Synagrops microlepis")
	# Function to see if the corrected version of a bad spp name
	# already exists in a data set; if it does,
	# then the wrong version is overwritten with the content 
	check.and.set <- function(wrong, corrected){
		check <- spp.key[,corrected%in%spp]
		if(check){
			# if the corrected name already exists,
			# make sure to have all the rows with the wrong name match up with the corrected rows,
			# that way if we make any changes, both sets get updated
			# For example, if a name XX is wrong, and Xx is the corrected name,
			# say that we are going to set the trophic level of Xx to 42, 
			# but the current entry is 40. If we were to say 'change all rows
			# with name XX to have a TL to 42, and also switch the bad XX name to the good Xx name',
			# then we would have some rows with TL of 42 (the ones that originally had the bad name), and 
			# some rows with TL of 40 (the ones that originally had the corrected name).
			# Thus, we have to get the names and other content to match before changing anything.
			# Bottom line is that we need to be sure that all things are consistent, and that this requires
			# more care when we are switching the 'spp' of an entry to a 'spp' that is already there.
			# 
			# stopifnot(all(sapply(spp.key[spp==corrected], function(x)length(unique(x[!is.na(x)]))<=1))) # this check is to ensure that the contents of the corrected data set do not contain conflicts (NA's aside, which may or may not be a good idea)
			stopifnot(all(sapply(spp.key[spp==corrected], function(x)length(unique(x))<=1)))
			noSet <- c("ref")
			all.but.noSet <- names(spp.key)[names(spp.key)!=noSet]
			spp.key[spp==wrong, c(all.but.noSet):=spp.key[spp==corrected,eval(s2c(all.but.noSet))]]			
		}else{
			# if the corrected name doesn't already exist,
			# then simply switch the wrong name to the corrected name,
			spp.key[spp==wrong, spp:=corrected]
		}
	}
	
	check.and.set(wrong="Moira atropus", corrected="Moira atropos")
	spp.key[spp=="Moira atropos",
		':='(
			taxLvl="species",
			species="Moira atropos",
			genus="Moira",
			website="http://www.marinespecies.org/echinoidea/aphia.php?p=taxdetails&id=158067"
		)
	]
	
	# spp.key[spp=="Astrea Orbicella", # couldn't find this one
	# ]
	
	check.and.set(wrong="Bathynectes superba", corrected="Bathynectes maravigna")
	spp.key[spp=="Bathynectes maravigna",
		':='(
			taxLvl="species",
			species="Bathynectes maravigna",
			genus="Bathynectes",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=107377"
		
		)
	]
	
	check.and.set(wrong="Centropristes ocyurus", corrected="Centropristis ocyurus")
	spp.key[spp=="Centropristis ocyurus",
		':='(
			taxLvl="species",
			species="Centropristis ocyurus",
			genus="Centropristis",
			common="Bank sea bass", 
			Picture="y", 
			trophicLevel=3.5, 
			trophicLevel.se=0.53,  
			website="http://www.fishbase.org/summary/3316"
		)
	]
	
	check.and.set(wrong="Glyphocrangon aculeata", corrected="Glyphocrangon aculeata")
	spp.key[spp=="Glyphocrangon aculeata",
		':='(
			taxLvl="species",
			species="Glyphocrangon aculeata",
			genus="Glyphocrangon",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=421812"
		
		)
	]
	
	check.and.set(wrong="Lycoteuthis diadema", corrected="Lycoteuthis lorigera")
	spp.key[spp=="Lycoteuthis lorigera",
		':='(
			taxLvl="species",
			species="Lycoteuthis lorigera",
			genus="Lycoteuthis",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=342361"

		)
	]
	
	# found a fix for Mustellus canis (only 1 l), but new information tells we
	# that we already have the correct name somewhere, so I have to fix then update both
	# spp.key[spp=="Mustelus canis"]
	
	check.and.set(wrong="Mustellus canis", corrected="Mustelus canis")
	spp.key[spp=="Mustelus canis",
		':='(
			taxLvl="species",
			common="Dusky smooth-hound",
			Picture="y",
			trophicLevel=3.6,
			# trophicLevel.se=0.2,
			website="http://www.fishbase.org/summary/Mustelus-canis.html"
		)
	]
	
	check.and.set(wrong="Phenacoscorpius nebris", corrected="Phenacoscorpius nebris")
	spp.key[spp=="Phenacoscorpius nebris",
		':='(
			taxLvl="species",
			species="Phenacoscorpius nebris",
			genus="Phenacoscorpius",
			common="Short-tube scorpionfish",
			Picture="y",
			trophicLevel=3.5,
			trophicLevel.se=0.6,
			website="http://www.fishbase.org/summary/12454"
		)
	]
	
	
	
	spp.key[spp=="Synagrops bellus",
		':='(
			taxLvl="species",
			species="Synagrops bellus",
			genus="Synagrops",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=159584"
		)
	]
	
	
	spp.key[spp=="Synagrops microlepis",
		':='(
			taxLvl="species",
			species="Synagrops microlepis",
			genus="Synagrops",
			common="Thinlip splitfin",
			Picture="y",
			trophicLevel=3.2,
			trophicLevel.se=0.37,
			website="http://www.fishbase.org/summary/5059"

		)
	]
	
	
	
	
	
	# spp.key[!is.na(spp) & !is.na(species) & taxLvl=="species"] # these are probably the good ones
	
	save(spp.key, file="data/spp.key.RData")
	write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)
	
	return(spp.key)
}

