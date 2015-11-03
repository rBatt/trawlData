library(taxize)
library(data.table)

load("./data/taxInfo.RData")
load("./data/spp.corr1.RData")
load("data/getSppData.RData")
load("data/getTaxData.RData")
load("data/getCmmnData.RData")

create.spp.key <- function(spp, taxInfo, spp.corr1){
	X.match <- match.tbl(ref=spp[-1], tbl.ref=taxInfo[,raw.spp], tbl.val=taxInfo[,spp])
	X.match[,mtch.src:=1]
	X.match2 <- match.tbl(ref=spp[-1], tbl.ref=spp.corr1[,spp], tbl.val=spp.corr1[,sppCorr])
	X.match2[,mtch.src:=2]
	# X.match3.1 <- match.tbl(ref=spp[-1],tbl.ref=match.gs[,spp],tbl.val=match.gs[,sppCorr])
	X.match3 <- match.tbl(ref=spp[-1],tbl.ref=getSppData[,spp],tbl.val=getSppData[,sppCorr], exact=T)
	X.match3[,mtch.src:=3]
	
	# need.search.i <- X.match[,is.na(val)] & X.match2[,is.na(val)]
	# need.search <- spp[need.search.i][-1]
	# getSppData <- getSpp(c.all(need.search))
	# getSppData[,sppCorr:=cullPost2(sppCorr)]
	# getSppData[,spp.orig:=need.search]
	# X.match3 <- data.table(
# 		ref = X.match[,ref],
# 		val = NA_character_,
# 		val.src = NA_character_,
# 		tbl.row = NA_real_,
# 		mtch.src = 3
# 	)
# 	X.match3[need.search.i,val:=getSppData[,sppCorr]]
	
	# spp.key0 <- rbind(X.match, X.match2, X.match3)
	spp.key00 <- rbind(X.match[!is.na(val)], X.match2[!is.na(val)], X.match3[!is.na(val)])
	spp.noMatch <- X.match[!ref%in%spp.key00[,ref], ref]
	X.noMatch <- data.table(
		ref = spp.noMatch,
		val = NA_character_,
		val.src = NA_character_,
		tbl.row = NA_real_,
		mtch.src = NA_real_
	)
	spp.key0 <- rbind(spp.key00, X.noMatch)
	
	
	sk.agree <- spp.key0[,list(matchesAgree=(length(unique(val[!is.na(val)]))<=1L)),by=ref] # all non-NA matches should be same
	setkey(spp.key0, ref)
	conflict.key <- spp.key0[sk.agree[!(matchesAgree)]]
	setorder(spp.key0, ref, mtch.src, val, na.last=T)
	
	
	# spp.key <- unique(spp.key0[!is.na(val)])
	spp.key <- unique(spp.key0)
	# X.match2[!is.na(val)][X.match2[!is.na(val),ref] %in% spp.key[is.na(spp), unique(ref)]]
	setnames(spp.key, "val", "spp")
	setkey(spp.key, spp)
	
	
	ti2 <- copy(taxInfo)
	setkey(ti2, spp)
	ti2 <- unique(ti2)
	ti2[,raw.spp:=NULL]
	ti2[,correctSpp:=NULL]
	ti2[,isSpecies:=NULL]
	ti2[,taxLvl:=tolower(taxLvl)]
	ti2[,tax.src:="taxInfo"]
	
	spp.key <- merge(spp.key, ti2, by="spp", all.x=T)
	setcolorder(spp.key, c("ref", "val.src", "tbl.row", "mtch.src", "tax.src", "spp", "common", "taxLvl", "species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom", "trophicDiet", "trophicOrig", "Picture", "trophicLevel", "trophicLevel.se"))
	
	
	for(i in 2:length(names(getTaxData))){
		tn <- names(getTaxData)[i]
		if(!tn%in%names(spp.key)){
			# If we're trying to add a column that spp.key doesn't have, then skip
			next
		}
		
		t.gTD <- getTaxData[,eval(s2c(tn))][[1]]
		t.s.k <- spp.key[,eval(s2c(tn))][[1]]
		
		can.help <- spp.key[is.na(t.s.k), spp] %in%  getTaxData[!is.na(t.gTD), sppCorr]
		
		if(any(can.help)){
			mt <- match.tbl(ref=spp.key[can.help,spp], tbl.ref=getTaxData[,sppCorr], tbl.val=getTaxData[,eval(s2c(tn))][[1]], exact=T)
			spp.key[can.help, c(tn, "tax.src2"):=list(mt[,val],"getTaxData")]
		}else{
			next
		}
	}
	
	# spp.key[is.na(spp), mtch.src:=NA_real_]
	
	
	
	# # ===========================
# 	# = Search and Update Again =
# 	# ===========================
# 	# Taxonomny always seems to be a highly iterative process
# 	match.badSpp <- function(x, value=FALSE){
#
# 		ux <- unique(x)
# 		badEgg <- grepl("[eE][gG]{2}", ux)
# 		badFish <- grepl("(?<![a-z])fish(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
# 		badLarv <- grepl("(?<![a-z])larv(a[e])?(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
# 		badYoy <- grepl("(?<![a-z])yoy(?![a-z])", ux, ignore.case=TRUE, perl=TRUE)
# 		missSpp <- ux=="" | is.na(ux)
# 		bad.x <- ux[(badEgg | badFish | badLarv | badYoy | missSpp)]
# 		bad.i <- (x%in%bad.x)
# 		if(value){
# 			return(x[bad.i])
# 		}else{
# 			return(bad.i)
# 		}
#
# 	}
#
# 	# index of things to not bother trying to find
# 	badSpp <- spp.key[,match.badSpp(ref)]
# 	noID <- spp.key[,spp=="" | is.na(spp)]
#
# 	# index of things to lookup for COMMON
# 	lookup.cmmn.i <- (!badSpp & !noID) & spp.key[,is.na(common) | common==""]
# 	lookup.cmmn <- spp.key[lookup.cmmn.i, spp]
#
# 	# index of things to lookup for CLASS
# 	class.names <- c("species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom")
# 	lookup.class.i <- (!badSpp & !noID) & rowSums(!is.na(spp.key[,eval(s2c(class.names))]))==0
# 	lookup.class <- spp.key[lookup.class.i, spp]
#
# 	# lookup both COMMON & CLASS
# 	find.class <- getTax(unique(lookup.class))
# 	find.cmmn <- getCmmn(unique(lookup.cmmn))
#
# 	# insert non-NA COMMON names where needed
# 	cn.cols <- c("taxLvl",class.names)
# 	for(cn in 1:length(cn.cols)){
# 		t.cn <- cn.cols[cn]
# 		t.m <- find.class[, match.tbl(spp.key[,spp], sppCorr, eval(s2c(t.cn))[[1]], exact=T)]
# 		setorder(t.m, ref, na.last=TRUE)
# 		setorder(spp.key, spp, na.last=TRUE)
# 		needs <- spp.key[,is.na(eval(s2c(t.cn))[[1]])]
# 		has <- t.m[,!is.na(val)]
# 		spp.key[needs&has,(t.cn):=t.m[needs&has,val]]
# 	}
	
	# insert non-NA taxonomnic CLASSifications where needed
	match.cmmn <- getCmmnData[, match.tbl(spp.key[,spp], sppCorr, common, exact=T)]
	needs.cmmn <- spp.key[,is.na(common)]
	has.cmmn <- match.cmmn[,!is.na(val)]
	spp.key[needs.cmmn&has.cmmn,common:=match.cmmn[needs.cmmn&has.cmmn,val]]
	
	spp.key[!is.na(spp)&(!is.na(tax.src)|!is.na(tax.src2)|!is.na(common))]
	spp.key[!is.na(spp)&!is.na(taxLvl)&taxLvl=="species"]
	
	spp.key[!is.na(spp), conflict:=any(!sapply(.SD, function(x)lu(x[!is.na(x)])<=1)), by="spp"]
	
	
	
	
	if(!"flag"%in%names(spp.key)){
		spp.key[,flag:=NA_character_]
	}
	spp2loop <- spp.key[!is.na(spp) & conflict & is.na(flag),unique(spp)]
	for(i in 1:length(spp2loop)){
		t.spp <- spp2loop[i]
		t.index <- spp.key[,spp==t.spp & !is.na(spp)]
		print(paste0(t.spp, "; ", i, " of ",length(spp2loop)))
		flag.spp(spp.key, t.index)
	}
	
	
	# =========================================
	# = Check for and Correct Inconsistencies =
	# =========================================
	check.consistent <- function(Z, col2check=names(Z)[!names(Z)%in%c(by.col,not.consistent)], by.col="spp", not.consistent=c("ref","flag")){
		replacementsMade <- 0
		replacementsFailed <- 0
		replacementUnneeded <- 0
		
		taxProblemSolution <- data.table(spp=character(), prob.col=character(), solution=character())
		tPS <- FALSE
		
		for(i in unique(Z[,eval(s2c(by.col))][[1]])){
			for(j in col2check){
				t.out <- Z[i,get(j)] # temporary value for a given `spp` and the j column
				if(lu(t.out)>1){
					if(lu(t.out[!is.na(t.out)])==1){ # if the 2+ values are just an NA and something else
						# then just replace the NA with the something else
						replacementsMade <- replacementsMade + 1L
						Z[i,c(j):=list(unique(t.out[!is.na(t.out)]))]
					}else{ # otherwise, if there are more than 2 non-NA unique values
						prob.spp <- i # prob. means "problem" / "problematic"
						prob.col <- j
						prob.opts <- unique(t.out[!is.na(t.out)])
				

						fix.taxProb <- function(){ # defining in function in loop for readability
							readline(paste(
								"Pick your solution. SKIP to skip, otherwise enter one of the following exactly (don't add quotes, etc.):\n ", 
								paste0(prob.opts, collapse="\n  "), "\n"
							))
						}
				
						if(tPS){ # FALSE; a prompt from when I had this in a script, was TRUE if reading in a file that had some answers
							prob.fix.t <- taxProblemSolution[spp==i & prob.col==j,solution]
							if(length(prob.fix.t)>0){
								prob.fix <- prob.fix.t
							}else{
								print(j)
								print(Z[i])
								prob.fix <- fix.taxProb()
								taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
							}
						}else{
							print(j)
							print(Z[i])
							prob.fix <- fix.taxProb()
							taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
						}
				
				
						if(prob.fix!="SKIP"){ # I've never tested the use of the SKIP response ...
							Z[i,c(j):=list(prob.fix)]
							replacementsMade <- replacementsMade + 1L
						}else{
							replacementsFailed <- replacementsFailed + 1L
						}
				
					}
				}else{
					replacementUnneeded <- replacementUnneeded + 1L
				}
			}
	
		}
	}
	
	
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
	
	# Function that is useful when
	# you see that you need to change the `spp` value for a certain `ref`,
	# but the new `spp` value you are switching to is already in spp.key.
	# In this case, you could enter everything to match what is already in spp.key,
	# but this function just takes your row with ref==Ref,
	# and sets its columns to the values already present in the spp==Spp rows.
	# It also sets the spp column in the ref==Ref row to be Spp.
	ref2spp <- function(Ref, Spp){
		check <- spp.key[,Spp%in%spp]
		if(check){
			
			noSet <- c("ref", "val.src", "tbl.row", "mtch.src", "website","website2","flag")
			all.but.noSet <- names(spp.key)[!names(spp.key)%in%noSet]
			stopifnot(all(sapply(spp.key[spp==Spp,eval(s2c(all.but.noSet))], function(x)length(unique(x))<=1)))
			
			new.vals <- spp.key[spp==Spp,eval(s2c(all.but.noSet))]	
			setkey(new.vals, spp)
			new.vals <- unique(new.vals)
			
			spp.key[ref==Ref, c(all.but.noSet):=new.vals]	
		}else{
			# if the corrected name doesn't already exist,
			# then simply switch the wrong name to the corrected name,
			spp.key[ref==Ref, spp:=Spp]
		}
	}
	
	# When a column of a data.frame has 2 unique values,
	# an NA, and a non-NA, this function will set all NA values to the non-NA value.
	# Z is the name of a full data.table, and index is a vector of TRUE/FALSE 
	# indicating which rows of Z are supposed to be examined
	# The use-case in mind is where Z is a full data.table,
	# and index indicates a portion of the data.table that is supposed to be compared.
	# It's done this way to avoid having to subset the data.table in advance,
	# that way the replacements can easily be made "in place", thus not soaking up extra memory.
	# Similarly, note that thus function changes the data.table passed to the Z argument ... 
	# there is no output, but the original data.table will be affected.
	set2nonNA <- function(Z, index){
		tn <- names(Z)
		to.set <- sapply(Z[index], function(x)any(is.na(x)) & lu(x[!is.na(x)])==1)
		c.to.set <- tn[to.set]
		if(length(c.to.set)>=1){
			for(i in 1:length(c.to.set)){
				set.vals <- Z[index, unique(eval(s2c(c.to.set[i]))[[1]])]
				set.val <- set.vals[!is.na(set.vals)]
				stopifnot(class(set.val)==Z[index,class(eval(s2c(c.to.set[i]))[[1]])])
				Z[index, c(c.to.set[i]):=list(set.val)]
			}
		}
		
	}
	
	check.and.set(wrong="Moira atropus", corrected="Moira atropos")
	spp.key[spp=="Moira atropos",
		':='(
			taxLvl="species",
			species="Moira atropos",
			genus="Moira",
			website="http://www.marinespecies.org/echinoidea/aphia.php?p=taxdetails&id=158067",
			flag="manual"
		)
	]
	
	# spp.key[spp=="Astrea Orbicella", # couldn't find this one
	# ]
	
	# check.and.set(wrong="Bathynectes superba", corrected="Bathynectes maravigna")
	# spp.key[spp=="Bathynectes maravigna",
	# 	':='(
	# 		taxLvl="species",
	# 		species="Bathynectes maravigna",
	# 		genus="Bathynectes",
	# 		website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=107377"
	#
	# 	)
	# ]
	
	check.and.set(wrong="Centropristis ocyurus", corrected="Centropristis ocyurus")
	spp.key[spp=="Centropristis ocyurus",
		':='(
			taxLvl="species",
			species="Centropristis ocyurus",
			genus="Centropristis",
			common="Bank sea bass", 
			Picture="y", 
			trophicLevel=3.5, 
			trophicLevel.se=0.53,  
			website="http://www.fishbase.org/summary/3316",
			flag="manual"
		)
	]
	#
	# check.and.set(wrong="Glyphocrangon aculeata", corrected="Glyphocrangon aculeata")
	# spp.key[spp=="Glyphocrangon aculeata",
	# 	':='(
	# 		taxLvl="species",
	# 		species="Glyphocrangon aculeata",
	# 		genus="Glyphocrangon",
	# 		website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=421812"
	#
	# 	)
	# ]
	#
	# check.and.set(wrong="Lycoteuthis diadema", corrected="Lycoteuthis lorigera")
# 	spp.key[spp=="Lycoteuthis lorigera",
# 		':='(
# 			taxLvl="species",
# 			species="Lycoteuthis lorigera",
# 			genus="Lycoteuthis",
# 			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=342361"
#
# 		)
# 	]
	
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
			website="http://www.fishbase.org/summary/Mustelus-canis.html",
			flag="manual"
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
			website="http://www.fishbase.org/summary/12454",
			flag="manual"
		)
	]
	
	
	
	spp.key[spp=="Synagrops bellus",
		':='(
			taxLvl="species",
			species="Synagrops bellus",
			genus="Synagrops",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=159584",
			flag="manual"
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
			website="http://www.fishbase.org/summary/5059",
			flag="manual"

		)
	]
	
	
	spp.key[ref=="EZUMIA BAIRDII",
		':='(
			spp="Nezumia bairdii",
			taxLvl="species",
			genus="Nezumia",
			species="Nezumia bairdii",
			trophicLevel=3.3,
			trophicLevel.se=0.1,
			Picture="y",
			common="Marlin=spike",
			website="http://www.fishbase.org/summary/3104",
			flag="manual"
		)
	
	]
	
	
	spp.key[ref=="PORTUNUS FLORIDANUS",
		':='(
			spp="Achelous floridanus",
			taxLvl="species",
			species="Achelous floridanus",
			genus="Achelous",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=557882",
			flag="manual"
			
		)
	
	]
	
	
	spp.key[ref=="ASTROPECTE COMPTUS",
		':='(
			spp="Astropecten comptus",
			taxLvl="species",
			genus="Astropecten",
			species="Astropecten comptus",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=178649",
			flag="manual"
			
		)
	
	]
	
	spp.key[ref=="BATHYPTEROIS BIGELOWI",
		':='(
			spp="Bathypterois bigelowi",
			taxLvl="species",
			genus="Bathypterois",
			species="Bathypterois bigelowi",
			website="http://www.fishbase.org/summary/Bathypterois-bigelowi.html",
			Picture="y",
			flag="manual"
			
		)
	]
	
	spp.key[ref=="PENNATULA BOREALIS",
		':='(
			spp="Pennatula grandis",
			taxLvl="species",
			genus="Pennatula",
			species="Pennatula grandis",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=128516",
			flag="manual"
			
		)
	]
	
	
	spp.key[spp=="Bollmannia communis",
		':='(
			spp="Bollmannia communis",
			taxLvl="species",
			genus="Bollmannia",
			species="Bollmannia communis",
			flag="manual"
		)
	]
	
	
	
	
	spp.key[ref=="Brisaster townsendi",
		':='(
			spp="Brisaster townsendi",
			taxLvl="species",
			genus="Brisaster",
			species="Brisaster townsendi",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=513144",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="Buccinum transliratum",
		':='(
			spp="Buccinum angulosum",
			common="angular whelk",
			family="Buccinidae",
			order="Neogastropoda",
			class="Gastropoda",
			phylum="Mollusca",
			kingdom="Animalia",
			taxLvl="species",
			genus="Buccinum",
			species="Buccinum angulosum",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=138858",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="CHILOMYCTERUS ATINGA",
		':='(
			spp="Chilomycterus reticulatus",
			common="spotfin burrfish",
			taxLvl="species",
			genus="Chilomycterus",
			species="Chilomycterus reticulatus",
			trophicLevel=3.45, # should just be 3.5?
			trophicLevel.se=0.41,
			Picture="y",
			website="http://www.fishbase.org/summary/Chilomycterus-reticulatus.html",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=403539",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="CHLAMYS SETIS",
		':='(
			spp="Caribachlamys sentis",
			taxLvl="species",
			genus="Caribachlamys",
			species="Caribachlamys sentis",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=393711",
			flag="manual"
		)
	]
	
	spp.key[ref=="Colus hypolispus",
		':='(
			spp="Latisipho hypolispus",
			taxLvl="species",
			genus="Latisipho",
			species="Latisipho hypolispus",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=491062",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=254476",
			flag="manual"
			
		)
	]
	
	spp.key[ref=="CYCLOTHONE BRAUERI",
		':='(
			spp="Cyclothone braueri",
			taxLvl="species",
			common="Garrick",
			genus="Cyclothone",
			species="Cyclothone braueri",
			family="Gonostomatidae",
			order="Stomiiformes",
			class="Actinopterygii",
			trophicLevel=3.1,
			trophicLevel.se=0.18,
			Picture="y",
			website="http://www.fishbase.de/summary/Cyclothone-braueri.html",
			flag="manual"
		)
	]
	
	
	
	
	spp.key[ref=="DIAPHUS SPLEDIDUS",
		':='(
			spp="Diaphus splendidus",
			taxLvl="species",
			common="Horned lanternfish",
			genus="Diaphus",
			species="Diaphus splendidus",
			family="Myctophidae",
			order="Myctophiformes",
			class="Actinopterygii",
			trophicLevel=3.0,
			trophicLevel.se=0,
			Picture="y",
			website="http://www.fishbase.org/summary/10174",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="DISTORSIO MCGITYI",
		':='(
			spp="Distorsio constricta mcgintyi",
			taxLvl="species",
			genus="Distorsio",
			species="Distorsio constricta",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=422768",
			flag="manual"
			
		)
	]
	
	
	spp.key[ref=="LEPIDOPA BEEDICTI",
		':='(
			spp="Lepidopa benedicti",
			genus="Lepidopa",
			species="Lepidopa benedicti",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=421881",
			flag="manual"
		)
	
	]
	
	
	spp.key[ref=="EPINEPHELUS NIVEATUS",
		':='(
			spp="Hyporthodus niveatus",
			genus="Hyporthodus",
			species="Hyporthodus niveatus",
			common="snowy grouper",
			family="Serranidae",
			order="Perciformes",
			class="Actinopteri",
			superclass="Osteichthyes",
			subphylum="Vertebrata",
			phylum="Chordata",
			kingdom="Animalia",
			trophicDiet="n",
			trophicOrig="y",
			taxLvl="species",
			trophicLevel=4.04,
			trophicLevel.se=0.58,
			Picture="y",
			flag="manual"
		)
	
	]
	
	
	spp.key[ref=="EQUETUS ACUMINATUS",
		':='(
			spp="Pareques acuminatus",
			genus="Pareques",
			species="Pareques acuminatus",
			taxLvl="species",
			common="high-hat",
			family="Sciaenidae",
			order="Perciformes",
			class="Actinopterygii",
			superclass="Osteichthyes",
			subphylum="Vertebrata",
			phylum="Chordata",
			kingdom="Animalia",
			trophicDiet="y",
			trophicOrig="y",
			Picture="y",
			trophicLevel=3.59,
			trophicLevel.se=0.46,
			flag="manual"
		)
	
	]
	
	
	
	spp.key[ref=="ETMOPTERUS VIRES",
		':='(
			spp="Etmopterus virens",
			genus="Etmopterus",
			species="Etmopterus virens",
			common="green lantern shark",
			trophicLevel=4.2,
			trophicLevel.se=0.73,
			Picture="y",
			website="http://www.fishbase.org/summary/690",
			flag="manual"
			
		)
	
	]
	
	
	spp.key[ref=="LUIDIA ELEGAS",
		':='(
			spp="Luidia sarsii elegans",
			genus="Luidia",
			species="Luidia sarsii",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=752124",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=368112",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="HALICHOERES RADIATUS",
		':='(
			spp="Halichoeres radiatus",
			genus="Halichoeres",
			species="Halichoeres radiatus",
			taxLvl="species",
			common="Puddingwife wrasse",
			trophicLevel=3.5,
			trophicLevel.se=0.1,
			Picture="y",
			website="http://www.fishbase.org/summary/1068",
			flag="manual"
		)
	]
	
	spp.key[ref=="Henricia aleutica",
		':='(
			spp="Henricia longispina aleutica",
			genus="Henricia",
			species="Henricia longispina",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=369124",
			flag="manual"
		)
	]
	
	
	
	ref2spp(Ref="ALUTERUS HEUDELOTTI", Spp="Aluterus heudelotii")
	ref2spp(Ref="ALUTERUS HEUDELOTI", Spp="Aluterus heudelotii")
	spp.key[spp=="Aluterus heudelotii",
		':='(
			common="Dotterel filefish",
			taxLvl="species",
			genus="Aluterus",
			species="Aluterus heudelotii",
			family="Monacanthidae",
			order="Tetraodontiformes",
			class="Actinopteri",
			superclass="Actinopterygii",
			subphylum="Craniata",
			phylum="Chordata",
			kingdom="Metazoa",
			Picture="y",
			trophicLevel=2.0,
			trophicLevel.se=0,
			website="http://www.fishbase.org/summary/4273",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="ALPHEUS ARMATUS",
		':='(
			spp="Alpheus armatus",
			taxLvl="species",
			genus="Alpheus",
			species="Alpheus armatus",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=421729",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="COOKEOLUS JAPONICUS", Spp="Cookeolus japonicus")
	spp.key[spp=="Cookeolus japonicus",
		':='(
			species="Cookeolus japonicus",
			genus="Cookeolus",
			taxLvl="species",
			common="Longfinned bullseye",
			Picture="y",
			trophicLevel=3.5,
			trophicLevel.se=0.6,
			website="http://www.fishbase.org/summary/3517",
			flag="manual"
			
			)
	]
	
	spp.key[ref=="APRISTURUS IDICUS",
		':='(
			spp="Apristurus indicus",
			species="Apristurus indicus",
			genus="Apristurus",
			taxLvl="species",
			common="Smallbelly catshark",
			trophicLevel=3.7,
			trophicLevel.se=0.3,
			Picture="y",
			website="http://www.fishbase.org/Summary/speciesSummary.php?ID=766&AT=Kleinbuikkathaai",
			flag="manual"
		)
	]
	
	spp.key[ref=="Arctomelon sp. cf. stearnsii (Clark & McLean)",
		':='(
			spp="Arctomelon stearnsii",
			species="Arctomelon stearnsii",
			genus="Arctomelon",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=384681",
			flag="manual"
		)
	]
	
	spp.key[ref=="ASTARTE GLOBULA",
		':='(
			spp="Astarte globula",
			species="Astarte globula",
			genus="Astarte",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=420839",
			flag="manual"
		)
	]
	
	spp.key[ref=="BUSYCO CARICA",
		':='(
			spp="Busycon carica",
			genus="Busycon",
			species="Busycon carica",
			taxLvl="species",
			Picture="y",
			common="Knobbed whelk",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=160185",
			website2="http://www.sealifebase.org/summary/Busycon-carica.html",
			flag="manual"
		)
	]
	
	spp.key[ref=="CALAMUS PENNA" | ref=="CALAMUS PEA",
		':='(
			spp="Cabdio penna",
			genus="Cabdio",
			species="Cabdio penna",
			taxLvl="species",
			common="Sheepshead porgy",
			Picture="y",
			trophicLevel=4.4,
			trophicLevel.se=0.2
			website="http://www.fishbase.org/summary/1227",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=159245",
			flag="manual"
		)
	]
	
	spp.key[ref=="CALLIECTES DAAE",
		spp="Callinectes danae",
		genus="Callinectes",
		species="Callinectes danae",
		taxLvl="species",
		common="Dana swimming crab",
		trophicLevel=3.63,
		trophicLevel.se=0.43,
		Picture="y",
		website="http://www.sealifebase.org/summary/Callinectes-danae.html",
		website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=107378"
	]
	
	
	
	# spp.key[ref=="Buccinum triplostephanum"]
# 	spp.key[spp=="Volutopsion castaneum"]
	ref2spp(Ref="Buccinum triplostephanum", Spp="Volutopsion castaneum")
	spp.key[spp=="Volutopsion castaneum",
		':='(
			species="Volutopsion castaneum",
			genus="Volutopsion",
			taxLvl="species",
			common="chestnut whelk",
			website="http://arctos.database.museum/name/Buccinum%20castaneum%20triplostephanum",
			website2="http://www.sealifebase.org/summary/Volutopsion-castaneum.html",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="BUSYCO CAALICULATUM",
		spp="Busycotypus canaliculatus",
		species="Busycotypus canaliculatus",
		genus="Busycotypus",
		taxLvl="species",
		common="Channeled whelk",
		Picture="y",
		trophicLevel=3.23,
		trophicLevel.se=0.17,
		website="http://arctos.database.museum/name/Busycon%20canaliculatum",
		website2="http://www.sealifebase.org/summary/Busycotypus-canaliculatus.html",
		flag="manual"
	]
	
	spp.key[ref=="BUSYCO SIISTRUM",
		':='(
			spp="Sinistrofulgur sinistrum",
			species="Sinistrofulgur sinistrum",
			genus="Sinistrofulgur",
			taxLvl="species",
			common="Lightning whelk",
			Picture="y",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=160189",
			website2="http://sealifebase.org/summary/Busycon-sinistrum.html",
			
		)
	]
	
	
	ref2spp(Ref="CALAPPA AGUSTA", Spp="Hepatus pudibundus")
	spp.key[spp=="Hepatus pudibundus", # the correct species name should already be in spp.key
		':='(
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=452047",
			website2="http://www.sealifebase.org/summary/Hepatus-pudibundus.html"
		)
	]
	
	
	spp.key[ref=="GOOSTOMA ATLATICUM",
		':='(
			spp="Gonostoma atlanticum",
			species="Gonostoma atlanticum",
			genus="Gonostoma",
			common="Atlantic fangjaw",
			taxLvl="species",
			trophicLevel=3.0,
			trophicLevel.se=0.13,
			Picture="y",
			website="http://www.fishbase.org/summary/5052",
			flag="manual"
		)
	]
	
	ref2spp(Ref="Careproctus sp. cf. rastrinus (Orr et al.)", Spp="Careproctus rastrinus")
	spp.key[ref=="Careproctus sp. cf. rastrinus (Orr et al.)",
		':='(
			flag="manual"
		)
	]
	spp.key[spp=="Careproctus rastrinus".
		Picture="y",
		website="http://www.fishbase.org/summary/Careproctus-rastrinus.html",
	]
	
	
	ref2spp(Ref="Careproctus sp. cf. gilberti (Orr)", Spp="Careproctus gilberti")
	spp.key[ref=="Careproctus sp. cf. gilberti (Orr)",
		':='(
			flag="manual"
		)
	]
	spp.key[spp=="Careproctus gilberti"
		':='(
			trophicLevel=3.2,
			trophicLevel.se=0.5,
			Picture="y",
			website="http://www.fishbase.org/summary/25161"
		)
	]
	
	spp.key[ref=="Careproctus lycopersicus",
		':='(
			spp="Careproctus lycopersicus",
			species="Careproctus lycopersicus",
			genus="Careproctus",
			common="tomato snailfish",
			taxLvl="species",
			trophicLevel=3.2,
			trophicLevel.se=0.5,
			website="http://www.fishbase.org/summary/67403",
			flag="manual"
		)
	]
	
	spp.key[ref=="COUS DAUCUS",
		':='(
			spp="Conus daucus",
			species="Conus daucus",
			genus="Conus",
			taxLvl="species",
			common="carrot cone",
			Picture="y",
			website="http://www.sealifebase.org/summary/Conus-daucus.html",
			flag="manual"
		)
	]
	
	spp.key[ref=="ICTALURUS FURCATUS", # freshwater/ brackish species!!
		':='(
			spp="Ictalurus furcatus",
			species="Ictalurus furcatus",
			genus="Ictalurus",
			taxLvl="species",
			common="blue catfish",
			trophicLevel=3.4,
			trophicLevel.se=0.44,
			Picture="y",
			website="http://www.fishbase.org/summary/3019",
			flag="manual"
		)
	]
	
	spp.key[ref=="CYPSELURUS MELANURUS",
		':='(
			spp="Cheilopogon melanurus",
			species="Cheilopogon melanurus",
			genus="Cheilopogon",
			taxLvl="species",
			Picture="y",
			trophicLevel=3.6,
			trophicLevel.se=0.5,
			website="http://www.fishbase.org/summary/cheilopogon-melanurus.html",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=312468",
			flag="manual"
		)
	]
	
	ref2spp(Ref="Chrysaora fuscens", Spp="Chrysaora fuscescens")
	spp.key[ref=="Chrysaora fuscens",
		':='(
			spp="Chrysaora fuscescens",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=287206",
			flag="manual"
		)
	]
	
	spp.key[ref=="LIMA SCABRA",
		':='(
			spp="Ctenoides scaber",
			genus="Ctenoides",
			species="Ctenoides scaber",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=420747",
			website2="http://www.sealifebase.org/summary/Ctenoides-scaber.html",
			Picture="y",
			flag="manual"
		)
	]
	
	spp.key[ref=="PIOTHERES MACULATUS",
		':='(
			spp="Tumidotheres maculatus",
			genus="Tumidotheres",
			species="Tumidotheres maculatus",
			common="squatter pea crab",
			taxLvl="species",
			Picture="y",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=454715",
			website2="http://sealifebase.org/summary/Tumidotheres-maculatus.html",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="DROMIDIA ATILLESIS",
		spp="Moreiradromia antillensis",
		species="Moreiradromia antillensis",
		genus="Moreiradromia",
		common="sponge crab",
		taxLvl="species"
		Picture="y",
		website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=241025",
		flag="manual"
	]
	
	spp.key[ref=="MICROPHRYS ANTILLENSIS",
		':='(
			spp="Microphrys antillensis",
			species="Microphrys antillensis",
			genus="Microphrys",
			taxLvl="species",
			common="lobed decorator crab",
			website="http://www.sealifebase.org/summary/Microphrys-antillensis.html",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="Cyclocardia sp. cf. borealis (Clark 2006)",
		':='(
			spp="Cyclocardia borealis",
			species="Cyclocardia borealis",
			genus="Cyclocardia",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=156832",
			flag="manual"
		)
	]
	
	spp.key[ref=="EPINEPHELUS MYSTACINUS",
		':='(
			spp="Hyporthodus mystacinus",
			species="Hyporthodus mystacinus",
			genus="Hyporthodus",
			taxLvl="species",
			common="misty grouper",
			Picture="y",
			trophicLevel=4.6,
			trophicLevel.se=0,
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=273861",
			website2="http://www.fishbase.org/summary/Hyporthodus-mystacinus.html",
			flag="manual"
		)
	]
	
	spp.key[ref=="ETMOPTERUS BULLISI",
		':='(
			spp="Etmopterus bullisi",
			species="Etmopterus bullisi",
			genus="Etmopterus",
			taxLvl="species",
			common="lined lanternshark",
			trophicLevel=4.2,
			trophicLevel.se=0.4,
			Picture="y",
			website="http://www.fishbase.org/summary/676",
			flag="manual"
		)
	]
	
	spp.key[ref=="GOBIOSOMA HORSTI",
		':='(
			spp="Elacatinus horsti",
			species="Elacatinus horsti",
			genus="Elacatinus",
			taxLvl="species",
			common="yellowline goby",
			Picture="y",
			trophicLevel=3.5,
			trophicLevel.se=0.4,
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=309575"
			website2="http://www.fishbase.org/summary/3873",
			flag="manual"
		)
	]
	
	ref2spp(Ref="Gorgonocephalus sp. cf. arcticus", Spp="Gorgonocephalus arcticus")
	spp.key[ref=="Gorgonocephalus sp. cf. arcticus",
		':='(
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=124966",
			flag="manual"
		)
	]
	
	
	spp.key[ref=="GORGONOCEPHALUS LAMARCKI",
		':='(
			spp="Gorgonocephalus lamarckii",
			species="Gorgonocephalus lamarckii",
			genus="Gorgonocephalus",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=416670",
			flag="manual"
		)
	
	]
	
	ref2spp(Ref="MONACANTHUS CILIATUS", Spp="Monacanthus ciliatus")
	spp.key[ref=="MONACANTHUS CILIATUS",
		website="http://www.fishbase.org/summary/4280",
		flag="manual"
	
	]
	
	# spp.key[!is.na(spp) & !is.na(species) & taxLvl=="species"] # these are probably the good ones
	
	save(spp.key, file="data/spp.key.RData")
	write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)
	
	return(spp.key)
}

