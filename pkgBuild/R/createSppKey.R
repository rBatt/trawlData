createSppKey <- function(new_ref, save.key=FALSE){
	load("data/taxInfo.RData")
	load("data/spp.corr1.RData")
	load("data/getSppData.RData")
	load("data/getTaxData.RData")
	load("data/getCmmnData.RData")
	
	# X.match is from taxInfo
	X.match <- match.tbl(ref=new_ref, tbl.ref=taxInfo[,raw.spp], tbl.val=taxInfo[,spp])
	X.match[,mtch.src:=1]
	
	# X.match2 is from the spp.corr1.RData (from taxize)
	X.match2 <- match.tbl(ref=new_ref, tbl.ref=spp.corr1[,spp], tbl.val=spp.corr1[,sppCorr])
	X.match2[,mtch.src:=2]
	
	# X.match3 is from getSppData.RData (more recent taxize on more taxa)
	X.match3 <- match.tbl(ref=new_ref,tbl.ref=getSppData[,spp],tbl.val=getSppData[,sppCorr], exact=T)
	X.match3[,mtch.src:=3]
	
	
	# Combine the 3 sources for ref -> spp conversions
	# Gives preference to non-na values in X.match > X.match2 > X.match3
	new_spp.key00 <- rbind(X.match[!is.na(val)], X.match2[!is.na(val)], X.match3[!is.na(val)])
	spp.noMatch <- X.match[!ref%in%new_spp.key00[,ref], ref]
	X.noMatch <- data.table(
		ref = spp.noMatch,
		val = NA_character_,
		val.src = NA_character_,
		tbl.row = NA_real_,
		mtch.src = NA_real_
	)
	new_spp.key0 <- rbind(new_spp.key00, X.noMatch)
	setorder(new_spp.key0, ref, val.src, mtch.src, val, na.last=T)
	
	
	# Pick which source's rows will be used for each spp
	new_spp.key <- new_spp.key0[!duplicated(ref)] #unique(new_spp.key0) # unique drops out 
	setnames(new_spp.key, "val", "spp")
	setkey(new_spp.key, spp)
	
	
	# prepare to merge tax info into the
	# current species key,
	# which only contains ref (raw) and spp (tax id) columns
	ti2 <- copy(taxInfo)
	setkey(ti2, spp)
	ti2 <- unique(ti2)
	ti2[,raw.spp:=NULL]
	ti2[,correctSpp:=NULL]
	ti2[,isSpecies:=NULL]
	ti2[,taxLvl:=tolower(taxLvl)]
	ti2[,tax.src:="taxInfo"]
	
	
	# merge tax info into species key
	new_spp.key <- merge(new_spp.key, ti2, by="spp", all.x=T)
	
	
	# set the column order for species key
	sk.colO0 <- c("ref", "val.src", "tbl.row", "mtch.src", "tax.src", "spp", "common", "taxLvl", "species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom", "trophicDiet", "trophicOrig", "Picture", "trophicLevel", "trophicLevel.se")
	sk.colO <- c(sk.colO0,names(new_spp.key)[!names(new_spp.key)%in%sk.colO0])
	setcolorder(new_spp.key, sk.colO)
	
	
	# Go through getTaxData (the bigger taxize database I pulled)
	# and see if it has any information that we can use to fill
	# in some of the missing values in new_spp.key
	# Loop through each column in getTaxData ...
	# then see if that same column exists in new_spp.key;
	# if it does, see if new_spp.key has missing values for a species
	# for which getTaxData has non-missing values
	for(i in 2:length(names(getTaxData))){
		tn <- names(getTaxData)[i]
		if(!tn%in%names(new_spp.key)){
			# If we're trying to add a column that new_spp.key doesn't have, then skip
			next
		}
		
		# temporary (subsets) of getTaxData and new_spp.key
		t.gTD <- getTaxData[,eval(s2c(tn))][[1]]
		t.s.k <- new_spp.key[,eval(s2c(tn))][[1]]
		
		# Indentify the indices for which
		# getTaxData has information that can fill in 
		# the missing values in new_spp.key
		can.help <- new_spp.key[is.na(t.s.k), spp] %in%  getTaxData[!is.na(t.gTD), sppCorr]
		
		# if there are any instances for which
		# getTaxData can fill in a missing value in new_spp.key, 
		if(any(can.help)){
			
			# find those matches using match.tbl
			mt <- match.tbl(
				ref=new_spp.key[can.help,spp], 
				tbl.ref=getTaxData[,sppCorr], 
				tbl.val=getTaxData[,eval(s2c(tn))][[1]], 
				exact=T
			)
			
			# then add those matches into new_spp.key
			new_spp.key[can.help, c(tn, "tax.src2"):=list(mt[,val],"getTaxData")]
			
		}else{
			next
		}
	}
	

	
	# insert non-NA common names where needed
	match.cmmn <- getCmmnData[, match.tbl(new_spp.key[,spp], sppCorr, common, exact=T)]
	needs.cmmn <- new_spp.key[,is.na(common)]
	has.cmmn <- match.cmmn[,!is.na(val)]
	new_spp.key[needs.cmmn&has.cmmn,common:=match.cmmn[needs.cmmn&has.cmmn,val]]

	# Define conflicts in new_spp.key
	conflict_cols <- c("common", "taxLvl", "species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom", "trophicDiet", "trophicOrig", "Picture", "trophicLevel", "trophicLevel.se", "tax.src2")
	new_spp.key[!is.na(spp), conflict:=any(!sapply(.SD[,eval(s2c(conflict_cols))], function(x)lu(x[!is.na(x)])<=1)), by="spp"]
	
	
	# Add flag column if it doesn't exist
	if(!"flag"%in%names(new_spp.key)){
		new_spp.key[,flag:=NA_character_]
	}
	if(!"conflict"%in%names(new_spp.key)){
		new_spp.key[,conflict:=NA_character_]
	}
	
	
	# Loop through conflicts by species, and flag
	# note that this is NOT automated
	if(interactive() & FALSE){
		spp2loop <- new_spp.key[!is.na(spp) & conflict & is.na(flag),unique(spp)]
		for(i in 1:length(spp2loop)){
			t.spp <- spp2loop[i]
			t.index <- new_spp.key[,spp==t.spp & !is.na(spp)]
			print(paste0(t.spp, "; ", i, " of ",length(spp2loop)))
			flag.spp(new_spp.key, t.index)
		}
	}else{
		message("Note: Key portions of creating the species key from scratch can only be completed in an interactive mode!")
	}
	
	
	# Save the species key
	if(save.key){
		new_spp.key[,flag:="added_automatically"]
		spp.key <- rbind(spp.key, new_spp.key, fill=TRUE)
		makeAsciiChar(spp.key)
		setkey(spp.key, spp, ref)
		save(spp.key, file="data/spp.key.RData")
		write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)
	}
	
	# Return spp.key as output
	invisible(new_spp.key)
}

