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
	
	
	
	
	# spp.key[!is.na(spp) & !is.na(species) & taxLvl=="species"] # these are probably the good ones
	
	save(spp.key, file="data/spp.key.RData")
	write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)
	
	return(spp.key)
}

