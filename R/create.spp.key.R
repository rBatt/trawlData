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
	setorder(spp.key0, ref, mtch.src)
	
	spp.key <- unique(spp.key0[!is.na(val)])
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
	
	return(spp.key)
}

save(spp.key, file="data/spp.key.RData")
write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)