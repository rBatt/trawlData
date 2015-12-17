
update_spp.key <- function(..., check_raw=FALSE){
	load("data/spp.key.RData")
	# Check raw regional objects for new ref names
	# ref is a column with spp names from raw data files
	if(check_raw){
		regs <- c("ai", "ebs", "gmex", "goa", "neus", "newf", "sa", "sgulf", "shelf", "wcann", "wctri")
	
		raw_ref <- c()
		for(i in 1:length(regs)){
			load(file.path("data",paste0("raw.",regs[i],".RData")))
			clean.names(t_reg <- copy(get(paste0("raw.",regs[i]))), reg=regs[i])
			clean.format(t_reg, regs[i])
			raw_ref <- c(raw_ref, clean.tax(t_reg, regs[i])[,unique(ref)[!unique(ref)%in%raw_ref]])
		}
	
		new_ref <- raw_ref[!raw_ref%in%spp.key[,ref] & raw_ref!="" & !is.na(raw_ref)]
	
		if(!length(new_ref)){
			message("createSppKey has not found any new 'ref' in raw regional files; ")
			return(NULL)
		}else{
			message("Updating spp.key with ", length(new_ref), " new 'ref' entries that createSppKey found in raw regional files")
			createSppKey(new_ref, save.key=TRUE)
		}
	}else{
		# Read in CSV
		sk_classes <- structure(c("character", "character", "integer", "integer", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "character", "logical", "character", "character", "character"), .Names = c("ref", "val.src", "tbl.row", "mtch.src", "tax.src", "spp", "common", "taxLvl", "species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom", "trophicDiet", "trophicOrig", "Picture", "trophicLevel", "trophicLevel.se", "tax.src2", "conflict", "flag", "website", "website2"))
		spp.key <- fread("inst/extdata/taxonomy/spp.key.csv", na.strings=c("","NA"), colClasses=sk_classes)

		# make sure encoding is ASCII
		makeAsciiChar(spp.key)

		# set data.table key (sorts)
		setkey(spp.key, spp, ref)
	
		# save both .RData and .csv
		save(spp.key, file="data/spp.key.RData")
		write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)
	
		invisible(NULL)
	}
	

}
