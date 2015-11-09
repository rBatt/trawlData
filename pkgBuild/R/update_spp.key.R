
update_spp.key <- function(...){
	# Read in CSV
	spp.key <- fread("inst/extdata/taxonomy/spp.key.csv", na.strings=c("","NA"))

	# make sure encoding is ASCII
	makeAsciiChar(spp.key)

	# set data.table key (sorts)
	setkey(spp.key, spp, ref)
	
	# save both .RData and .csv
	save(spp.key, file="data/spp.key.RData")
	write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)
	
	invisible(NULL)
}
