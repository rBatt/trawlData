

update_cleanData <- function(reg, ...){	
	
	reg.p <- gsub("\\.RData", "", basename(reg), perl=T)	
	
	makeData(regions=reg.p, raw.load=T, clean=T)
	
}