

update_rawData <- function(reg, ...){	
	
	reg.p <- gsub("\\.zip", "", basename(reg), perl=T)
	
	makeData(regions=reg.p, raw.read=T)
	
}