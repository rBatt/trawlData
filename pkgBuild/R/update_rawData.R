

update_rawData <- function(reg){	
	# reg = "inst/extdata/sgulf.zip"
	# reg = "inst/extdata/neus/"
	reg.p <- gsub("\\.zip", "", basename(reg), perl=T)
	
	
	makeData(regions=reg.p, raw.read=T)
	
}