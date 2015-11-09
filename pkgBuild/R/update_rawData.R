

update_rawData <- function(reg, ...){	
	
	reg.p <- gsub("(\\.RData|\\.csv|raw\\.|-.*|\\.zip)", "", basename(reg[1]), perl=T)
	
	makeData(regions=reg.p, raw.read=T)
	
}