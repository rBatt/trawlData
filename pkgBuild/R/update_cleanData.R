

update_cleanData <- function(reg, ...){	
	message(paste0("received reg as: ",reg))
	reg.p <- gsub("(\\.RData|\\.csv|raw\\.|-.*)", "", basename(reg[1]), perl=T)
	message(paste0("stripped to ",reg.p))
	
	makeData(regions=reg.p, raw.load=T, clean=T)
	
}