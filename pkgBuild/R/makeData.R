makeData <- function(regions, raw.read=F, raw.save=raw.read, raw.load=!raw.read, clean=F, clean.save=clean, clean.load=!clean, trimR=T, trimC=T){
	
	# this function regenerates the R Data files 
	# that contain the trawl data for each region
	# In order to do this, the package cannot be loaded
	# (or, it is safer it is not, because those objects are in the NAMESPACE)
	
	# Thus, there are 3 important things to remember when using this function:
	# 1) set directory to the top of the package
	# 2) unload()
	# 3) This script sources the R scripts in "./R", and uses the spp.key in load("data/spp.key.RData")
	
	# source package scripts
	invisible(sapply(list.files("R", full.names=T), source, .GlobalEnv))

	
	if(missing(regions)){
		regions <- c("ai", "ebs", "gmex", "goa", "neus", "newf", "sa", "sgulf", "shelf", "wcann", "wctri")
	}
	
	
	compressSave <- function(path){
		# first, check to see if this .RData file already exists;
		# if it does, figure out the optimal type of compression for it
		# (this is the optimal compression for what was saved before, not what we're doing now)
		if(file.exists(path)){
			# set the compression type to use based on the optimal compression
			# for the older version of the file
			compress2use <- tools::checkRdaFiles(path)[,"compress"]
		}else{
			# make "xz" default if no file is found
			# "xz" is slow, but usually best compression for these data
			compress2use <- "xz"
		}
		
		# save the current data to the file, using compression
		save(list=nm, file=path, compress=compress2use)
		
		# check to see if what we just saved was optimally compressed
		# if it wasn't resave under optimal compression type
		compression.check <- tools::checkRdaFiles(path)[,"compress"]
		if(compression.check!=compress2use){
			tools::resaveRdaFiles(path, compress=compression.check)
		}
	}
	
	
	
	# ==================
	# = Raw Read/ Save =
	# ==================
	if(raw.read){
		# read in raw
		message("\nReading raw trawl data (as received by trawlData package creators from data providers)\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			nm <- paste0("raw.", regions[i])
			assign(nm, read.trawl(regions[i]))
			
			# ensure encoding is ASCII
			makeAsciiChar(get(nm))
			
			
			# If desired, save, and do so with optimal compression
			if(raw.save){
				compressSave(paste0("data/",nm,".RData"))
			}
			
			if(length(regions)>1){setTxtProgressBar(pb, i)}
			
		}
		
		if(length(regions)>1){close(pb)}
	}
	
	

	# ============
	# = Load Raw =
	# ============
	if(raw.load){
		# load raw
		message("Loading raw (.RData) region data")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			nm <- paste0("raw.", regions[i])
			load(file=paste0("data/",nm,".RData"))
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
		
		if(length(regions)>1){close(pb)}
	}
	
	
	
	# ============
	# = Cleaning =
	# ============
	if(clean){
		stopifnot(all(paste0("raw.",regions)%in%ls()))
		
		# create clean region objects
		cnms <- c()
		for(i in 1:length(regions)){
			cnms[i] <- paste0("clean.",regions[i])
			assign(cnms[i], copy(get(nm)))
		}
		
		# clean up column names
		message("\nCleaning column names\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			clean.names(get(cnms[i]), regions[i])
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
		if(length(regions)>1){close(pb)}

		# format column values
		message("\nFormatting column values (typos, basic formatting)\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			clean.format(get(cnms[i]), regions[i])
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
		if(length(regions)>1){close(pb)}

		# clean column content, add columns
		message("\nAdd and polish column content (class, units, add columns)\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			clean.columns(get(cnms[i]), regions[i])
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
		if(length(regions)>1){close(pb)}
		
		# clean taxa names
		message("\nCleaning taxonomy and adding ecology\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		# load("data/spp.key.RData")
		for(i in 1:length(regions)){
			assign(cnms[i], clean.tax(get(cnms[i]), regions[i]))
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
		if(length(regions)>1){close(pb)}

		# add column for row trimming
		message("\nAdd a column to suggest which rows to drop\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			clean.trimRow(get(cnms[i]), regions[i])
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
		if(length(regions)>1){close(pb)}
		
	}
	
	


	# ==============
	# = Save Clean =
	# ==============
	if(clean.save){
		stopifnot(clean)
		# save clean
		message("\nSaving cleaned region data\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			
			# ensure encoding is ASCII
			makeAsciiChar(get(cnms[i]))
			
			# save
			compressSave(paste0("data/",nm,".RData"))
			
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
	}
	
	
	
	# ==============
	# = Load Clean =
	# ==============
	if(clean.load){
		# load clean
		message("\nLoading clean region data\n")
		if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
		for(i in 1:length(regions)){
			cnm <- paste0("clean.", regions[i])
			load(file=paste0("data/",cnm,".RData"))
			if(length(regions)>1){setTxtProgressBar(pb, i)}
		}
	}
	
	
	
	# ======================================
	# = Rename clean.REGION as just REGION =
	# ======================================
	# rename clean, rm old
	# message("\nRenaming region objects in workspace (dropping 'clean.' prefix)\n")
	# if(length(regions)>1){pb <- txtProgressBar(min=1, max=length(regions), style=3)}
	# for(i in 1:length(regions)){
	# 	nm <- paste0("clean.", regions[i])
	# 	assign(regions[i], copy(get(nm)))
	# 	rm(list=nm)
	# 	if(length(regions)>1){setTxtProgressBar(pb, i)}
	# }



	# cnames <- sort(unique(
# 		c(
# 			names(ai),names(raw.ai), names(clean.ai),
# 			names(ebs), names(raw.ebs), names(clean.ebs),
# 			names(gmex), names(raw.gmex), names(clean.gmex),
# 			names(goa), names(raw.goa), names(clean.goa),
# 			names(neus), names(raw.neus), names(clean.neus),
# 			names(newf), names(raw.newf), names(clean.newf),
# 			names(sa), names(raw.sa), names(clean.sa),
# 			names(sgulf), names(raw.sgulf), names(clean.sgulf),
# 			names(shelf), names(raw.shelf), names(clean.shelf),
# 			names(wcann),names(raw.wcann), names(clean.wcann),
# 			names(wctri), names(raw.wctri), names(clean.wctri)
# 		 )
# 	))
	
	# spp <- sort(unique(c(ai[,unique(spp)], ebs[,unique(spp)], gmex[,unique(spp)], goa[,unique(spp)], neus[,unique(spp)], newf[,unique(spp)], sa[,unique(spp)], sgulf[,unique(spp)], shelf[,unique(spp)], wcann[,unique(spp)], wctri[,unique(spp)]))) # use this in creat.spp.key.R

	# reg.list <- list(ai, ebs, gmex, goa, neus, newf, sa, sgulf, shelf, wcann, wctri)
	
	
	# return(reg.list)

}