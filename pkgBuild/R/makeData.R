makeData <- function(regions, raw.read=F, raw.save=raw.read, raw.load=!raw.read, clean=F, clean.save=clean, clean.load=!clean, trimR=T, trimC=T){
	
	if(missing(regions)){
		regions <- c("ai", "ebs", "gmex", "goa", "neus", "newf", "sa", "sgulf", "shelf", "wcann", "wctri")
	}
	

	# ==================
	# = Raw Read/ Save =
	# ==================
	if(raw.read){
		# read in raw
		for(i in 1:length(regions)){
			nm <- paste0("raw.", regions[i])
			assign(nm, read.trawl(regions[i]))
			if(raw.save){
				save(list=nm, file=paste0("data/",nm,".RData"), compress="xz")
			}
			
		}
	}
	


	# ============
	# = Load Raw =
	# ============
	if(raw.load){
		# load raw
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- paste0("raw.", regions[i])
			load(file=paste0("data/",nm,".RData"))
			setTxtProgressBar(pb, i)
		}
	}
	


	# ============
	# = Cleaning =
	# ============
	if(clean){
		# clean up column names
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- paste0("raw.", regions[i])
			assign(regions[i], copy(get(nm)))
			clean.names(get(regions[i]), regions[i])
			setTxtProgressBar(pb, i)
		}

		# format column values
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- regions[i]
			clean.format(get(nm), nm)
			setTxtProgressBar(pb, i)
		}

		# clean column content, add columns
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- regions[i]
			clean.columns(get(nm), nm)
			setTxtProgressBar(pb, i)
		}
		
		# clean taxa names
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- regions[i]
			assign(nm, clean.tax(get(nm), nm))
			setTxtProgressBar(pb, i)
		}

		# add column for row trimming
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- regions[i]
			clean.trimRow(get(nm), nm)
			setTxtProgressBar(pb, i)
		}
	}
	
	


	# ==============
	# = Save Clean =
	# ==============
	if(clean.save){
		# save clean
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- paste0("clean.", regions[i])
			assign(nm, copy(get(regions[i])))
			save(list=nm, file=paste0("data/",nm,".RData"), compress="xz")
			setTxtProgressBar(pb, i)
		}
	}
	


	# ==============
	# = Load Clean =
	# ==============
	if(clean.load){
		# load clean
		pb <- txtProgressBar(min=1, max=length(regions), style=3)
		for(i in 1:length(regions)){
			nm <- paste0("clean.", regions[i])
			load(file=paste0("data/",nm,".RData"))
			setTxtProgressBar(pb, i)
		}
	}
	


	# ======================================
	# = Rename clean.REGION as just REGION =
	# ======================================
	# rename clean, rm old
	pb <- txtProgressBar(min=1, max=length(regions), style=3)
	for(i in 1:length(regions)){
		nm <- paste0("clean.", regions[i])
		assign(regions[i], copy(get(nm)))
		rm(list=nm)
		setTxtProgressBar(pb, i)
	}



	# cnames <- sort(unique(c(names(ai), names(ebs), names(gmex), names(goa), names(neus), names(newf), names(sa), names(sgulf), names(shelf), names(wcann), names(wctri))))
	
	# spp <- sort(unique(c(ai[,unique(spp)], ebs[,unique(spp)], gmex[,unique(spp)], goa[,unique(spp)], neus[,unique(spp)], newf[,unique(spp)], sa[,unique(spp)], sgulf[,unique(spp)], shelf[,unique(spp)], wcann[,unique(spp)], wctri[,unique(spp)]))) # use this in creat.spp.key.R

	reg.list <- list(ai, ebs, gmex, goa, neus, newf, sa, sgulf, shelf, wcann, wctri)
	
	
	return(reg.list)

}