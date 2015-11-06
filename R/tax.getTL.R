
getTL <- function(spp, found=NULL, found.data=NULL){
	data(fishbase)
	fb.names <- fish_names(fish.data)
	findable <- fb.names %in% spp
	
	if(!is.null(findable)){
		needed <- findable & !fb.names%in%found
	}else{
		needed <- findable
	}
	
	print(paste0(lu(spp), " names supplied, ", sum(findable), " findable, of which ", sum(needed), " are needed"))
	
	if(is.null(found.data)){
		out.table <- data.table(spp=spp, trophicLevel=NA_real_, trophicLevel.se=NA_real_)
		setkey(out.table, spp)
	}else{
		stopifnot(all(c("spp","trophicLevel","trophicLevel.se")%in%names(out.table)))
		out.table <- found.data
	}
	
	
	fd.sub <- fish.data[needed]
	
	
	tlvl.pb <- txtProgressBar(min=1, max=length(fd.sub), style=3)
	for(i in 1:length(fd.sub)){
		t.tl <- tryCatch({getTrophicLevel(fd.sub[i], as_table=F)}, error=function(cond)NA_real_)
		t.tl.se <- tryCatch({getTrophicLevel(fd.sub[i], as_table=F, justSE=TRUE)}, error=function(cond)NA_real_)
		t.name <- fb.names[needed][i]
		out.table[t.name, trophicLevel:=t.tl]
		out.table[t.name, trophicLevel.se:=t.tl.se]
		
		setTxtProgressBar(tlvl.pb, i)
	}
	close(tlvl.pb)
	
	return(out.table)
	
}
