file.copy("./R/","../trawlData copy", recursive=TRUE)
file.copy("./man/","../trawlData copy", recursive=TRUE)
file.copy("./man-roxygen/","../trawlData copy", recursive=TRUE)
file.copy("./pkgBuild/","../trawlData copy", recursive=TRUE)

otherFiles <- c("DESCRIPTION","NAMESPACE",".Rbuildignore", "README.md")
for(i in 1:length(otherFiles)){
	file.copy(otherFiles[i], "../trawlData copy", overwrite=TRUE)
}

dir.create("../trawlData copy/data")

dataFiles <- list.files("data")
for(i in 1:length(dataFiles)){
	before <- NULL
	before <- ls()
	load(file.path("data",dataFiles[i]))
	# Sys.sleep(0.1)
	New <- setdiff(ls(), before)
	for(j in 1:length(New)){
		assign(New[j],head(get(New[j])))
	}
	# Sys.sleep(0.1)
	do.call(save, list(list=New, file=file.path("../trawlData copy/data",dataFiles[i])))
	# Sys.sleep(0.1)
	rm(list=New)
	# Sys.sleep(0.1)
}



# invisible(sapply(list.files("R", full.names=T), source, .GlobalEnv))