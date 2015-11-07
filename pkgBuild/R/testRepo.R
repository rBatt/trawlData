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
	New <- setdiff(ls(), before)
	for(j in 1:length(New)){
		assign(New[j],head(get(New[j])))
	}
	do.call(save, list(list=New, file=file.path("../trawlData copy/data",dataFiles[i])))
	rm(list=New)
}