ai <- read.trawl("ai")
clean.names(ai, "ai")

ebs <- read.trawl("ebs")
clean.names(ebs, "ebs")

gmex <- read.trawl("gmex")
clean.names(gmex, "gmex")

goa <- read.trawl("goa")
clean.names(goa, "goa")

neus <- read.trawl("neus")
clean.names(neus, "neus")

newf <- read.trawl("newf")
clean.names(newf, "newf")

sa <- read.trawl("sa")
clean.names(sa, "sa")

sgulf <- read.trawl("sgulf")
clean.names(sgulf, "sgulf")

shelf <- read.trawl("shelf")
clean.names(shelf, "shelf")

wcann <- read.trawl("wcann")
clean.names(wcann, "wcann")

wctri <- read.trawl("wctri")
clean.names(wctri, "wctri")


cnames <- unique(c(names(ai), names(ebs), names(gmex), names(goa), names(neus), names(wcann), names(wctri)))


regions <- c("ai", "ebs", "gmex", "goa", "neus", "newf", "sa", "sgulf", "shelf", "wcann", "wctri")
# read in raw
for(i in 1:length(regions)){
	nm <- paste0("raw.", regions[i])
	assign(nm, read.trawl(regions[i]))
	save(list=nm, file=paste0("data/",nm,".RData"), compress="xz")
}

# load raw
for(i in 1:length(regions)){
	nm <- paste0("raw.", regions[i])
	load(file=paste0("data/",nm,".RData"))
}

# clean up column names
for(i in 1:length(regions)){
	nm <- paste0("raw.", regions[i])
	assign(regions[i], clean.names(get(nm), regions[i]))
}

cnames <- sort(unique(c(names(ai), names(ebs), names(gmex), names(goa), names(neus), names(newf), names(sa), names(sgulf), names(shelf), names(wcann), names(wctri))))