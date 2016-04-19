library(trawlData)

becca <- fread("~/Downloads/new.spp.forworms_matched (1).csv")

becca <- becca[!is.na(AphiaID)]
becca[taxLvl=="species", species:=spp]
becca[,flag:="becca_batch2"]

sk_names <- names(trawlData::spp.key)

becca <- becca[,eval(s2c(sk_names[sk_names%in%names(becca)]))]

for(i in names(becca)){
	becca[get(i)=="",i:=NA, with=FALSE]
}
b_ref <- becca[,ref]

spp.key <- copy(trawlData::spp.key)
spp.key <- spp.key[!ref%in%b_ref]

rbind(becca, spp.key, fill=TRUE)