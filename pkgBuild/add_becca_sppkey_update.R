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

spp.key[ref%in%b_ref & (!is.na(spp)|!is.na(trophicLevel)|!is.na(common))] # make sure everything becca looked up was just NA across the board

spp.key <- copy(trawlData::spp.key)
spp.key <- spp.key[!ref%in%b_ref]

spp.key <- rbind(becca, spp.key, fill=TRUE)

# ---- resave ----
makeAsciiChar(spp.key)

# set data.table key (sorts)
setkey(spp.key, spp, ref)

# save both .RData and .csv
save(spp.key, file="data/spp.key.RData")
write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)