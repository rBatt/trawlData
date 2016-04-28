




sk <- copy(spp.key)


# only one word, but is a "species"
c1 <- sk[,!grepl(" ", ref)&taxLvl=="species"&flag!="bad"]
c1 <- c1&!is.na(c1)
check(sk, c1, random=TRUE)
sk1 <- copy(sk)

# be more consistent in common names
sk[(common)=="sea star", common:="sea stars"]
sk[(common)=="A sea cucumber", common:="sea cucumbers"]
sk[(common)=="sea cucumber", common:="sea cucumbers"]
sk[(common)=="Octopus", common:="octopuses"]
sk[(common)=="true whelk", common:="true whelks"]
sk[(common)=="Mussels", common:="mussels"]
sk[(common)=="bivalve", common:="bivalves"]


# common name belongs to multiple spp
# get refs2check from refs_checkFirst.R
setorderv(sk, c("common","ref","spp"))
generic_common <- c("sea cucumbers", "barnacles", "bivalves","eels", "lampreys", "mussels", "mackerels", "octopuses", "scallops", "sea fir", "sea stars", "shrimp", "sea anemone", "sea pens", "sea urchin", "sponge", "squids", "true whelks", "soft corals", "hermit crabs","amphipods","nudibranch","ribbonfishes","polychaete worm","true soft corals", "mud crabs", "eelpouts", "ark")
mltpl_common <- sk[,((common)%in%sk[!is.na(common) & !is.na(spp),list(n_spp=lu(spp)), by="common"][n_spp>1, common])]
c2 <- sk[,ref%in%refs2check & !flag%in%c("bad","ok", "fine", "check") & !(common)%in%generic_common & mltpl_common, ]
c2 <- c2&!is.na(c2)
check(sk, c2)
setorderv(sk, c("ref","spp"))
sk2 <- copy(sk)

# common contains non- A-Za-z chars
c3 <- sk[,grepl("[^a-zA-Z' -]", common)]
c3 <- c3&!is.na(c3)
check(sk, c3)
sk3 <- copy(sk)

# species is NA, but common name isn't, and isn't flagged
# need to write a command to take a spp and copy it into species, change taxLvl to 'species', take the first word of spp and put it into genus, and change the flag to check
# need to write another command to set common to NA and set flag to check
c4 <- sk[,is.na(species) & (!is.na(common)&!common%in%generic_common) & (is.na(flag) | (!flag%in%c("bad","check","JM")))]
c4 <- c4&!is.na(c4)
check(sk, c4)
sk4 <- copy(sk)


# need a check to see if spp is "very" different from ref



# check entries where there is a 2-word ref, but taxLvl and species are NA
word2_lett4 <- function(x){ # TRUE if there are at least 2 words with 4 letters; x is a vector of characters
	sapply(strsplit(x, " "), function(x)sum(nchar(x)>=4))>=2
}
c5 <- sk[,(ref%in%refs2check & !c4 & word2_lett4(ref) & (taxLvl!="species" | is.na(taxLvl)) & is.na(species) & (!flag%in%c("check","ok","fine") | is.na(flag))) | ref=="ETROPUS INTERMEDIUS"]
c5 <- c5&!is.na(c5)
check(sk, c5)
sk5 <- copy(sk)



# check entries that are taxLvl=="species", but ref does not have 2 words
c6 <- sk[,!word2_lett4(ref) & (taxLvl=="species" & !is.na(taxLvl)) & (is.na(flag) | flag!="bad")]
c6 <- c6&!is.na(c6)
check(sk, c6)
sk6 <- copy(sk)



# check for inconsistences
ignore_cols <- c("ref","flag","val.src","tbl.row","mtch.src","tax.src","website","website2","tax.src2","conflict", "family","order","class","phylum","kingdom","superclass","subphylum")
col2check <- names(sk)[!names(sk)%in%ignore_cols]
spp_tbl <- sk[,table(spp)]
st_2words <- sk[,spp%in%names(spp_tbl)[grepl(" ", names(spp_tbl))]]
st_multi <- sk[,spp%in%names(spp_tbl)[spp_tbl>1]]
spp_inc <- sk[st_2words&st_multi, any(sapply(.SD[,eval(s2c(col2check))], lu, na.rm=TRUE)>1),by=c("spp")][(V1),spp]
c7 <- sk[,spp%in%spp_inc]
check(sk, c7)

sk7_0 <- copy(sk)
checkConsistent(Z=sk7_0, not.consistent=ignore_cols)
sk7 <- copy(sk7_0)


# genus should be the first word of species
has_species <- sk[,!is.na(species)]
genus_hat <- sk[, gsub(" .*", "", species)]
c8 <- sk[,has_species & (genus!=genus_hat | is.na(genus))]
check(sk, c8)
sk8 <- copy(sk)


# do checks based on what are colonizing/ leaving so I can send something good to data providers
load("~/Documents/School&Work/pinskyPost/trawl/trawlDiversity/pkgBuild/spp_check/colonize_refs.RData")
col_refs_2check <- colonize_refs[is.na(flag) | !flag%in%c("check","ok"), ref]
c9 <- sk[,ref%in%col_refs_2check & (is.na(flag) | !flag%in%c("check","ok"))]
check(sk, c9)
# sk[spp=="Hippasteria phrygiana", c("trophicLevel","trophicLevel.se"):=list(3.45, 0.39)]
# sk[spp=="Laqueus vancouveriensis", c("spp","species"):=list("Laqueus californianus","Laqueus californianus")]
sk9 <- copy(sk)

# ref is not equal to spp (case aside), and flag is added_automatically or is NA & taxLvl is species
c10_flag <- sk[,is.na(flag)|flag=="added_automatically"]
c10_spp_ref <- sk[,tolower(ref)!=tolower(spp) | is.na(spp)]
c10_taxLvl_species <- sk[,taxLvl=="species" & !is.na(taxLvl)]
c10 <- c10_taxLvl_species & c10_spp_ref & c10_flag & sk[,ref%in%refs2check]
check(sk, c10)
# sk[(spp)=="Ophichthus ophis", c("trophicLevel", "trophicLevel.se"):=list(4.34, 0.44)]
# sk[(spp)=="Bathyraja richardsoni", c("trohpicLevel","trophicLevel.se"):=list(4.02, 0.69)]
# sk[(spp)=="Gonostoma denudatum", c("trophicLevel","trophicLevel.se"):=list(3.30, 0.34)]
sk10 <- copy(sk)





# infer full taxonomy, one step at a time
genus_family <- sk[!is.na(genus),j={
	tbl <- table(family)
	fam <- names(tbl)[which.max(tbl)]
	list(family=fam)
},by="genus"]
family_order <- sk[!is.na(family) & family%in%genus_family[,una(family)], j={
	tbl <- table((order))
	ord <- names(tbl)[which.max(tbl)]
	list(order=ord)
},by="family"]
order_class <- sk[!is.na(order) & order%in%family_order[,una(order)], j={
	tbl <- table((class))
	cla <- names(tbl)[which.max(tbl)]
	list(class=cla)
},by="order"]
class_phylum <- sk[!is.na(class) & class%in%order_class[,una(class)], j={
	tbl <- table((phylum))
	phy <- names(tbl)[which.max(tbl)]
	list(phylum=phy)
},by="class"]
phylum_kingdom <- sk[!is.na(phylum) & phylum%in%class_phylum[,una(phylum)], j={
	tbl <- table((kingdom))
	kin <- names(tbl)[which.max(tbl)]
	list(kingdom=kin)
},by="phylum"]

