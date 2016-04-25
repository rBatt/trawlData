




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
check(sk, c4, google=TRUE)
sk4 <- copy(sk)


# need a check to see if spp is "very" different from ref



# check entries where there is a 2-work spp, but taxLvl and species are NA


