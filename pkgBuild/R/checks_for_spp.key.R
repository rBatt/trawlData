




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


# common name belongs to multiple spp
generic_common <- c("sea cucumbers", "barnacles", "bivalves","eels", "lampreys", "mussels", "mackerels", "octopuses", "scallops", "sea fir", "sea stars", "shrimp", "sea anemone", "sea pens", "sea urchin", "sponge", "squids", "true whelks", "soft corals", "hermit crabs","amphipods","nudibranch","ribbonfishes","polychaete worm","true soft corals")
mltpl_common <- sk[,((common)%in%sk[!is.na(common) & !is.na(spp),list(n_spp=lu(spp)), by="common"][n_spp>1, common])]

c2 <- sk[,ref%in%refs2check & !flag%in%c("bad","ok", "fine", "check") & !(common)%in%generic_common & mltpl_common, ]
c2 <- c2&!is.na(c2)
check(sk, c2)
sk2 <- copy(sk)

# common contains non- A-Za-z chars
c3 <- sk[,grepl("[^a-zA-Z -]", common)]
c3 <- c3&!is.na(c3)
check(sk, c3)

# species is NA, but common name isn't, and isn't flagged
sk[is.na(species) & !is.na(common) & (is.na(flag) | flag!="bad")]


