

sk[ref=="TRACHINOCEPHALUS MYOPS", c("species", "genus"):=list(spp, "Synodus")]
sk[spp=="Lycodapus dermatinus", common:=NA]
sk[species=="Eucryphycus californicus", c("spp", "common", "flag"):=list("Eucryphycus californicus", "persimmon eelpout", "check")]
sk[spp=="Eogastropoda", common:=NA]
sk[spp=="Myliobatidae", common:=NA]
sk[spp%in%c("Fasciolaria hunteria", "Nephtheidae"), common:=NA]
sk[spp=="Fasciolaria hunteria", flag:="check"]
sk[spp=="Parahollardia lineata", common:="jambeau"]

sk[spp=="Poromitra crassiceps", common:="crested bigscale"]
sk[spp=="Rhacochilus toxotes", common:="rubberlip seaperch"]
sk[spp=="Japetella", common:=NA]
sk[spp=="Rhinobatidae", common:=NA]

sk[spp=="Labrisomus nuchipinnis", common:="hairy blenny"]
sk[spp=="Rhinoliparis barbulifer", common:="longnose snailfish"]
sk[spp=="Rochinia crassa", c("common","species", "taxLvl", "flag"):=list("inflated spiny crab", "Rochinia crassa", "species", "check")]
sk[spp=="Lampadena", common:=NA]
sk[spp=="Liopropoma eukrines", common:="wrasse bass"]
sk[spp=="Anasimus latus", c("taxLvl","species","genus", "flag"):=list("species","Anasimus latus","Anasimus","check")]
sk[spp=="Anasimus", c("taxLvl","kingdom","flag","common"):=list("genus","Animalia","check",NA)]


sk[ref=="MONOMITOPUS AGASSIZII", c("spp","species","genus","family","order","class","flag","common"):=list("Monomitopus agassizii","Monomitopus agassizii","Monomitopus","Ophidiidae","Ophidiiformes","Actinopteri","check",NA)]
sk[spp=="Lobopilumnus agassizii", common:="areolated hairy crab"]
sk[spp=="Solariella", common:="sea snails"]
sk[spp=="Notoscopelus bolini", common:="lanternfish"]
sk[spp=="Vesicomyidae", common:=NA]
sk[ref=="DIOGENIDAE", c("family","spp","order","class","phylum","common"):=list("Diogenidae","Diogenidae",NA,NA,NA,"hermit crabs")]
sk[ref=="TELLINIDAE",c("family","order","class","phylum","common"):=list("Tellinidae",NA,NA,NA,"bivalves")]
sk[spp=="Psathyrometra fragilis",common:=NA]
sk[spp=="Psenes pellucidus", c("common", "trophicLevel", "trophicLevel.se"):=list("bluefin driftfish", 3.85, 0.53)]
sk[spp=="Salpidae", common:=NA]
sk[spp=="Scyllaridae",common:=NA]
sk[ref=="ARGENTINA GEORGEI", c("spp","species","common","flag"):=list("Argentina georgei","Argentina georgei","blackbelly argentine","check")]
sk[ref=="ATRINA SERRATA", c("spp","species","genus","family","order","class","phylum","common","flag"):=list("Atrina serrata","Atrina serrata","Atrina","	Pinnidae","Pterioida","Bivalvia","	Mollusca","saw-toothed pen shell","check")]
sk[ref=="BULLA STRIATA", c("spp","species","genus","family","order","class","phylum","common","flag"):=list("Bulla striata","Bulla striata","Bulla","Bullidae","Cephalaspidea","Gastropoda","Mollusca",NA,"check")]
sk[ref=="UIOIDAE", c("spp","common"):=list("Unionidae",NA)]
sk[spp=="Asteronyx",common:=NA]
sk[spp=="Asteronyx loveni", flag:="check"]
sk[ref=="ASTROPECTEN COMPTUS", c("spp","species","common", "flag"):=list("Astropecten comptus","Astropecten comptus",NA, "check")]
sk[spp=="Astropecten comptus",common:=NA]
sk[spp=="Calliostoma",common:=NA]
sk[spp=="Lactophrys quadricornis",c("spp"):="Acanthostracion quadricornis"]
sk[spp=="Acanthostracion quadricornis", c("taxLvl","species","genus","family","order","class","phylum","kingdom","flag"):=list("species","Acanthostracion quadricornis","Acanthostracion","Ostraciidae","Tetraodontiformes","Actinopteri","Chordata","Animalia","check")]
sk[ref=="Balanus evermanni", flag:="check"]

sk[ref=="MACROCOELOMA CAMPTOCERUM", c("common","flag"):=list(NA,"check")]
sk[ref=="MACROCOELOMA EUTHECA", c("spp","species","common","flag"):=list("Macrocoeloma eutheca","Macrocoeloma eutheca",NA,"check")]
sk[ref=="MYRICHTHYS OCULATUS", spp:="Myrichthys ocellatus"]
sk[spp=="Myrichthys ocellatus", c("species","genus","common"):=list("Myrichthys ocellatus","Myrichthys","goldspotted eel")]

sk[spp=="Callogorgia kinoshitae", common:=NA]
sk[ref=="EUNEPHTHYA RUBIFORMIS", c("spp","species","genus","flag"):=list("Gersemia rubiformis","Gersemia rubiformis","Gersemia","check")]
sk[spp=="Gersemia rubiformis", c("family","order","class","phylum","kingdom"):=list("Nephtheidae","Alcyonacea","Anthozoa","Cnidaria","Animalia")]
sk[spp%in%c("Carcharhininae","Galatheoidea"), common:=NA]
sk[ref=="LITTORINA SP.", c("spp","genus","common")==list("Littorina","Littorina",NA)]
sk[ref=="LUCINA", c("spp","genus","common","flag"):=list("Lucina","Lucina",NA,"check")]
sk[spp=="Centropristis",common:=NA]
sk[spp=="Chiasmodon", common:=NA]
sk[spp=="Graneledone", common:=NA]
sk[ref=="CORONASTER", flag:="bad"]

ref2spp("COODO OBILIS", "Conodon nobilis", Z=sk)

sk[spp=="Cardita floridana",c("common","taxLvl","species","genus"):=list("broad-ribbed cardita","species", "Cardita floridana","Cardita")]
sk[spp=="Atearius radiosus", c("flag","common"):=list("bad",NA)]
sk[spp%in%c("Ophiothricidae","Ophiothrix","Zoroasteridae"), common:=NA]

ref2spp("Colus aphelus", "Latisipho hypolispus", Z=sk)
sk[spp=="Cystisoma fabricii", c("taxLvl","species","genus","common"):=list("species","Cystisoma fabricii","Cystisoma",NA)]
sk[spp=="Danielum ixbauchac", c("taxLvl","species","genus","common"):=list("species","Danielum ixbauchac","Danielum",NA)]
sk[spp=="Aega psora", common:=NA]
sk[spp=="Diplectrum",common:=NA]
ref2spp("RAJA LAEVIS","Dipturus laevis",Z=sk)
sk[spp%in%c("Stylaster","Stylasterina"), common:=NA]
sk[ref=="PLEUROBRANCHAEA TARDA", c("spp","taxLvl","species","genus","common","flag"):=list("Pleurobranchaea tarda", "species","Pleurobranchaea tarda","Pleurobranchaea",NA,"check")]
sk[spp=="Lepidopa benedicti",common:=NA]
sk[spp%in%c("Brisingella","Fistularia"), common:=NA]
sk[spp=="Florometra", common:=NA]
sk[spp=="Glycera", common:=NA]
sk[spp%in%c("Goneplacinae","Ophioscolex corynetes"),common:=NA]
sk[spp=="Gorgonocephalus lamarckii", common:=NA]
sk[spp=="Geodia", common:=NA]
sk[spp=="Gymnelus", common:=NA]
sk[ref=="POMACANTHUS PARU", c("spp","taxLvl","species","genus","family","order","class","phylum","common","trophicLevel","trophicLevel.se","superclass","subphylum","flag"):=list("Pomacanthus paru","species","Pomacanthus paru","Pomacanthus",NA,NA,NA,NA,"French angelfish",2.85,0.03,NA,NA,"check")]
ref2spp("PRIACANTHUS CRUENTATUS","Heteropriacanthus cruentatus",Z=sk)

sk[spp=="Hexapanopeus", common:=NA]
sk[ref=="PAOPEUS BERMUDESIS",c("spp","taxLvl","species","genus","common","flag"):=list("Acantholobulus bermudensis","species","Acantholobulus bermudensis","Acantholobulus","strongtooth mud crab","check")]

sk[spp=="Hypsoblennius ionthas", common:="freckled blenny"]
sk[spp=="Halicardia perplicata", c("taxLvl","species","genus","common","flag"):=list("species","Halicardia perplicata","Halicardia",NA,"check")]
sk[spp=="Collodes",common:=NA]
sk[spp=="Japetella heathi", c("taxLvl","species","genus","common","flag"):=list("species","Japetella heathi","Japetella",NA,"check")]
sk[spp=="Rhinoliparis", common:=NA]
sk[ref=="POLYCHELES",c("spp","common","flag"):=list(NA,NA,"bad")]
sk[ref%in%c("POLYCHELES","PHOCIDAE","PHALIUM","PALICIDAE","HYALINA","HAEMULIDAE","EURYALIDAE"), c("spp","taxLvl","family","order","class","phylum","kingdom","common","flag"):=list(rep(NA,8),"bad")]
sk[spp=="Collodes leptocheles", c("taxLvl","species","genus","common","flag"):=list("species","Collodes leptocheles","Collodes",NA,"check")]
sk[ref%in%c("POTOPHILUS","OXYCEPHALUS","MALACOCEPHALUS","HYMEOCEPHALUS","GADUS  SP.","EPINEPHELUS","EPIEPHELUS","Alepocephalus sp.","ALEPOCEPHALUS SP."), c("spp","taxLvl","family","order","class","phylum","kingdom","common","flag"):=list(rep(NA,8),"bad")]

sk[ref=="PTERYGIOTEUTHIS SP.", c("common","flag"):=list(NA,"bad")]
ref2spp("LAGODO RHOMBOIDES","Lagodon rhomboides",Z=sk)



