library(taxize)
library(data.table)

load("./data/taxInfo.RData")
load("./data/spp.corr1.RData")
load("data/getSppData.RData")
load("data/getTaxData.RData")
load("data/getCmmnData.RData")

create.spp.key <- function(spp, taxInfo, spp.corr1){
	
	# X.match is from taxInfo
	X.match <- match.tbl(ref=spp[-1], tbl.ref=taxInfo[,raw.spp], tbl.val=taxInfo[,spp])
	X.match[,mtch.src:=1]
	
	# X.match2 is from the spp.corr1.RData (from taxize)
	X.match2 <- match.tbl(ref=spp[-1], tbl.ref=spp.corr1[,spp], tbl.val=spp.corr1[,sppCorr])
	X.match2[,mtch.src:=2]
	
	# X.match3 is from getSppData.RData (more recent taxize on more taxa)
	X.match3 <- match.tbl(ref=spp[-1],tbl.ref=getSppData[,spp],tbl.val=getSppData[,sppCorr], exact=T)
	X.match3[,mtch.src:=3]
	

	
	# Combine the 3 sources for ref -> spp conversions
	# Gives preference to non-na values in X.match > X.match2 > X.match3
	spp.key00 <- rbind(X.match[!is.na(val)], X.match2[!is.na(val)], X.match3[!is.na(val)])
	spp.noMatch <- X.match[!ref%in%spp.key00[,ref], ref]
	X.noMatch <- data.table(
		ref = spp.noMatch,
		val = NA_character_,
		val.src = NA_character_,
		tbl.row = NA_real_,
		mtch.src = NA_real_
	)
	spp.key0 <- rbind(spp.key00, X.noMatch)
	setorder(spp.key0, ref, mtch.src, val, na.last=T)
	
	spp.key <- unique(spp.key0) # unique drops out 
	setnames(spp.key, "val", "spp")
	setkey(spp.key, spp)
	
	
	ti2 <- copy(taxInfo)
	setkey(ti2, spp)
	ti2 <- unique(ti2)
	ti2[,raw.spp:=NULL]
	ti2[,correctSpp:=NULL]
	ti2[,isSpecies:=NULL]
	ti2[,taxLvl:=tolower(taxLvl)]
	ti2[,tax.src:="taxInfo"]
	
	spp.key <- merge(spp.key, ti2, by="spp", all.x=T)
	setcolorder(spp.key, c("ref", "val.src", "tbl.row", "mtch.src", "tax.src", "spp", "common", "taxLvl", "species", "genus", "family", "order", "class", "superclass", "subphylum", "phylum", "kingdom", "trophicDiet", "trophicOrig", "Picture", "trophicLevel", "trophicLevel.se"))
	
	
	for(i in 2:length(names(getTaxData))){
		tn <- names(getTaxData)[i]
		if(!tn%in%names(spp.key)){
			# If we're trying to add a column that spp.key doesn't have, then skip
			next
		}
		
		t.gTD <- getTaxData[,eval(s2c(tn))][[1]]
		t.s.k <- spp.key[,eval(s2c(tn))][[1]]
		
		can.help <- spp.key[is.na(t.s.k), spp] %in%  getTaxData[!is.na(t.gTD), sppCorr]
		
		if(any(can.help)){
			mt <- match.tbl(ref=spp.key[can.help,spp], tbl.ref=getTaxData[,sppCorr], tbl.val=getTaxData[,eval(s2c(tn))][[1]], exact=T)
			spp.key[can.help, c(tn, "tax.src2"):=list(mt[,val],"getTaxData")]
		}else{
			next
		}
	}
	

	
	# insert non-NA taxonomnic CLASSifications where needed
	match.cmmn <- getCmmnData[, match.tbl(spp.key[,spp], sppCorr, common, exact=T)]
	needs.cmmn <- spp.key[,is.na(common)]
	has.cmmn <- match.cmmn[,!is.na(val)]
	spp.key[needs.cmmn&has.cmmn,common:=match.cmmn[needs.cmmn&has.cmmn,val]]

	# Define conflicts in spp.key
	spp.key[!is.na(spp), conflict:=any(!sapply(.SD, function(x)lu(x[!is.na(x)])<=1)), by="spp"]
	
	
	
	# Add flag column if it doesn't exist
	if(!"flag"%in%names(spp.key)){
		spp.key[,flag:=NA_character_]
	}
	
	
	# Loop through conflicts by species, and flag
	spp2loop <- spp.key[!is.na(spp) & conflict & is.na(flag),unique(spp)]
	for(i in 1:length(spp2loop)){
		t.spp <- spp2loop[i]
		t.index <- spp.key[,spp==t.spp & !is.na(spp)]
		print(paste0(t.spp, "; ", i, " of ",length(spp2loop)))
		flag.spp(spp.key, t.index)
	}
	
	
	
	
	
	# ======================
	# = Manual Corrections =
	# ======================	
	check.and.set(wrong="Moira atropus", corrected="Moira atropos")
	spp.key[spp=="Moira atropos",
		':='(
			taxLvl="species",
			species="Moira atropos",
			genus="Moira",
			website="http://www.marinespecies.org/echinoidea/aphia.php?p=taxdetails&id=158067",
			flag="manual"
		)
	]
	
	
	check.and.set(wrong="Centropristis ocyurus", corrected="Centropristis ocyurus")
	spp.key[spp=="Centropristis ocyurus",
		':='(
			taxLvl="species",
			species="Centropristis ocyurus",
			genus="Centropristis",
			common="Bank sea bass", 
			Picture="y", 
			trophicLevel=3.5, 
			trophicLevel.se=0.53,  
			website="http://www.fishbase.org/summary/3316",
			flag="manual"
		)
	]
	
	
	check.and.set(wrong="Mustellus canis", corrected="Mustelus canis")
	spp.key[spp=="Mustelus canis",
		':='(
			taxLvl="species",
			common="Dusky smooth-hound",
			Picture="y",
			trophicLevel=3.6,
			# trophicLevel.se=0.2,
			website="http://www.fishbase.org/summary/Mustelus-canis.html",
			flag="manual"
		)
	]
	
	check.and.set(wrong="Phenacoscorpius nebris", corrected="Phenacoscorpius nebris")
	spp.key[spp=="Phenacoscorpius nebris",
		':='(
			taxLvl="species",
			species="Phenacoscorpius nebris",
			genus="Phenacoscorpius",
			common="Short-tube scorpionfish",
			Picture="y",
			trophicLevel=3.5,
			trophicLevel.se=0.6,
			website="http://www.fishbase.org/summary/12454",
			flag="manual"
		)
	]
	
	
	check.and.set(wrong="Synagrops bellus", corrected="Synagrops bellus")
	spp.key[spp=="Synagrops bellus",
		':='(
			taxLvl="species",
			species="Synagrops bellus",
			genus="Synagrops",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=159584",
			flag="manual"
		)
	]
	
	check.and.set(wrong="Synagrops microlepis", corrected="Synagrops microlepis")
	spp.key[spp=="Synagrops microlepis",
		':='(
			taxLvl="species",
			species="Synagrops microlepis",
			genus="Synagrops",
			common="Thinlip splitfin",
			Picture="y",
			trophicLevel=3.2,
			trophicLevel.se=0.37,
			website="http://www.fishbase.org/summary/5059",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="EZUMIA BAIRDII", Spp="Nezumia bairdii")
	spp.key[spp=="Nezumia bairdii",
		':='(
			spp="Nezumia bairdii",
			taxLvl="species",
			genus="Nezumia",
			species="Nezumia bairdii",
			trophicLevel=3.3,
			trophicLevel.se=0.1,
			Picture="y",
			common="Marlin-spike",
			website="http://www.fishbase.org/summary/3104",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="PORTUNUS FLORIDANUS", Spp="Achelous floridanus")
	spp.key[spp=="Achelous floridanus",
		':='(
			spp="Achelous floridanus",
			taxLvl="species",
			species="Achelous floridanus",
			genus="Achelous",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=557882",
			flag="manual"
			
		)
	]
	
	
	ref2spp(Ref="ASTROPECTE COMPTUS", Spp="Astropecten comptus")
	spp.key[spp=="Astropecten comptus",
		':='(
			spp="Astropecten comptus",
			taxLvl="species",
			genus="Astropecten",
			species="Astropecten comptus",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=178649",
			flag="manual"	
		)
	]
	
	ref2spp(Ref="BATHYPTEROIS BIGELOWI", Spp="Bathypterois bigelowi")
	spp.key[spp=="Bathypterois bigelowi",
		':='(
			spp="Bathypterois bigelowi",
			taxLvl="species",
			genus="Bathypterois",
			species="Bathypterois bigelowi",
			website="http://www.fishbase.org/summary/Bathypterois-bigelowi.html",
			Picture="y",
			flag="manual"
		)
	]
	
	ref2spp(Ref="PENNATULA BOREALIS", Spp="Pennatula grandis")
	spp.key[spp=="Pennatula grandis",
		':='(
			spp="Pennatula grandis",
			taxLvl="species",
			genus="Pennatula",
			species="Pennatula grandis",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=128516",
			flag="manual"
		)
	]
	
	
	check.and.set(wrong="Bollmannia communis", corrected="Bollmannia communis")
	spp.key[spp=="Bollmannia communis",
		':='(
			spp="Bollmannia communis",
			taxLvl="species",
			genus="Bollmannia",
			species="Bollmannia communis",
			flag="manual"
		)
	]
	
	
	
	ref2spp(Ref="Brisaster townsendi", Spp="Brisaster townsendi")
	spp.key[spp=="Brisaster townsendi",
		':='(
			spp="Brisaster townsendi",
			taxLvl="species",
			genus="Brisaster",
			species="Brisaster townsendi",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=513144",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Buccinum transliratum", Spp="Buccinum angulosum")
	spp.key[spp=="Buccinum angulosum",
		':='(
			spp="Buccinum angulosum",
			common="angular whelk",
			family="Buccinidae",
			order="Neogastropoda",
			class="Gastropoda",
			phylum="Mollusca",
			kingdom="Animalia",
			taxLvl="species",
			genus="Buccinum",
			species="Buccinum angulosum",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=138858",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="CHILOMYCTERUS ATINGA", Spp="Chilomycterus reticulatus")
	spp.key[spp=="Chilomycterus reticulatus",
		':='(
			spp="Chilomycterus reticulatus",
			common="spotfin burrfish",
			taxLvl="species",
			genus="Chilomycterus",
			species="Chilomycterus reticulatus",
			trophicLevel=3.45, # should just be 3.5?
			trophicLevel.se=0.41,
			Picture="y",
			website="http://www.fishbase.org/summary/Chilomycterus-reticulatus.html",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=403539",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="CHLAMYS SETIS", Spp="Caribachlamys sentis")
	spp.key[spp=="Caribachlamys sentis",
		':='(
			spp="Caribachlamys sentis",
			taxLvl="species",
			genus="Caribachlamys",
			species="Caribachlamys sentis",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=393711",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Colus hypolispus", Spp="Latisipho hypolispus")
	spp.key[spp=="Latisipho hypolispus",
		':='(
			spp="Latisipho hypolispus",
			taxLvl="species",
			genus="Latisipho",
			species="Latisipho hypolispus",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=491062",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=254476",
			flag="manual"
		)
	]
	
	ref2spp(Ref="CYCLOTHONE BRAUERI", Spp="Cyclothone braueri")
	spp.key[spp=="Cyclothone braueri",
		':='(
			spp="Cyclothone braueri",
			taxLvl="species",
			common="Garrick",
			genus="Cyclothone",
			species="Cyclothone braueri",
			family="Gonostomatidae",
			order="Stomiiformes",
			class="Actinopterygii",
			trophicLevel=3.1,
			trophicLevel.se=0.18,
			Picture="y",
			website="http://www.fishbase.de/summary/Cyclothone-braueri.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="DIAPHUS SPLEDIDUS", Spp="Diaphus splendidus")
	spp.key[spp=="Diaphus splendidus",
		':='(
			spp="Diaphus splendidus",
			taxLvl="species",
			common="Horned lanternfish",
			genus="Diaphus",
			species="Diaphus splendidus",
			family="Myctophidae",
			order="Myctophiformes",
			class="Actinopterygii",
			trophicLevel=3.0,
			trophicLevel.se=0,
			Picture="y",
			website="http://www.fishbase.org/summary/10174",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="DISTORSIO MCGITYI", Spp="Distorsio constricta mcgintyi")
	spp.key[spp=="Distorsio constricta mcgintyi",
		':='(
			spp="Distorsio constricta mcgintyi",
			taxLvl="species",
			genus="Distorsio",
			species="Distorsio constricta",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=422768",
			flag="manual"
		)
	]
	
	ref2spp(Ref="LEPIDOPA BEEDICTI", Spp="Lepidopa benedicti")
	spp.key[spp=="Lepidopa benedicti",
		':='(
			spp="Lepidopa benedicti",
			genus="Lepidopa",
			species="Lepidopa benedicti",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=421881",
			flag="manual"
		)
	]
	
	
	spp.key[spp=="Hyporthodus niveatus",subphylum:="Vertebrata"]
	setkey(spp.key, spp, ref)
	ref2spp(Ref="EPINEPHELUS NIVEATUS", Spp="Hyporthodus niveatus")
	spp.key[spp=="Hyporthodus niveatus",
		':='(
			spp="Hyporthodus niveatus",
			genus="Hyporthodus",
			species="Hyporthodus niveatus",
			common="snowy grouper",
			family="Serranidae",
			order="Perciformes",
			class="Actinopteri",
			superclass="Osteichthyes",
			subphylum="Vertebrata",
			phylum="Chordata",
			kingdom="Animalia",
			trophicDiet="n",
			trophicOrig="y",
			taxLvl="species",
			trophicLevel=4.04,
			trophicLevel.se=0.58,
			Picture="y",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="EQUETUS ACUMINATUS", Spp="Pareques acuminatus")
	spp.key[spp=="Pareques acuminatus",
		':='(
			spp="Pareques acuminatus",
			genus="Pareques",
			species="Pareques acuminatus",
			taxLvl="species",
			common="high-hat",
			family="Sciaenidae",
			order="Perciformes",
			class="Actinopterygii",
			superclass="Osteichthyes",
			subphylum="Vertebrata",
			phylum="Chordata",
			kingdom="Animalia",
			trophicDiet="y",
			trophicOrig="y",
			Picture="y",
			trophicLevel=3.59,
			trophicLevel.se=0.46,
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="ETMOPTERUS VIRES", spp="Etmopterus virens")
	spp.key[spp=="Etmopterus virens",
		':='(
			spp="Etmopterus virens",
			genus="Etmopterus",
			species="Etmopterus virens",
			common="green lantern shark",
			trophicLevel=4.2,
			trophicLevel.se=0.73,
			Picture="y",
			website="http://www.fishbase.org/summary/690",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="LUIDIA ELEGAS", Spp="Luidia sarsii elegans")
	spp.key[spp=="Luidia sarsii elegans",
		':='(
			spp="Luidia sarsii elegans",
			genus="Luidia",
			species="Luidia sarsii",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=752124",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=368112",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="HALICHOERES RADIATUS", Spp="Halichoeres radiatus")
	spp.key[spp=="Halichoeres radiatus",
		':='(
			spp="Halichoeres radiatus",
			genus="Halichoeres",
			species="Halichoeres radiatus",
			taxLvl="species",
			common="Puddingwife wrasse",
			trophicLevel=3.5,
			trophicLevel.se=0.1,
			Picture="y",
			website="http://www.fishbase.org/summary/1068",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Henricia aleutica", Spp="Henricia longispina aleutica")
	spp.key[spp=="Henricia longispina aleutica",
		':='(
			spp="Henricia longispina aleutica",
			genus="Henricia",
			species="Henricia longispina",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=369124",
			flag="manual"
		)
	]
	
	
	
	ref2spp(Ref="ALUTERUS HEUDELOTTI", Spp="Aluterus heudelotii")
	ref2spp(Ref="ALUTERUS HEUDELOTI", Spp="Aluterus heudelotii")
	spp.key[spp=="Aluterus heudelotii",
		':='(
			common="Dotterel filefish",
			taxLvl="species",
			genus="Aluterus",
			species="Aluterus heudelotii",
			family="Monacanthidae",
			order="Tetraodontiformes",
			class="Actinopteri",
			superclass="Actinopterygii",
			subphylum="Craniata",
			phylum="Chordata",
			kingdom="Metazoa",
			Picture="y",
			trophicLevel=2.0,
			trophicLevel.se=0,
			website="http://www.fishbase.org/summary/4273",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="ALPHEUS ARMATUS", Spp="Alpheus armatus")
	spp.key[spp=="Alpheus armatus",
		':='(
			spp="Alpheus armatus",
			taxLvl="species",
			genus="Alpheus",
			species="Alpheus armatus",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=421729",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="COOKEOLUS JAPONICUS", Spp="Cookeolus japonicus")
	spp.key[spp=="Cookeolus japonicus",
		':='(
			species="Cookeolus japonicus",
			genus="Cookeolus",
			taxLvl="species",
			common="Longfinned bullseye",
			Picture="y",
			trophicLevel=3.5,
			trophicLevel.se=0.6,
			website="http://www.fishbase.org/summary/3517",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="APRISTURUS IDICUS", Spp="Apristurus indicus")
	spp.key[spp=="Apristurus indicus",
		':='(
			spp="Apristurus indicus",
			species="Apristurus indicus",
			genus="Apristurus",
			taxLvl="species",
			common="Smallbelly catshark",
			trophicLevel=3.7,
			trophicLevel.se=0.3,
			Picture="y",
			website="http://www.fishbase.org/Summary/speciesSummary.php?ID=766&AT=Kleinbuikkathaai",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Arctomelon sp. cf. stearnsii (Clark & McLean)", Spp="Arctomelon stearnsii")
	spp.key[ref=="Arctomelon sp. cf. stearnsii (Clark & McLean)",
		':='(
			spp="Arctomelon stearnsii",
			species="Arctomelon stearnsii",
			genus="Arctomelon",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=384681",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="ASTARTE GLOBULA", Spp="Astarte globula")
	spp.key[ref=="ASTARTE GLOBULA",
		':='(
			spp="Astarte globula",
			species="Astarte globula",
			genus="Astarte",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=420839",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="BUSYCO CARICA", Spp="Busycon carica")
	spp.key[spp=="Busycon carica",
		':='(
			spp="Busycon carica",
			genus="Busycon",
			species="Busycon carica",
			taxLvl="species",
			Picture="y",
			common="Knobbed whelk",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=160185",
			website2="http://www.sealifebase.org/summary/Busycon-carica.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="CALAMUS PENNA", Spp="Cabdio penna")
	ref2spp(Ref="CALAMUS PEA", Spp="Cabdio penna")
	spp.key[spp=="Cabdio penna",
		':='(
			spp="Cabdio penna",
			genus="Cabdio",
			species="Cabdio penna",
			taxLvl="species",
			common="Sheepshead porgy",
			Picture="y",
			trophicLevel=4.4,
			trophicLevel.se=0.2,
			website="http://www.fishbase.org/summary/1227",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=159245",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="CALLIECTES DAAE", Spp="Callinectes danae")
	spp.key[spp=="Callinectes danae",
		':='(
			spp="Callinectes danae",
			genus="Callinectes",
			species="Callinectes danae",
			taxLvl="species",
			common="Dana swimming crab",
			trophicLevel=3.63,
			trophicLevel.se=0.43,
			Picture="y",
			website="http://www.sealifebase.org/summary/Callinectes-danae.html",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=107378"
		)
	]
	
	
	ref2spp(Ref="Buccinum triplostephanum", Spp="Volutopsion castaneum")
	spp.key[spp=="Volutopsion castaneum",
		':='(
			species="Volutopsion castaneum",
			genus="Volutopsion",
			taxLvl="species",
			common="chestnut whelk",
			website="http://arctos.database.museum/name/Buccinum%20castaneum%20triplostephanum",
			website2="http://www.sealifebase.org/summary/Volutopsion-castaneum.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="BUSYCO CAALICULATUM", Spp="Busycotypus canaliculatus")
	spp.key[spp=="Busycotypus canaliculatus",
		':='(
			spp="Busycotypus canaliculatus",
			species="Busycotypus canaliculatus",
			genus="Busycotypus",
			taxLvl="species",
			common="Channeled whelk",
			Picture="y",
			trophicLevel=3.23,
			trophicLevel.se=0.17,
			website="http://arctos.database.museum/name/Busycon%20canaliculatum",
			website2="http://www.sealifebase.org/summary/Busycotypus-canaliculatus.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="BUSYCO SIISTRUM", Spp="Sinistrofulgur sinistrum")
	spp.key[spp=="Sinistrofulgur sinistrum",
		':='(
			spp="Sinistrofulgur sinistrum",
			species="Sinistrofulgur sinistrum",
			genus="Sinistrofulgur",
			taxLvl="species",
			common="Lightning whelk",
			Picture="y",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=160189",
			website2="http://sealifebase.org/summary/Busycon-sinistrum.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="CALAPPA AGUSTA", Spp="Hepatus pudibundus")
	spp.key[spp=="Hepatus pudibundus", # the correct species name should already be in spp.key
		':='(
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=452047",
			website2="http://www.sealifebase.org/summary/Hepatus-pudibundus.html"
		)
	]
	
	
	
	ref2spp(Ref="GOOSTOMA ATLATICUM", Spp="Gonostoma atlanticum")
	spp.key[spp=="Gonostoma atlanticum",
		':='(
			spp="Gonostoma atlanticum",
			species="Gonostoma atlanticum",
			genus="Gonostoma",
			common="Atlantic fangjaw",
			taxLvl="species",
			trophicLevel=3.0,
			trophicLevel.se=0.13,
			Picture="y",
			website="http://www.fishbase.org/summary/5052",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Careproctus sp. cf. rastrinus (Orr et al.)", Spp="Careproctus rastrinus")
	spp.key[spp=="Careproctus rastrinus",
		':='(
			Picture="y",
			website="http://www.fishbase.org/summary/Careproctus-rastrinus.html",
			flag="manual"
		)
	]
	
	
	
	ref2spp(Ref="Careproctus sp. cf. gilberti (Orr)", Spp="Careproctus gilberti")
	spp.key[spp=="Careproctus gilberti",
		':='(
			trophicLevel=3.2,
			trophicLevel.se=0.5,
			Picture="y",
			website="http://www.fishbase.org/summary/25161",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Careproctus lycopersicus", Spp="Careproctus lycopersicus")
	spp.key[spp=="Careproctus lycopersicus",
		':='(
			spp="Careproctus lycopersicus",
			species="Careproctus lycopersicus",
			genus="Careproctus",
			common="tomato snailfish",
			taxLvl="species",
			trophicLevel=3.2,
			trophicLevel.se=0.5,
			website="http://www.fishbase.org/summary/67403",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="COUS DAUCUS", Spp="Conus daucus")
	spp.key[spp=="Conus daucus",
		':='(
			spp="Conus daucus",
			species="Conus daucus",
			genus="Conus",
			taxLvl="species",
			common="carrot cone",
			Picture="y",
			website="http://www.sealifebase.org/summary/Conus-daucus.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="ICTALURUS FURCATUS", Spp="Ictalurus furcatus")
	spp.key[spp=="Ictalurus furcatus", # freshwater/ brackish species!!
		':='(
			spp="Ictalurus furcatus",
			species="Ictalurus furcatus",
			genus="Ictalurus",
			taxLvl="species",
			common="blue catfish",
			trophicLevel=3.4,
			trophicLevel.se=0.44,
			Picture="y",
			website="http://www.fishbase.org/summary/3019",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="CYPSELURUS MELANURUS", Spp="Cheilopogon melanurus")
	spp.key[spp=="Cheilopogon melanurus",
		':='(
			spp="Cheilopogon melanurus",
			species="Cheilopogon melanurus",
			genus="Cheilopogon",
			taxLvl="species",
			Picture="y",
			trophicLevel=3.6,
			trophicLevel.se=0.5,
			website="http://www.fishbase.org/summary/cheilopogon-melanurus.html",
			website2="http://www.marinespecies.org/aphia.php?p=taxdetails&id=312468",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Chrysaora fuscens", Spp="Chrysaora fuscescens")
	spp.key[spp=="Chrysaora fuscescens",
		':='(
			spp="Chrysaora fuscescens",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=287206",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="LIMA SCABRA", Spp="Ctenoides scaber")
	spp.key[spp=="Ctenoides scaber",
		':='(
			spp="Ctenoides scaber",
			genus="Ctenoides",
			species="Ctenoides scaber",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=420747",
			website2="http://www.sealifebase.org/summary/Ctenoides-scaber.html",
			Picture="y",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="PIOTHERES MACULATUS", Spp="Tumidotheres maculatus")
	spp.key[spp=="Tumidotheres maculatus",
		':='(
			spp="Tumidotheres maculatus",
			genus="Tumidotheres",
			species="Tumidotheres maculatus",
			common="squatter pea crab",
			taxLvl="species",
			Picture="y",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=454715",
			website2="http://sealifebase.org/summary/Tumidotheres-maculatus.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="DROMIDIA ATILLESIS", Spp="Moreiradromia antillensis")
	spp.key[spp=="Moreiradromia antillensis",
		':='(
			spp="Moreiradromia antillensis",
			species="Moreiradromia antillensis",
			genus="Moreiradromia",
			common="sponge crab",
			taxLvl="species",
			Picture="y",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=241025",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="MICROPHRYS ANTILLENSIS", Spp="Microphrys antillensis")
	spp.key[spp=="Microphrys antillensis",
		':='(
			spp="Microphrys antillensis",
			species="Microphrys antillensis",
			genus="Microphrys",
			taxLvl="species",
			common="lobed decorator crab",
			website="http://www.sealifebase.org/summary/Microphrys-antillensis.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="Cyclocardia sp. cf. borealis (Clark 2006)", Spp="Cyclocardia borealis")
	spp.key[spp=="Cyclocardia borealis",
		':='(
			spp="Cyclocardia borealis",
			species="Cyclocardia borealis",
			genus="Cyclocardia",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=156832",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="EPINEPHELUS MYSTACINUS", Spp="Hyporthodus mystacinus")
	spp.key[spp=="Hyporthodus mystacinus",
		':='(
			spp="Hyporthodus mystacinus",
			species="Hyporthodus mystacinus",
			genus="Hyporthodus",
			taxLvl="species",
			common="misty grouper",
			Picture="y",
			trophicLevel=4.6,
			trophicLevel.se=0,
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=273861",
			website2="http://www.fishbase.org/summary/Hyporthodus-mystacinus.html",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="ETMOPTERUS BULLISI", Spp="Etmopterus bullisi")
	spp.key[spp=="Etmopterus bullisi",
		':='(
			spp="Etmopterus bullisi",
			species="Etmopterus bullisi",
			genus="Etmopterus",
			taxLvl="species",
			common="lined lanternshark",
			trophicLevel=4.2,
			trophicLevel.se=0.4,
			Picture="y",
			website="http://www.fishbase.org/summary/676",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="GOBIOSOMA HORSTI", Spp="Elacatinus horsti")
	spp.key[spp=="Elacatinus horsti",
		':='(
			spp="Elacatinus horsti",
			species="Elacatinus horsti",
			genus="Elacatinus",
			taxLvl="species",
			common="yellowline goby",
			Picture="y",
			trophicLevel=3.5,
			trophicLevel.se=0.4,
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=309575",
			website2="http://www.fishbase.org/summary/3873",
			flag="manual"
		)
	]
	
	
	spp.key[spp=="Gorgonocephalus arcticus", order:="Phrynophiurida"]
	ref2spp(Ref="Gorgonocephalus sp. cf. arcticus", Spp="Gorgonocephalus arcticus")
	spp.key[spp=="Gorgonocephalus arcticus",
		':='(
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=124966",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="GORGONOCEPHALUS LAMARCKI", Spp="Gorgonocephalus lamarckii")
	spp.key[spp=="Gorgonocephalus lamarckii",
		':='(
			spp="Gorgonocephalus lamarckii",
			species="Gorgonocephalus lamarckii",
			genus="Gorgonocephalus",
			taxLvl="species",
			website="http://www.marinespecies.org/aphia.php?p=taxdetails&id=416670",
			flag="manual"
		)
	]
	
	
	ref2spp(Ref="MONACANTHUS CILIATUS", Spp="Monacanthus ciliatus")
	spp.key[spp=="Monacanthus ciliatus",
		':='(
			website="http://www.fishbase.org/summary/4280",
			flag="manual"
		)
	]
	
	# spp.key <- fread("inst/extdata/taxonomy/spp.key.csv", na.strings=c("","NA"))
	setkey(spp.key, spp, ref)
	save(spp.key, file="data/spp.key.RData")
	write.csv(spp.key, file="inst/extdata/taxonomy/spp.key.csv", row.names=F)
	
	return(spp.key)
}

