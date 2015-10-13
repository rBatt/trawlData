


library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)


# =========================
# = Memory-saving options =
# =========================
delOldTrawl <- c(FALSE, TRUE)[1]


# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# ============================================
# = Identify individual regions to be loaded =
# ============================================
cleanDirectory <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions"
cleanedRegions <- list.files(cleanDirectory)
cleanNames.reg <- regexpr("[a-z]+[0-9]?(?=\\.RData$)", cleanedRegions, perl=TRUE)
cleanNames <- regmatches(cleanedRegions, cleanNames.reg)


# ==========================================
# = Load regions locally, combine globally =
# ==========================================
local({
	for(i in 1:length(cleanedRegions)){
		load(paste(cleanDirectory, cleanedRegions[i], sep="/")) # load each .RData file
	}
	trawl000 <<- rbindlist(mget(cleanNames)) # assign to global, or if found in a parent, reassign in that environment
	# print(ls()) # note that ls() default is to current frame, so only see locally-defined variables with ls()
	rm(list=ls()) # thus removing ls() preserves trawl and all variables defined outside of call to local()
})


# ============================
# = Convert spp to character =
# ============================
trawl000[,spp:=as.character(spp)]


# ======================
# = Remove bad species =
# ======================
uspp <- unique(trawl000[,spp])
badEgg <- uspp[grepl("[eE][gG]{2}", uspp)]
badFish <- uspp[grepl("(?<![a-z])fish(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
badLarv <- uspp[grepl("(?<![a-z])larv(a[e])?(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
# badJuv <- uspp[grepl("(?<![a-z])juven(ile)?(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
badYoy <- uspp[grepl("(?<![a-z])yoy(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]

badSpp <- unique(c(badEgg, badFish, badLarv, badYoy))

setkey(trawl000, spp)
trawl00 <- trawl000[!.(badSpp),]

# ==================
# = Save Memory #1 =
# ==================
if(delOldTrawl){
	rm(list="trawl000")
}


# =============================
# = Clean and reaggregate spp =
# =============================
trawl00[,raw.spp:=spp]
trawl00[,spp:=cullParen(cullSp(fixCase(cullExSpace(raw.spp))))]
setkey(trawl00, spp, year, s.reg)


# ================================================
# = Save a file containing key back to raw names =
# ================================================
# for use with matching to malin's spptaxonomy_2014-09-19.csv
raw.and.spp <- trawl00[,list(raw.spp, spp)]
setkey(raw.and.spp, raw.spp, spp)
raw.and.spp <- unique(raw.and.spp)
save(raw.and.spp, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/raw.and.spp.RData")

trawl00[,isSpecies:=is.species(spp)] # infer whether the taxa are identified to species or to genus (1 or 2 words)


# =============================
# = Watch out for duplicates? =
# =============================
# setkey(trawl00, s.reg, year, spp, stratum, datetime)
# sum(duplicated(trawl00))
#
# setkey(trawl00, s.reg, year, spp, stratum, haulid, datetime)
# sum(duplicated(trawl00))
#
# setkey(trawl00, s.reg, year, spp, stratum, haulid, datetime, raw.spp)
# sum(duplicated(trawl00))

# need the "duplicates" for matching to malin's taxa, i guess


# ================================
# = Use Taxize to clean up names =
# ================================
# eol.key <- "f0822ff32cb0af5fda7e4c9e02c66e47e7848e74"
# getkey("f0822ff32cb0af5fda7e4c9e02c66e47e7848e74", service="eol")

uspp <- trawl00[,unique(spp)]

tax.files <- dir("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy")


# =============================
# = Get Correct Species Names =
# =============================
if("spp.corr1.RData"%in%tax.files){
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")
	newlyChecked.spp <- getSpp(uspp=uspp, oldSpp=spp.corr1)
	spp.corr1 <- newlyChecked.spp
}else{
	newlyChecked.spp <- getSpp(uspp=uspp)
	spp.corr1 <- newlyChecked.spp
}

save(spp.corr1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")


# ==============================
# = Get "Correct" Common Names =
# ==============================
if("spp.cmmn1.RData"%in%tax.files){
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.cmmn1.RData")
	newlyChecked.cmmn <- getCmmn(u.sppCorr=spp.corr1[,sppCorr], oldCmmn=spp.cmmn1)
	spp.cmmn1 <- newlyChecked.cmmn
}else{
	newlyChecked.cmmn <- getCmmn(uspp=spp.corr1[,sppCorr])
	spp.cmmn1 <- newlyChecked.cmmn
}

save(spp.cmmn1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.cmmn1.RData")


# ===========================================
# = Merge sppCorr and common names together =
# ===========================================
setkey(spp.cmmn1, sppCorr)
setkey(spp.corr1, sppCorr)
trawl.newSpp <- spp.corr1[unique(spp.cmmn1)]
# trawl.newSpp[,sum(is.na(sppCorr)&!is.na(spp))] # I get 0 ... not anymore, now I get 24
trawl.newSpp[!grepl("[a-zA-Z]", common)|common=="", common:=as.character(NA)] # remove any common names that don't contain english chars

# check for duplicates (arising because of different original "spp" value, but resolved to be same sppCorr & common values)
# actually, i need to keep those different original spp values! otherwise I can't match back to original data set :(
setkey(trawl.newSpp, spp, sppCorr, common)
trawl.newSpp <- unique(trawl.newSpp)


# ================================
# = Get Taxonomic Classification =
# ================================

if("taxLvl1.RData"%in%tax.files){
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxLvl1.RData")
	
	taxLvl1 <- getTax(sppCorr2=trawl.newSpp[,sppCorr], oldTax=taxLvl1)

	setkey(taxLvl1, sppCorr)
	taxLvl1 <- unique(taxLvl1)
}else{
	taxLvl1 <- getTax(sppCorr2=trawl.newSpp[,sppCorr])
	
	setkey(taxLvl1, sppCorr)
	taxLvl1 <- unique(taxLvl1)
}

save(taxLvl1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxLvl1.RData")


# ====================
# = Add Manual Names =
# ====================
manualTax <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spptaxonomy_2014-10-09_plusManual_no_spp._RDB.csv")
rmWhite(manualTax) # have to remove leading and trailing white space

setnames(manualTax, c("taxon", "name"), c("spp", "sppCorr"))

manualTax[,spp:=cullParen(cullSp(fixCase(cullExSpace(spp))))]

setkey(manualTax, spp)
manualTax <- unique(manualTax)

# check to make sure the same sppCorr doesn't match to more than 1 common name in the manual taxonomy
# checkManDupCmmn <- manualTax[,list(nCmmn=lu(common),cmmn=paste0(unique(common))),by="sppCorr"]
# checkManDupCmmn[nCmmn!=1]


# ========================================================================
# = Define intersections of information in automated and manual tax info =
# ========================================================================
trawl.newSpp[,c("defined","share.spp","share.sppCorr"):=list("auto",FALSE,FALSE)]
manualTax[,c("defined","share.spp","share.sppCorr"):=list("manu",FALSE,FALSE)]

shared.spp <- intersect(trawl.newSpp[,spp],manualTax[,spp])
trawl.newSpp[spp%in%shared.spp, share.spp:=TRUE]
manualTax[spp%in%shared.spp, share.spp:=TRUE]

shared.sppCorr <- intersect(trawl.newSpp[,sppCorr],manualTax[,sppCorr])
trawl.newSpp[sppCorr%in%shared.sppCorr, share.sppCorr:=TRUE]
manualTax[sppCorr%in%shared.sppCorr, share.sppCorr:=TRUE]


# ===============================
# = Combine Sources of tax info =
# ===============================
trawl.newSpp2 <- rbind(manualTax[,list(sppCorr, spp, common, defined, share.spp, share.sppCorr)], trawl.newSpp) # combine sources
trawl.newSpp2 <- trawl.newSpp2[!share.spp | (share.spp & defined=="manu"),] # remove auto-defs where manu-defs exist


# ======================================================================
# = Check for multiple common names per sppCorr after combine tax info =
# ======================================================================
# Check to see if need to replace auto-defined commons with manu-defined commons when share.spp==FALSE & share.sppCorr==TRUE and the two sources don't have the same common names
check2DupCmmn <- trawl.newSpp2[,list(nCmmn=lu(common),cmmn=paste0(unique(common))),by="sppCorr"]
if(check2DupCmmn[,any(nCmmn!=1)]){ # don't want to do this automatically b/c it's kinda weird, and I it's currently not necessary
	trawl.newSpp2 <- trawl.newSpp2[(share.sppCorr), common:=.SD[defined=="manu",unique(common)], by="sppCorr"]
}

# Check again, just to make sure it worked
check2DupCmmn2 <- trawl.newSpp2[,list(nCmmn=lu(common),cmmn=paste0(unique(common))),by="sppCorr"]
if(check2DupCmmn2[,any(nCmmn!=1)]){
	stop("Something terrible has happened! Well, just duplicate common names per sppCorr, and my correction for this failed ...")
}else{
	trawl.newSpp2 <- trawl.newSpp2[,list(sppCorr,spp,common)]
}


# ======================================
# = Update species names in trawl data =
# ======================================
setkey(trawl.newSpp2, spp)
setkey(trawl00, spp)
trawl0 <- merge(trawl00, trawl.newSpp2, all.x=TRUE, by="spp") #trawl[trawl.newSpp]


# ==================
# = Save Memory #2 =
# ==================
if(delOldTrawl){
	rm(list="trawl00")
}

trawl0[!is.na(sppCorr),spp:=sppCorr]
trawl0[,correctSpp:=!is.na(sppCorr)]
trawl0[,sppCorr:=NULL]

setkey(trawl0, spp)
# taxLvl1[,sum(is.na(sppCorr))] # this needs to be 0
setnames(taxLvl1, "sppCorr", "spp")
if(taxLvl1[,sum(is.na(spp))]==0){
	trawl0 <- merge(trawl0, taxLvl1, all.x=TRUE)
}else{
	print("NA's in sppCorr in taxLvl1 !!")
}



# =======================================
# = Save Compiled Taxonomic Information =
# =======================================
taxInfo <- trawl0[,list(spp, common, taxLvl, species, genus, family, order, class, superclass, subphylum, phylum, kingdom, raw.spp, isSpecies, correctSpp)]
setkeyv(taxInfo, names(taxInfo))
taxInfo <- unique(taxInfo)


# ===========================================
# = Merge in Rachel Taxa and Trophic Levels =
# ===========================================
# ==============================================
# = Read Rachel Taxa, Merge with Prior taxInfo =
# ==============================================
rachelTax00 <- fread("./trawl/Data/Taxonomy/rachelTaxa.csv", na.strings=c("NA","","?"))
rachelTax0 <- rachelTax00[!is.na(spp)]
rachelTax <- rachelTax0[,list(trophicDiet,trophicOrig,Picture,rachelSpp,rachelCommon,trophicLevel,trophicLevel.se,raw.spp)]
rmWhite(rachelTax) # remove leading and trailing white space


taxInfo[,raw.spp][!taxInfo[,raw.spp]%in%rachelTax0[,raw.spp]]

# Merge Rachel's taxonomy with taxInfo
taxInfo2 <- merge(taxInfo, rachelTax, by=c("raw.spp"), all=TRUE)


# ============================================================================
# = Perform Basic Checks on Rachel's Taxonomy Entries (few errors, but some) =
# ============================================================================
# Next, make sure that for a given `spp`, all of the rachelSpp are the same
spp2 <- taxInfo2[duplicated(spp)|duplicated(spp, fromLast=TRUE)][,list(spp,rachelSpp)]
spp2[,list(lu(spp), lu(rachelSpp))] # well, they're of different lengths, probably because rachel just left NA's if she thought the entered `spp` was correct; so it doesn't prove anything (wouldn't anyway, but if these were the same, it'd suggest she was consistent, although it wouldn't prove it)
uspp <- spp2[,unique(spp)] # there are 565 names that were entered multiple (>=2) times, presumably because various raw species names would have converged to the same `spp` after correction. Each of these 565 names thus presents *at least* 565 opportunities for Rachel to have made  mistake (not counting extra white space, because I trimmed that out).

# Loop through to make sure she entered taxonomy consistently
consistent.names <- logical(length(uspp))
for(i in 1:length(uspp)){
	consistent.names[i] <- spp2[spp==uspp[i],lu(rachelSpp)==1L]
}

spp2[spp%in%uspp[!consistent.names]] # found 3 typos; but in no case did she offer multiple alternatives to the same "spp"

# Fix Rachel Typos
taxInfo2[rachelSpp=="Apogon pseduomaculatus", rachelSpp:="Apogon pseudomaculatus"]
taxInfo2[rachelSpp=="Actinoscyhpia", rachelSpp:="Actinoscyphia"]
taxInfo2[rachelSpp=="Anuropus bathypelagiucs", rachelSpp:="Anuropus bathypelagicus"]


# ============================
# = Fix Random Rachel Errors =
# ============================
# I found this too – I don't want any punctuation
taxInfo2[rachelSpp=="Ancylopsetta (ommata) quadrocellata", rachelSpp:="Ancylopsetta quadrocem"]

# There was one critter, where for different raw.spp but same spp, Rachel either didn't enter the same thing for both, or she only entered the taxonomy for 1 which happened to be different from the other (i.e., I gave her 1 case that was blank and she didn't fill it in, or I gave her 1 case that was filled and she left blank). Either way, it's clear how to fix: if they're the same species they should have the same info, and there's only 1 non-NA option. Later when I combine down to trawl2 this fix would be implemented for 3 columns (common, taxLvl, phylum) but with a warning (in that case the logic is implemented for all possible combines, not just this particular `spp`). I am fixing surgically here for "Brisaster latifrons", with the intent of avoiding warnings later (kinda remove the impact of the warning if I see it every time)
for(i in names(taxInfo2)){
	topts <- taxInfo2[spp=="Brisaster latifrons", get(i)]
	if(any(is.na(topts)) & sum(!is.na(unique(topts)))==1){
		taxInfo2[spp=="Brisaster latifrons", c(i):=list(topts[!is.na(topts)])]
	}
}


# ====================================
# = Check Taxonomy for many-to-one's =
# ====================================
# One `spp` is related to many values in another column; e.g., multiple trophicLevel per species, or multiple `family` per `spp`
out <- taxInfo2[,lapply(.SD, lu), by="spp"]
out2 <- copy(out)

print(out[apply(out2[,c("spp"):=list(NULL)], 1, max)>1], nrow=Inf) # 565 cases where some of the information differs for a given `spp`
print(out[apply(out2[,c("spp","raw.spp"):=list(NULL)], 1, max)>1], nrow=Inf) # if you drop the raw.spp column, then there are only 38 cases where for a `spp` there is more than 1 unique value across rows for a given column, which implies that *at least* 527 `spp` originally had more than 1 (between 2 and 10) raw.spp name.
print(out[apply(out2[,c("spp","raw.spp","isSpecies"):=list(NULL)], 1, max)>1], nrow=Inf) # if we additionally remove the isSpecies column, we see that there are only 3 spp for which multiple column values were assigned for that same spp. 2 of these are multiple values under `Picture`, 1 of these is a case where 2 values were assigned under `trophicLevel`. Additionally, in these 3 cases isSpecies was not duplicated, which means that there were 35 instances where a single `spp` was given multiple isSpecies values.

# The many-to-1 relationship for raw.spp-to-spp is OK/ desired/ expected. The many-to-1 for isSpecies-to-spp is a little annoying (unexpected, but not terribly surprising), but not a big deal because I can simply recompute isSpecies on the new `spp`. The `Picture` issue is trivial, but surprising, and should be investigated manually (only 2 instances). The `trohpicLevel` issue is important and problematic, however, it is also easily investigated manually as there is only 1 isntance. Finally, it is important to note that `common`, `taxLvl`, and `phylum` were not duplicated for a given `spp`, which was the original problem I was investigating.


# ==================================
# = Correct Isolated many-to-one's =
# ==================================
# Multiple `Picture` per `spp`:
taxInfo2[spp%in%c("Acanella arbuscula", "Acanthephyra")]
# For both species there was a "y", and the non-"y" values were NA; if there's a picture for 1 instance of this `spp`, there's a picture for them all!
taxInfo2[spp%in%c("Acanella arbuscula", "Acanthephyra"), Picture:="y"]

# Multiple `trophicLevel` per `spp`:
taxInfo2[spp%in%c("Syngnathus floridae")]
# http://www.fishbase.org/Ecology/FishEcologySummary.php?StockCode=3496&GenusName=Syngnathus&SpeciesName=floridae
# the true value is 3.32
taxInfo2[spp%in%c("Syngnathus floridae"), trophicLevel:=3.32]


# ==========================================================
# = Update correctSpp to reflect trust in Rachel's entries =
# ==========================================================
# If it's a 2+ word "rachelSpp", or if isTRUE(correctSpp), then correctSpp:=TRUE
taxInfo2[correctSpp | is.species(rachelSpp), correctSpp:=TRUE] # if Rachel wrote 2 words, it's legit! I think there were only 3 cases where Rachel took something that wasn't a species according to correctSpp, and found its identity. I think her main contribution was not increasing the number of taxa; rather, it may have been to reduce it by taking outdated taxonomy and finding the appropriate name.

# Note on above: see the end of script; I'm now thinking we have 2557 verified species in the data set. I think before we were between 1900 and 2000. So after Rachel's efforts we have ~600 more species. That seems to contradict what I wrote above. Part of the difference could be that I had previously been relying on `taxLvl`, which I realized had several hundred entries that were `species` instead of `Species`.

# taxInfo2[!correctSpp&!is.na(spp)&isSpecies]


# ===================================================================
# = Overwrite `common` and `spp` where Rachel supplied alternatives =
# ===================================================================
# Insert Rachel's spp where needed
useRach.spp <- (taxInfo2[,spp]!=taxInfo2[,rachelSpp] | is.na(taxInfo2[,spp])) & !is.na(taxInfo2[,rachelSpp]) # there were 308 instances here before I removed leading and trailing whitespace, now there are only 290! ;) Spaces matter, Rachel! :) ... I did more things, and now it's down to 287.
# print(taxInfo2[useRach.spp,list(spp, rachelSpp)], nrow=Inf) # all of Rachel's proposed changes to `spp`; there are several cases where her changes result in a convergence of taxonomy
# When merging in rachel's species values, need to address issues where convergence of taxonomy could lead to discrepancies in the columns aside from `spp`. Hopefully most of these discrepancies are issues where one instance has an NA and the other does not.
for(i in names(taxInfo2)){
	topts <- taxInfo2[spp=="Brisaster latifrons", get(i)]
	if(any(is.na(topts)) & sum(!is.na(unique(topts)))==1){
		taxInfo2[spp=="Brisaster latifrons", c(i):=list(topts[!is.na(topts)])]
	}
}
taxInfo2[useRach.spp, spp:=rachelSpp]

# Insert Rachel's common names where needed
useRach.com <- (taxInfo2[,common]!=taxInfo2[,rachelCommon] | is.na(taxInfo2[,common])) & !is.na(taxInfo2[,rachelCommon]) # there were 718 instances here, now only 698 after removing white space
taxInfo2[useRach.com, common:=rachelCommon]

# Delete the old columns after their necessary elements have been used
taxInfo2[,c("rachelSpp", "rachelCommon"):=NULL]


# =======================
# = Recompute isSpecies =
# =======================
taxInfo2[,isSpecies:=is.species(spp)]


# ==================================================
# = Do an exhaustive many-to-one check and correct =
# ==================================================
# This phase will require manually deciding between conflicting entries (the is.na() vs !is.na() case is easy and automated, others done manually)

# First, check to see if for a saved file of manual corrections (when I first did this, there were 43)
if("taxProblemSolution.RData"%in%list.files("./trawl/Data/Taxonomy")){
	load("./trawl/Data/Taxonomy/taxProblemSolution.RData")
	tPS <- TRUE # the tPS logic will be used in the loops to figure out if taxProblemSolution exists
}else{
	taxProblemSolution <- data.table(spp=character(), prob.col=character(), solution=character())
	tPS <- FALSE
}

# Copy taxInfo2 into taxInfo3, and use key
# note that I have run into some very odd errors with index something like taxInfo[spp=="Genus species"] and it returnign some `spp` that are *not* "Genus species". If I flanked the logic with a `&!is.na(spp)`, or if I used a key, I avoided this weird result (even when no `spp` in the entire data set were NA). I don't know why this was happening. But that's why I'm using a key here.
taxInfo3 <- copy(taxInfo2)
setkey(taxInfo3, spp)

# Identify columns to check as being "many's" in the many-to-one
col2check <- names(taxInfo3)[!names(taxInfo3)%in%c("raw.spp","spp","rachelSpp","rachelCommon","isSpecies")]

# Set up vectors to track progress for fixing, failing, or not needing to fix many-to-one for each `spp`
# I.e., replacementsMade+replacementsFailed+replacementUnneeded == taxInfo3[,lu(spp)] ... in theory (at least before I created the taxProblemSolution file)
replacementsMade <- 0
replacementsFailed <- 0
replacementUnneeded <- 0

# Begin looping: i's loop through unique `spp`, j's loop through the different columns that should only have 1 unique value per `spp`, but which might have more, in which case I will make manual corrections such that there will subsequently be only 1 unique value in that column per `spp`
for(i in taxInfo3[,unique(spp)]){
	for(j in col2check){
		t.out <- taxInfo3[i,get(j)] # temporary value for a given `spp` and the j column
		if(lu(t.out)>1){
			if(lu(t.out[!is.na(t.out)])==1){
				replacementsMade <- replacementsMade + 1L
				taxInfo3[i,c(j):=list(unique(t.out[!is.na(t.out)]))]
			}else{
				prob.spp <- i # prob. means "problem" / "problematic"
				prob.col <- j
				prob.opts <- unique(t.out[!is.na(t.out)])
				

				fix.taxProb <- function(){ # defining in function in loop for readability
					readline(paste("Pick your solution. SKIP to skip, otherwise enter one of the following:\n ", paste0(prob.opts, collapse="\n  "), "\n"))
				}
				
				if(tPS){
					prob.fix.t <- taxProblemSolution[spp==i & prob.col==j,solution]
					if(length(prob.fix.t)>0){
						prob.fix <- prob.fix.t
					}else{
						print(j)
						print(taxInfo3[i])
						prob.fix <- fix.taxProb()
						taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
					}
				}else{
					print(j)
					print(taxInfo3[i])
					prob.fix <- fix.taxProb()
					taxProblemSolution <- rbind(taxProblemSolution, data.table(spp=i, prob.col=j, solution=prob.fix))
				}
				
				
				if(prob.fix!="SKIP"){ # I've never tested the use of the SKIP response ...
					taxInfo3[i,c(j):=list(prob.fix)]
					replacementsMade <- replacementsMade + 1L
				}else{
					replacementsFailed <- replacementsFailed + 1L
				}
				
			}
		}else{
			replacementUnneeded <- replacementUnneeded + 1L
		}
	}
	
}

# Save Solutions to many-to-one problems in the taxonomy
save(taxProblemSolution, file="./trawl/Data/Taxonomy/taxProblemSolution.RData") # save it!

# Save the taxonomy
taxInfo <- taxInfo3 # renaming so that loading taxInfo gives you the object taxInfo (I don't want to create confusing versions, and I want the file name to match the object name)
save(taxInfo, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxInfo.RData")

# TODO Still need to incorporate the rest of Rachel's taxonomy ... phylum, etc. Haven't done that, but I also don't think it matters much (very few things changed, I believe)


# =================================================================
# = Recreate trawl0 after adding in Rachel's Taxonomy and Trophic =
# =================================================================
trawl.notTax <- bquote(list(
	year,
	datetime,
	haulid,
	stratum,
	stratumarea,
	lat,
	lon,
	depth,
	stemp,
	btemp,
	wtcpue,
	cntcpue,
	region,
	s.reg,
	raw.spp
))
trawl0 <- merge(trawl0[,eval(trawl.notTax)], taxInfo, all.x=TRUE, by=c("raw.spp"))


# ===========================
# = Trim trawl columns down =
# ===========================
trawl4 <- trawl0[,list(region, s.reg, phylum, spp, taxLvl, common, year, datetime, haulid, stratum, lat, lon, depth, stemp, btemp, wtcpue, cntcpue, trophicLevel, trophicLevel.se, trophicDiet, trophicOrig, Picture, correctSpp)] # this is where I drop all of the other pieces of taxonomic information
setkey(trawl4, s.reg, taxLvl, phylum, spp, year, stratum)
trawl4[,depth:=as.numeric(depth)]




# ==================
# = Save Memory #3 =
# ==================
if(delOldTrawl){
	rm(list="trawl0")
}



# ====================
# = Fix date formats =
# ====================
# regular expression patterns
pat2y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{2})(?=\\s|$)" # for dates like 6/23/07 or 06/5/07 or 06/05/07
pat4y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{4})(?=\\s|$)" # for dates like 6/23/2007, or 06/23/2007, etc
pat4y.only <- "^(\\d{4})$" # for dates that are just the year, e.g., 2007

trawl4[,datetime:=gsub(pat2y, "20\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/07 for 2007-6-23

trawl4[,datetime:=gsub(pat4y, "\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/2007 for 2007-6-23

trawl4[,datetime:=gsub(pat4y.only, "\\1-01-01", datetime, perl=TRUE)] # e.g., switch out 2007 for 2007-01-01


# ======================
# = Add POSIX to trawl =
# ======================
trawl.posix <- data.table(datetime=trawl4[,unique(datetime)], datetimeP=as.POSIXct(trawl4[,unique(datetime)], tz="GMT"))
setkey(trawl.posix, datetime)
setkey(trawl4, datetime)
trawl3 <- merge(trawl4, trawl.posix, all.x=TRUE)
trawl3[,datetime:=datetimeP]
trawl3 <- trawl3[,j=names(trawl3)[names(trawl3)!="datetimeP"], with=FALSE]


# =================================================
# = Add Replicates (new version of haulid, kinda) =
# =================================================
trawl3[,haulid:=paste0(round(roundGrid(lat,1/3),3),round(roundGrid(lon,1/3),3)), by=c("s.reg","year","stratum")]
trawl3[,K:=as.integer(as.factor(haulid)), by=c("s.reg","year","stratum")]


# ==================
# = Save Memory #4 =
# ==================
if(delOldTrawl){
	rm(list="trawl4")
}


# ==================================
# = Prepare Padding by Aggregating =
# ==================================
# this could probably be made much faster by using lapply at the end, but that's a little difficult b/c different columns require different functions
trawl2 <- trawl3[, # this aggregates among multiple hauls within the same substratum (K); 
	{
		ucom <- unique(common)
		if(length(ucom)>1){
			if(sum(!is.na(ucom))>1){ # if the length is >1 due to non-NA's
				print(unique(spp))
				print(ucom)
				stop("trying to add too many common names – match of species name to multiple commons")
			}else{
				common <- ucom[!is.na(ucom)]
				warning(paste(ucom, unique(spp), "trying to add too many common names – match of species name to multiple commons; if NA's are dropped, 1 unqiue common. Replacing NA's with unique non-NA value", sep=" -- "))
			}	
		}
		
		utax <- unique(taxLvl)
		if(length(utax)>1){
			if(sum(!is.na(utax))>1){ # if the length is >1 due to non-NA's
				print(unique(spp))
				print(utax)
				stop("trying to add too many taxLvl – match of species name to multiple taxonomic classifications")
			}else{
				taxLvl <- utax[!is.na(utax)]
				warning(paste(utax, unique(spp), "trying to add too many taxLvl – match of species name to multiple taxonomic classifications; if NA's are dropped, 1 unqiue taxLvl. Replacing NA's with unique non-NA value", sep=" -- "))
			}	
			
		}
		
		uphy <- unique(phylum)
		if(length(uphy)>1){
			if(sum(!is.na(uphy))>1){ # if the length is >1 due to non-NA's
				print(unique(spp))
				print(uphy)
				stop("trying to add too many phylum – match of species name to multiple taxonomic classifications")
			}else{
				phylum <- uphy[!is.na(uphy)]
				warning(paste(uphy, unique(spp),"trying to add too many phylum – match of species name to multiple taxonomic classifications; if NA's are dropped, 1 unqiue phylum. Replacing NA's with unique non-NA value", sep=" -- "))
			}
			
		}
		
		if(lu(common)>1 | lu(phylum)>1 | lu(taxLvl)>1){
			stop("crap; not successful in dropping NA levels when non-NA level of common, pnylum ,or taxLvl exists.")
		}
		
		# OK, create condensed output list
		list(
			# datetime=as.POSIXct(mean(datetime, na.rm=TRUE), tz="GMT", origin="1970-01-01 00:00.00 GMT"),
			# lat=roundGrid(mean(lat, na.rm=TRUE)),
			# lon=roundGrid(mean(lon, na.rm=TRUE)),
			lat=mean(lat, na.rm=TRUE),
			lon=mean(lon, na.rm=TRUE),
			depth=mean(depth, na.rm=TRUE), 
			stemp=meanna(stemp), 
			btemp=meanna(btemp), 
			wtcpue=meanna(wtcpue), 
			trophicLevel=mean(trophicLevel, na.rm=TRUE),
			trophicLevel.se=mean(trophicLevel.se, na.rm=TRUE),
			correctSpp=any(correctSpp),
			# common=.SD[correctSpp,unique(common)],
			# taxLvl=.SD[correctSpp,unique(taxLvl)]
			common=unique(common),
			taxLvl=unique(taxLvl),
			phylum=unique(phylum)	
		) 
	},
	by=c("region", "s.reg", "year", "stratum", "K", "spp")
] # note that sometimes wtcpue is 0 when cntcpue is non-0, hence why you can have normal numerics for depth and temp even though there is 0 cpue (which would seemingly imply a no-obs, but that's not necessarily true)
setkey(trawl2, s.reg, year, stratum, K, phylum, spp)


# ==================
# = Save Memory #5 =
# ==================
if(delOldTrawl){
	rm(list="trawl3")
}

# ======================
# = Save the raw trawl =
# ======================
setkey(trawl2, s.reg, year, stratum, K, phylum, spp)
save(trawl2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl2.RData")



# =================================================================================
# = Notes on the Number of Species in the Data Set (evening counting is tricky!!) =
# =================================================================================
# trawl2[,lu(spp)]
# [1] 3624
#
#
# trawl2[(correctSpp),lu(spp)]
# [1] 3600
#
#
# trawl2[(correctSpp)&is.species(spp),lu(spp)]
# [1] 2571
#
# trawl2[(correctSpp)&taxLvl=="Species",lu(spp)]
# [1] 1856
#
# trawl2[(correctSpp)&taxLvl!="Species"&is.species(spp),lu(spp)]
# [1] 261 # note: I checked, and these really do look like actual species; not a glitch
#
# trawl2[(correctSpp)&taxLvl=="Species"&is.species(spp),lu(spp)]
# [1] 1855 # note that adding the `is.species` restriction only reduces the # of species by 1 ... somewhere "Species" was entered for a taxLvl that was only 1 word, and thus highly unlikely to be identified to species
#
# trawl2[is.na(taxLvl),lu(spp)]
# [1] 620 # lots of times the taxLvl wasn't identified (mostly from taxize, some from rachel maybe)
#
# trawl2[(correctSpp)&is.na(taxLvl)&is.species(spp),lu(spp)]
# [1] 455 # if we say that the species had to be identified as "correct" (either confirmed by taxize or manual checking [i.e., found it on the web somewhere] ... only applies to if the identifier is valid, not actually if ID'd to spp), and if we focus on the cases where taxLvl is not informative (NA), and then we say that additionally that the `spp` has to be at least 2 words, then there are 455 of these cases.
#
# 1855 + 261 + 455 # if we sum up all 2+ word identifiers that were manually (website by me, rachel, or malin) or automatically (taxize), we cover all 3 cases of what taxLvl could be (is Species, not Species, is.na()), and thus we get the number of identifiers of that are 2 words and confirmed (i.e., `trawl2[(correctSpp)&is.species(spp),lu(spp)]`)
#
# trawl2[(correctSpp)&(taxLvl=="Species"|taxLvl=="species")&is.species(spp),lu(spp)]
# [1] 2102 # accounting for the instances where taxLvl could be Species or species, we see that speces accounts for a few hundred more taxa, so that makes sense ...
#
# trawl2[(correctSpp)&(taxLvl=="Species"|taxLvl=="species"|is.na(taxLvl))&is.species(spp),lu(spp)]
# [1] 2557 # we get really close to the 2571 count if we say that taxLvl has to say Species, species, or be uninformative. This means that there are 2571-2557 = 14 instances where taxLvl says something other than S/species (or nothing), but it seems like this could really be a species. Any guesses?
#
# trawl2[(correctSpp)&!(taxLvl=="Species"|taxLvl=="species"|is.na(taxLvl))&is.species(spp),lu(spp)]
# [1] 14 # see previous/ above
#
# > trawl2[(correctSpp)&!(taxLvl=="Species"|taxLvl=="species"|is.na(taxLvl))&is.species(spp),unique(spp)]
#  [1] "Parthenope agonus" "Aplatophis chauliodus" "Astrocyclus caecilia" "Fasciolaria lilium"
#  [5] "Busycon candelabrum" "Opisthognathus lonchurus" "Rochinia crassa" "Coelorinchus caelorinchus"
#  [9] "Stomias boa ferox" "Paralepis coregonoides borealis" "Notoscopelus elongatus kroyeri" "Pilumnus pannosus"
# [13] "Hyperoglyphe perciformes" "Chlamys hastata hericia" # here we see the 14 species meeting those criteria.... but what were the taxLvl's??
#
# unique(trawl2[(correctSpp)&!(taxLvl=="Species"|taxLvl=="species"|is.na(taxLvl))&is.species(spp),list(taxLvl, spp)])
#         taxLvl                             spp
#  1:      genus               Parthenope agonus
#  2:      genus           Aplatophis chauliodus
#  3:      genus            Astrocyclus caecilia
#  4:      genus              Fasciolaria lilium
#  5:      genus             Busycon candelabrum
#  6:      genus        Opisthognathus lonchurus
#  7:      genus                 Rochinia crassa
#  8: Subspecies       Coelorinchus caelorinchus
#  9: Subspecies               Stomias boa ferox
# 10: Subspecies Paralepis coregonoides borealis
# 11: Subspecies  Notoscopelus elongatus kroyeri
# 12:      genus               Pilumnus pannosus
# 13:      order        Hyperoglyphe perciformes
# 14: Subspecies         Chlamys hastata hericia
#
# # And there it is! several of them were ID'd as subspecies. It might be that the other taxLvl values were the most specific taxonomic levels that I was able to grab from taxize, but then rachel confirmed the taxa, and didn't add the new taxLvl, or that