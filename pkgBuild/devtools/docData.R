#' Document Data
#' 
#' Semi-Automated Data Set Documentation
#' 
#' @param x A data set
#' @param title The data set title; if left NULL (default), uses name of object
#' @param desc The data set descript, if left NULL, default, uses name of object
#' @param idh character vector specifying each column's definition; when left as NULL (default) tries to match columns to \code{gen.cols}, a set of general column definitions. If no match is found when \code{idh=NULL}, definition is set to "insert description here"
#' @param clean If TRUE, uses \code{\link(clean.names)} to clean names before trying to match them to the general definitions in \code{gen.cols}
#' @param reg Character specifying region name to be passed to \code{\link{clean.names}}
#' @param append logical, when FALSE (default) output is sent to console; if TRUE, searches for a file called "datasets.R", and appends the documentation to the end of the file
#' 
#' @details
#' Output can go to file or to console. Automated definitions are generic, and regions might have important deviations from general definitions, so caution is recommended.
#' 
#' @returns
#' Invisibly returns a character string that can be used to generate documentation. As a side effect, \code{\link{cat}} is called on this character string. Furthermore, A file named datasets.R may be modified.
#' 
#' @examples
#' docData(raw.ai, title="AI Survey Raw Data", clean=TRUE, reg="ai")

docData <- function(x, title=NULL, desc=NULL, idh=NULL, clean=FALSE, reg=NULL, append=FALSE){
	x.name <- gsub(".*x \\= ([a-zA-z0-9]*[\\.]{0,1}[a-zA-Z0-9]*)[,|\\)].*", "\\1", deparse(match.call())[1], perl=T)
	x.col.names <- names(x)
	stopifnot(!is.null(x.col.names))
	stopifnot(!is.null(dim(x)) & length(dim(x))==2)
	
	nc <- ncol(x)
	nr <- nrow(x)
	if(!is.null(dim(x))){
		dim.desc <- paste("dim =", paste(dim(x),collapse=" x "))
	}else if(!is.null(length(x))){
		dim.desc <- paste("length =",length(x))
	}
	
	x.class <- class(x)
	x.col.class <- sapply(x, class)
	
	# is there an idh ("insert description here")?
	# if so, use it
	if(!is.null(idh) & length(idh)==nc){
		txt <- idh
	}else{
		if(!is.null(idh)){
			warning("idh description not the same length as number of columns; using place-holder description")
		}
		txt <- rep("insert_description_here", nc)
		txt[x.col.names%in%names(gen.cols)] <- gen.cols[x.col.names[x.col.names%in%names(gen.cols)]]
		
		if(clean){
			if(is.null(reg)){
				message("Need to supply reg to convert column names to clean version for definition matching")
			}else{
				txt <- match_txt(x, x.col.names, reg, txt)
				txt[x.col.names%in%names(gen.cols)] <- gen.cols[x.col.names[x.col.names%in%names(gen.cols)]]
			}
		}
		
	}
	
	if(is.null(title)){
		title <- x.name
	}
	if(is.null(desc)){
		desc <- x.name
	}
	rc <- "#' "
	head.txt <- paste0(rc,title, "  \n", rc, "  \n", rc, desc, "  \n", rc, "  \n", rc, "@format\n", rc, "A ", dim.desc, " ", paste(x.class,collapse=" "), ":  \n")
	
	tab.start <-paste0(rc, "\\tabular{rlll}{\n")
	tab.meat <- c()
	for(i in 1:nc){
		# ind <- paste("[,", formatC(i, width=nchar(nc)), "]")
		c.start <- paste0(rc, "[,", i, "]")
		tn <- x.col.names[i]
		tc <- x.col.class[i]
		c.end <- "\\cr"
		tab.meat[i] <- paste(c(paste(c(c.start, tn, paste0(tc, ""), txt[i]), collapse=" \\tab "),c.end,"\n"), collapse="")
	}
	tail.txt <- paste0("\"",x.name,"\"")
	
	txt.out <- paste(c(head.txt,tab.start, tab.meat, rc, "}\n", tail.txt, "\n\n\n"), collapse="")
	
	if(append){sink("R/datasets.R", append=TRUE, type="output")}
	cat(txt.out)
	if(append){sink(NULL)}
	invisible(txt.out)

}

match_txt <- function(x, nms, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri"), txt){
	reg <- match.arg(reg)
	
	nms_match_ind <- !nms%in%names(gen.cols)
	nms2match <- nms[nms_match_ind]
	tiny <- copy(x[1])
	clean.names(tiny, reg)
	tiny_match_ind <- names(tiny)%in%names(gen.cols)
	match_ind <- nms_match_ind & tiny_match_ind
	match_ind_nms <- names(tiny)[match_ind]
	txt[match_ind] <- (gen.cols[names(gen.cols)%in%match_ind_nms])[match_ind_nms]
	
	txt[match_ind] <- paste0("cleans up to \\code{", match_ind_nms, "}, ", txt[match_ind])
	
	txt
	
}


#' @describeIn docData General Column Definitions
"gen.cols"
gen.cols <- c(
	# region information
	"reg" = "survey region",
	
	# time information
	"year" = "year of haul", 
	"datetime" = "the day and time of the haul",
	"timezone" = "time zone",
	"time" = "starting time of the tow",
	"date" = "date of the tow",
	
	# species names
	"ref" = "reference taxonomic ID from raw data",
	"spp" = "species scientific name; Genus species",
	"common" = "the common name of the organism sampled",
	"genus" = "the genus of the species",
	"species" = "the species name of the species",
	
	# haul ID info
	"BGSID" = "record ID number", # from gmex
	"SID" = "species identification number",
	"vessel" = "vessel ID",
	"cruise" = "cruise ID",
	"haul" = "the integer haul number within a cruise", # does same description apply beyond just AI?
	"haulid" = "a unique identifier for the haul; vessel ID - cruise ID - haul number", 
	"stratum" = "the statistical stratum of the haul",
	"station"= "the station ID for the haul" ,
	"CRUISE_NO" = "cruise ID as number",
	
	# Method info
	"BGSCODE" = "flags information about the biological sample",
	"gearsize" = "the dimension of the gear; for trawl net, the width of the mouth in ft",
	"geartype" = "code for the type of gear used",
	"meshsize" = "the size of the net mesh (inches of stretch)",
	"duration" = "duration of the haul (how long the net was being towed)",
	"towspeed" = "the speed of the vessel",
	"areaswept" = "area sweap by the net (an index of effort)",
	"towduration" = "the duration (time) for the tow",
	"effort" = "some measure of the effort for this catch; could be area, time, speed, net size, or some combination. When possible, was converted to area swept by net during the entire tow",
	
	# location info
	"stratumarea" = "the area of the statistical stratum (km2)",
	"stratumarea2" = "the area of the statistical stratum (nmi2)", 
	"lat" = "latitude of the haul", 
	"lon" = "longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)",
	"lat.deg" = "latitude of the haul; degree component",
	"lat.min" = "latitude of the haul; minutes component",
	"lon.deg" = "longitude of the haul; degree component",
	"lon.min" = "longitude of the haul; minutes component",
	"depth" = "the maximum depth of the water at the location of the haul",
	"depth2"= "depth of the haul",
	"NPFMCArea" = "region name",
	"Subarea Description" = "subarea name",
	"StratumCode" = "matches STRATUM column in bio data set",
	"OldStratumCode" = "stratum code from older classification system (only neus)",
	"DepthIntervalm" = "range of depths for the stratum, in meters",
	
	# environmental info
	"stemp" = "water temperature at the surface at the location of the haul", 
	"btemp" = "water temperature at the bottom at the location of the haul",
	"sdo" = "surface dissolved oxygen",
	"bdo" = "bottom dissolved oxygen",
	"ssalin" = "surface salinity",
	"bsalin" = "bottom salinity",
	"airtemp" = "air temperature",
	"wind" = "wind speed (?)",
	
	
	
	
	# species measurements
	"wtcpue" = "weight (mass) of the catch", 
	"cntcpue"="number of individuals caught per hectare in the haul",
	"cnt" = "number of individuals in the whole net (may be extrapolated)",
	"weight"="the weight (mass) of all items in the net (may be extrapolated)",
	"length" = "length of catch (typically of an individual, but may be average for some regions)",
	"NUMLEN" = "number of individuals used to ascertain catch length",
	
	
	# biological info
	"trophicDiet" = "source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source",
	"trophicOrig" = "from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?",
	"Picture" = "Is there a picture of this critter assoicated with the package? Note: this isn't always accurate",
	"trophicLevel" = "the trophic level from Fish Base or Sea Life Base",
	"trophicLevel.se" = "the standard error of the trophic level",
	"sex" = "the gender of the catch",
	
	
	# taxonomy info
	"taxLvl" = "the most specific level of classification indicated by spp",
	"family" = "taxonomic family",
	"order" = "taxonomic order", 
	"class" = "taxonomic class", 
	"superclass" = "taxonomic superclass",
	"subphylum" = "taxonomic subphylum",
	"kingdom" = "taxonomic kingdom", 
	"tax.src" = "informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet",
	"tax.src2" = "informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)",
	"conflict" = "for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking",
	"flag" = "flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key",
	"website" = "URL reference for taxonomic information",
	"website2" = "secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name",
	"keep.row" = "Column indicating whether or not the row show likely be excluded",
	"row_flag" = "if keep.row is TRUE, why was this row flagged to be dropped?",
	"val.src" = "indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \\code{\\link{cull}}) needed to be adjusted, m3 indicates that \\code{\\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match",
	"tbl.row" = "the row in the taxonomic data base where a match was found",
	"mtch.src" = "the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3"

)