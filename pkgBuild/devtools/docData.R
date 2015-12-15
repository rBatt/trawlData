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
	x.name <- gsub(".*x \\= (.*\\.[a-zA-Z0-9]*)[, |\\)].*", "\\1", deparse(match.call())[1], perl=T)
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
		txt[x.col.names%in%names(gen.cols)] <- gen.cols[names(gen.cols)%in%x.col.names]
		
		if(clean){
			if(is.null(reg)){
				message("Need to supply reg to convert column names to clean version for definition matching")
			}else{
				txt <- match_txt(x.col.names, reg, txt)
				txt[x.col.names%in%names(gen.cols)] <- gen.cols[names(gen.cols)%in%x.col.names]
			}
		}
		
	}
	
	if(is.null(title)){
		title <- x.name
	}
	if(is.null(desc)){
		desc <- x.name
	}
	head.txt <- paste0(title, "\n", desc, "\n", "@format\nA ", dim.desc, " ", paste(x.class,collapse=" "), ":  \n")
	
	tab.start <-"\\tabular{rlll}{\n"
	tab.meat <- c()
	for(i in 1:nc){
		# ind <- paste("[,", formatC(i, width=nchar(nc)), "]")
		c.start <- paste0("[,", i, "]")
		tn <- x.col.names[i]
		tc <- x.col.class[i]
		c.end <- "\\cr"
		tab.meat[i] <- paste(c(paste(c(c.start, tn, paste0(tc, ""), txt[i]), collapse=" \\tab "),c.end,"\n"), collapse="")
	}
	tail.txt <- paste0("\"",x.name,"\"")
	
	txt.out <- paste(c(head.txt,tab.start, tab.meat, "}\n",tail.txt), collapse="")
	
	cat(txt.out)
	invisible(txt.out)

}

match_txt <- function(nms, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri"), txt){
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
gen.cols <- c(
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
	
	# Method info
	"BGSCODE" = "flags information about the biological sample",
	"gearsize" = "the dimension of the gear; for trawl net, the width of the mouth in ft",
	"geartype" = "code for the type of gear used",
	"meshsize" = "the size of the net mesh (inches of stretch)",
	"duration" = "duration of the haul (how long the net was being towed)",
	"towspeed" = "the speed of the vessel",
	"areaswept" = "area sweap by the net (an index of effort)",
	
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
	
	# species measurements
	"wtcpue" = "weight (mass) of the catch", 
	"cntcpue"="number of individuals caught per hectare in the haul",
	"cnt" = "number of individuals in the whole net (may be extrapolated)",
	# "cnt_sample" = "number of individuals counted (may be a subsample)",
	"weight"="the weight (mass) of all items in the net (may be extrapolated)"#,
	# "weight_sample" = "the weight (mass) of the sample (may be subsampled)"
	
)