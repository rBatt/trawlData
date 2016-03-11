update_dataset_doc <- function(){

	library(trawlData)


	docData(raw.ai, title="Raw Aleutian Islands", desc="Raw data from the Aleutian Islands bottom trawl", clean=TRUE, reg="ai", append=T)

	docData(raw.ebs, title="Raw Eastern Berring Sea", desc="Raw data set for the Eastern Berring Sea bottom trawl survey", clean=TRUE, reg="ebs", append=T)

	docData(raw.gmex, title="Raw Gulf of Mexico", desc="Raw data set for the Gulf of Mexico bottom trawl survey", clean=TRUE, reg="gmex", append=T)

	docData(raw.goa, title="Raw Gulf of Alaska", desc="Raw data set for the Gulf of Alaska bottom trawl survey", clean=TRUE, reg="goa", append=T)
	
	docData(raw.neus, title="Raw Northeast US", desc="Raw data set for the Northeast US bottom trawl survey", clean=TRUE, reg="neus", append=T)
	
	docData(raw.newf, title="Raw Newfoundland", desc="Raw data set for the Newfoundland bottom trawl survey", clean=TRUE, reg="newf", append=T)
	
	docData(raw.sa, title="Raw South Atlantic (Southeast US)", desc="Raw data set for the South Atlantic (Southeast US) bottom trawl survey", reg="sa", clean=TRUE,  append=T)
	
	docData(raw.sgulf, title="Raw Southern Gulf of St. Lawrence", desc="Raw data set for the Southern Gulf of St. Lawrence bottom trawl survey", clean=TRUE, reg="sgulf", append=T)
	
	docData(raw.shelf, title="Raw Scotian Shelf", desc="Raw data set for the Scotian Shelf bottom trawl survey", clean=TRUE, reg="shelf", append=T)
	
	docData(raw.wcann, title="Raw West Coast Annual", desc="Raw data set for the West Coast Annual bottom trawl survey", clean=TRUE, reg="wcann", append=T)
	
	docData(raw.wctri, title="Raw West Coast Triennial", desc="Raw data set for the West Coast Triennial bottom trawl survey", clean=TRUE, reg="wctri", append=T)
	
	
	docData(clean.ai, title="Clean Aleutian Islands", desc="Clean data from the Aleutian Islands bottom trawl",  append=T)

	docData(clean.ebs, title="Clean Eastern Berring Sea", desc="Clean data set for the Eastern Berring Sea bottom trawl survey",  append=T)

	docData(clean.gmex, title="Clean Gulf of Mexico", desc="Clean data set for the Gulf of Mexico bottom trawl survey",  append=T)

	docData(clean.goa, title="Clean Gulf of Alaska", desc="Clean data set for the Gulf of Alaska bottom trawl survey",  append=T)
	
	docData(clean.neus, title="Clean Northeast US", desc="Clean data set for the Northeast US bottom trawl survey",  append=T)
	
	docData(clean.newf, title="Clean Newfoundland", desc="Clean data set for the Newfoundland bottom trawl survey",  append=T)
	
		docData(clean.sa, title="Clean South Atlantic (Southeast US)", desc="Clean data set for the South Atlantic (Southeast US) bottom trawl survey",  append=T)
	
	docData(clean.sgulf, title="Clean Southern Gulf of St. Lawrence", desc="Clean data set for the Southern Gulf of St. Lawrence bottom trawl survey",  append=T)
	
	docData(clean.shelf, title="Clean Scotian Shelf", desc="Clean data set for the Scotian Shelf bottom trawl survey",  append=T)
	
	docData(clean.wcann, title="Clean West Coast Annual", desc="Clean data set for the West Coast Annual bottom trawl survey",  append=T)
	
	docData(clean.wctri, title="Clean West Coast Triennial", desc="Clean data set for the West Coast Triennial bottom trawl survey",  append=T)
	
	docData(getCmmnData, title="Common Names", desc="Species's common names matched to scientific names", append=TRUE)
	docData(getSppData, title="Spp Names", desc="Scientific names matched to raw taxonomic name entries", append=TRUE)
	docData(getTaxData, title="Taxonomic Classification", desc="Taxonomic classification matched to scientific names", append=TRUE)
	docData(spp.corr1, title="Corrected Species Names (legacy)", desc="Species names that had been corrected in older version of code", append=TRUE)
	docData(taxInfo, title="Taxonomic Information (legacy)", desc="Taxonomic classification and ecological information", append=TRUE)
	docData(spp.key, title="Species Key", "Key to taxonomic and ecological information for all species surveyed", append=TRUE)

	gridded_data <- 
"
#' ETOPO Depth
#' 
#' Water depth gridded from the ETOPO data source
#' 
#' @format raster brick
\"depth\"

#' HadISST Surface Temperatures
#' 
#' Surface temperatures from the HadISST data source. Includes terrestrial and aquatic. Gridded.
#' 
#' The full data set is in \\code{sst}; \\code{sst.ann} has annual averages; \\code{sst.mu} is the long-term temporal mean of the annual averages (so 1 value per grid cell).
#' 
#' @format raster brick
#' @name HadISST
NULL

#' @rdname HadISST
\"sst\"

#' @rdname HadISST
\"sst.ann\"

#' @rdname HadISST
\"sst.mu\"


#' SODA
#' Gridded SODA bottom temperatures  
#' 
#' From 1958 to 2008. Longitude: 200W to 20E. Latitude: 0N to 90N.  
#' 
#' Can be the full data set (\\code{soda}), or the annual maximum or mean (\\code{soda.annMax} and \\code{soda.annMean}, respectively). Furthermore, \\code{soda.annMax.mu} and \\code{soda.annMean.mu} represent the long-term averages of their variables, such that \"Max.mu\" indicates the among-year average of within-year maxima, and \"Mean.mu\" similarly indicates the among-year average of the within-year average. Note that in the typical case where each year is comprised of the same number of observations, the long-term average could be taken all at once, rather than first being average within a year.
#' 
#' @examples
# Example from bselden: https://gist.github.com/bselden/81c139c3c1dcfe89d225
# Example uses depth (ETOPO) and btemp data (soda)
# note: need to avoid 'at' symbol to document using roxygen, hence slot()
#
#' depth.200 <- depth
#' d2v <- slot(slot(depth.200,'data'),'values')
#' # Set depths below 200m or above sea level to NA, shallower to 1
#' one_logic <- d2v <=0 & d2v >=-200
#' slot(slot(depth.200,'data'),'values')[one_logic] <- 1
#' slot(slot(depth.200,'data'),'values')[!one_logic] <- NA
#'
#' temps.crop <- crop(soda.annMean, extent(depth.200)) # crop soda data to region for which depth data exist
#' plot(temps.crop[[1]])
#'
#' # match up the resolution of depth and temp
#' # depth is coarser, so take its max as agg funct
#' t_res <- res(temps.crop)
#' d_res <- res(depth.200)
#' depth.200.coarse <- aggregate(depth.200, fact=(t_res/d_res), fun=max)
#' plot(depth.200.coarse)
#'
#' # multiply the temperature by the 0m-200m depth logic
#' # i.e., multiply temps outside the 0m-200m depth range by NA, else by 1
#' temp.200 <- overlay(temps.crop, depth.200.coarse, fun="*")
#' names(temp.200) <- names(temps.crop)
#' plot(temp.200[[1]])
#' 
#' @format raster brick
#' @name SODA
NULL

#' @rdname SODA
\"soda\"

#' @rdname SODA
\"soda.annMax\"

#' @rdname SODA
\"soda.annMax.mu\"

#' @rdname SODA
\"soda.annMean\"

#' @rdname SODA
\"soda.annMean.mu\"

#' Rugosity
#' 
#' Rugosity and depth data
#' 
#' @format raster brick
\"rugosity\"

"
	
	sink("R/datasets.R", append=TRUE)
	cat(gridded_data)
	sink(NULL)
	
}
