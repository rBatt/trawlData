# ===================
# = Raw Data Sets =
# ===================
#' Raw Aleutian Islands
#' 
#' Raw data set for the Aleutian Islands bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.ai"




#' Raw Eastern Berring Sea
#' 
#' Raw data set for the Eastern Berring Sea bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.ebs"




#' Raw Gulf of Mexico
#' 
#' Raw data set for the Gulf of Mexico bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.gmex"




#' Raw Gulf of Alaska
#' 
#' Raw data set for the Gulf of Alaska bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.goa"




#' Raw Northeast US
#' 
#' Raw data set for the Northeast US bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.neus"




#' Raw Newfoundland
#' 
#' Raw data set for the Newfoundland bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.newf"




#' Raw South Atlantic
#' 
#' Raw data set for the US Southeast bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.sa"




#' Raw Southern Gulf of St. Lawrence
#' 
#' Raw data set for the Southern Gulf of St. Lawrence bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.sgulf"




#' Raw Scotian Shelf
#' 
#' Raw data set for the Scotian Shelf bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.shelf"




#' Raw West Coast Annual
#' 
#' Raw data set for the West Coast Annual bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.wcann"




#' Raw West Coast Triennial
#' 
#' Raw data set for the West Coast Triennial bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"raw.wctri"

# ===================
# = Clean Data Sets =
# ===================
#' Clean Aleutian Islands
#' 
#' Clean data set for the Aleutian Islands bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.ai"




#' Clean Eastern Berring Sea
#' 
#' Clean data set for the Eastern Berring Sea bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.ebs"




#' Clean Gulf of Mexico
#' 
#' Clean data set for the Gulf of Mexico bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.gmex"




#' Clean Gulf of Alaska
#' 
#' Clean data set for the Gulf of Alaska bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.goa"




#' Clean Northeast US
#' 
#' Clean data set for the Northeast US bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.neus"




#' Clean Newfoundland
#' 
#' Clean data set for the Newfoundland bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.newf"




#' Clean South Atlantic
#' 
#' Clean data set for the US Southeast bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.sa"




#' Clean Southern Gulf of St. Lawrence
#' 
#' Clean data set for the Southern Gulf of St. Lawrence bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.sgulf"




#' Clean Scotian Shelf
#' 
#' Clean data set for the Scotian Shelf bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.shelf"




#' Clean West Coast Annual
#' 
#' Clean data set for the West Coast Annual bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.wcann"




#' Clean West Coast Triennial
#' 
#' Clean data set for the West Coast Triennial bottom trawl survey
#' 
#' 
#' 
#' @format A data.table
"clean.wctri"



# =================================
# = Environmental/ Misc Data Sets =
# =================================
#' ETOPO Depth
#' 
#' Water depth gridded from the ETOPO data source
#' 
#' @format raster brick
"depth"

#' HadISST Surface Temperatures
#' 
#' Surface temperatures from the HadISST data source. Includes terrestrial and aquatic. Gridded.
#' 
#' The full data set is in \code{sst}; \code{sst.ann} has annual averages; \code{sst.mu} is the long-term temporal mean of the annual averages (so 1 value per grid cell).
#' 
#' @format raster brick
#' @name HadISST
NULL

#' @rdname HadISST
"sst"

#' @rdname HadISST
"sst.ann"

#' @rdname HadISST
"sst.mu"


#' SODA
#' Gridded SODA bottom temperatures  
#' 
#' From 1958 to 2008. Longitude: 200W to 20E. Latitude: 0N to 90N.  
#' 
#' Can be the full data set (\code{soda}), or the annual maximum or mean (\code{soda.annMax} and \code{soda.annMean}, respectively). Furthermore, \code{soda.annMax.mu} and \code{soda.annMean.mu} represent the long-term averages of their variables, such that "Max.mu" indicates the among-year average of within-year maxima, and "Mean.mu" similarly indicates the among-year average of the within-year average. Note that in the typical case where each year is comprised of the same number of observations, the long-term average could be taken all at once, rather than first being average within a year.
#' 
#' @format raster brick
#' @name SODA
NULL

#' @rdname SODA
"soda"

#' @rdname SODA
"soda.annMax"

#' @rdname SODA
"soda.annMax.mu"

#' @rdname SODA
"soda.annMean"

#' @rdname SODA
"soda.annMean.mu"



# =======================
# = Taxonomic Data Sets =
# =======================
#' Common Names
#' Species' common names matched to scientific names
#' @seealso \code{\link{getCmmn}} \code{\link{getSpp}} \code{\link{getTax}}
"getCmmnData"

#' Spp Names
#' Scientific names matched to raw taxonomic name entries
#' @seealso \code{\link{getCmmn}} \code{\link{getSpp}} \code{\link{getTax}}
"getSppData"

#' Taxonomic Classification
#' Taxonomic classification matched to scientific names
#' @seealso \code{\link{getCmmn}} \code{\link{getSpp}} \code{\link{getTax}}
"getTaxData"

#' Corrected Species Names (legacy)
#' Species names that had been corrected in older version of code
#' Used to construct spp.key initially, but should no longer be needed. Provided for reproducibility of older results regarding taxonomy.
#' Now superceded by \code{\link{getSppData}}. It is recommended that \code{\link{spp.key}} be used for taxonomic reference.
#' @seealso \code{\link{getCmmn}} \code{\link{getSpp}} \code{\link{getTax}} \code{\link{spp.key}}
"spp.corr1"

#' Taxonomic Information (legacy)
#' Taxonomic classification and ecological information
#' This is the data set manually checked by Rachel Berman.
#' Superceded by \code{\link{spp.key}}.
"taxInfo"

#' Species Key
#' Key to taxonomic and ecological information for all species surveyed
#' This data.table is the desirable key to use for species names. Relates survey-entered taxonomic identifiers to true scientific names and other related information.
#' This key was partially derived from the \code{\link{taxInfo}} data set checked by Rachel Berman. However, taxonomy for this data set seems to be an ongoing process, and is always improving. Thus, \code{spp.key} should represent an improvement, and will continue to improve. Check for updates.
#' To match the values in this key to taxonomic data, one can use \code{merge} using \code{by="ref"}; alternatively, \code{trawlData::match.tbl} may also be convenient for grabbing specific values.
"spp.key"