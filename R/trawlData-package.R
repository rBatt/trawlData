#' trawlData
#' 
#' The trawlData package emphasizes an organized and reliable presentation of bottom trawl survey data. The package includes the functions used to read and clean these data, as well as the read-in and cleaned versions of the raw data set. Also included in the package are gridded data sets of environmental variables, and several functions designed to enhance manipulation and exploration of these data.
#' 
#' @section Regions
#' Most of the data sets follow a convention of \code{prefix.___}, where \code{prefix} is either "raw" or "clean", and \code{___} is the abbreviation of a region name. The region names are:  
#' \tabular{ll}{
#' 	ai \tab Aleutian Islands \cr
#' 	ebs \tab Eastern Bering Sea \cr
#' 	gmex \tab Gulf of Mexico \cr
#' 	goa \tab Gulf of Alaska \cr
#' 	neus \tab Northeast US \cr
#' 	newf \tab Newfoundland \cr
#' 	sa \tab US South Atlantic \cr
#' 	sgulf \tab Southern Gulf of St. Lawrence \cr
#' 	shelf \tab Scotian Shelf \cr
#' wcann \tab West Coast Annual \cr
#' wctri \tab West Coast Triennial \cr
#' }
#' 
#' @section Raw Data
#' The truly raw data sets (from the data provider) can be found with a call to \code{system.file(package="trawlData")}. Explore the zip files in inst (except for neus, which is not zipped). For the raw data that have been already read into R, just check out \code{raw.___}. To read these data into R yourself, try \code{\link{read.trawl}}.
#' 
#' @section Clean Data
#' Cleaned data sets take the form of \code{clean.___}. These files are the equivalently to sequentially processing a raw data set with \code{\link{clean.names}}, \code{\link{clean.format}}, \code{\link{clean.columns}}, \code{\link{clean.tax}}, and \code{\link{clean.trimRow}}. Unlike the other cleaning functions, there is 1 additional cleaning function that actually results in loss of information relative to the original data set: \code{\link{clean.trimCol}}.
#' 
#' @section Gridded Environmental Data
#' There are surface temperature data sets in \code{\link{HadISST}}, and bottom temperatures in \code{\link{soda}}. Gridded depth data can be found in \code{\link{depth}} (from ETOPO). Coming soon will be a rugosity data set (stay tuned!)
#' 
#' @section Taxonomy
#' There are actually several taxonomy-related data sets, but most of them are aimed at producing our diamond in the rough: \code{\link{spp.key}}. There is also a .csv version of this data set that can be found in 'inst' (again, use \code{\link{system.file}}). If you find any corrections that need to be made here, please, let us know by [opening an Issue on GitHub](https://github.com/rBatt/trawlData)! Or email Ryan Batt. Better yet, create a pull request with the appropriate edits in the .csv ^_^
#' 
#' @section Data Manipulation
#' There are only a few of these. For aggregating data, check out \code{\link{trawlAgg}}. For "casting" data (e.g., \code{\link{reshape2::acast}}), see \code{\link{trawlCast}} (note: this is the function you want for adding 0's for unoserved species into the data set). To combine the tasks accomplished by \code{\link{clean.trimCol}} and \code{\link{clean.trimRow}} simulataneously for multiple regions (and combining the result of those regions into 1 data.table), see \code{\link{trawlTrim}}.
#' 
#' 
#' @section Helper Functions
#' There are many, but I'll list a few that I use a lot. First up are \code{\link{pick}} and \code{\link{mpick}}: together, these functions can do amazing things for helping you get good test data sets! At times \code{\link{mpick}} can be slow as it uses a brute-force approach when it has to; if you have a better approach, post it as an answer to my long and unpopular [question on SO](http://stackoverflow.com/q/33714985/2343633)!
#' 
#' Another handy function is \code{\link{match.tbl}}. When \code{exact=TRUE} it behaves similarly to \code{\link{match}}; otherwise, it tries a bunch of approximations. Due to the input-output format, it has often able to clean up sections of my code, making it much more reliable and easy to read. I often use it when trying to pull in information from multiple sources before merging it into a master data.table (is used heavily in creating and maintaining \code{\link{spp.key}}).
#' 
#' On the simpler end of things, I use \code{\link{lu}} very often; although this (and \code{\link{una}}) are getting replaced by functionality in data.table \code{\link{data.table::uniqueN}} (that's a newer function in 1.9.6 or 1.9.7, which aren't compatible with this package at the moment). For gridding data, I use \code{\link{ll2strat}} and \code{\link{roundGrid}}.
#' 
#' Finally, if you use data.table a lot (which you will be with this package!), you'll notice that you often have a character vector that you need to evaluate in the middle of a complex \code{j=} expression. Yeah, you can use \code{with=FALSE}, but then you lose a lot of functionality elsewhere. Enter \code{\link{s2c}}, which I often use in the form of \code{dt[,eval(s2c(character_vector))]}. You'll get a list out of it, so be mindful of that! Anyway, very handy.
#' 
#' @section Plotting
#' Currently, there's only 1 plotting function, although more will be forthcoming. At least \code{\link{sppImg}} is a fun one :)


