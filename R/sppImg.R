#' Species Image
#' 
#' Plot an image of a species from the package database of images
#' 
#' @param x the name of the species (Genus species)
#' @param common The species' common name; if NULL, no common name printed
#' @param ... Arguments to be passed to \code{\link{image}}
#' 
#' @return
#' Numeric vector of length two indicating the number of pixes horizontally and vertically; or if no match, returns NULL and prints a message.
#' 
#' @examples
#' library(rgdal)
#' dev.new(width=8, height=7)
#' par(mar=c(0.5,0.5,1,0.5), oma=c(0,0,0,0), ps=8, cex=1, mfrow=c(3,3))
#' clean.ebs[Picture=="y"][pick(spp,9),
#' 	sppImg(
#' 		unique(spp),
#' 		unique(common),
#' 		side=3,
#' 		adj=0.1,
#' 		line=-0.85,
#' 		xpd=TRUE
#' 	),
#' 	by="spp"
#' ]
#' 
#' @export
sppImg <- function(x, common=NULL, ...){
	if(!requireNamespace("rgdal", quietly = TRUE)){
		message("Please install package 'rgdal' via install.packages('rgdal')")
	}
	# library(rgdal)
	imgs <- list.files(file.path(system.file(package="trawlData"),"extdata/taxPictures"), full.names=TRUE)
	imgs_spp <- gsub("copy|\\.[a-z]*$", "", basename(imgs), ignore.case=TRUE)
	imgs_spp <- gsub("_", " ", imgs_spp)
	mtch <- match(x, imgs_spp)
	if(is.na(mtch)){
		message(paste("No match found for",x))
		return(NULL)
	}
	img <- suppressWarnings(rgdal::readGDAL(imgs[mtch]))
	image(img, red=1, green=2, blue=3)
	dims <- img@grid@cells.dim
	
	
	# cin <- par()$cin
# 	pin <- par()$pin
# 	char1 <- (dims/pin)*cin
	nms <- paste(c(x, common), collapse="\n")
	# text(0, dims[2]-char1[2]*1.5, labels=nms, col=col, pos=4)
	# text(0, dims[2]-char1[2]*1.5, labels=nms,col="black", pos=4)
	
	mtext(nms, ...)
	
	
	return(dims)
}

