
#' FWF Fread
#' 
#' Fast read for fixed width format files
#' 
#' @param \code{...} file names to open
#' @param \code{cols} vector of integer column widths
#' @param column_types character vector indicating classes of columns
#' @param column_names column_names character vector of column headers
#' 
#' @details uses data.table and LaF packages. The read is performed entirely by \code{\link{laf_open_fwf}}, but the output is converted to a data.table.
#' 
#' @return a data.table
#' @export fread.fwf
fread.fwf <- function(..., cols, column_types, column_names){
	if (!requireNamespace("LaF", quietly = TRUE)) {
		stop("LaF needed for this function to work. Please install it.", call. = FALSE)
	}
	if(missing(cols)){
		cols <- c(3,4,4,3) # for newf data
	}
	
	if(missing(column_types)){
		column_types <- rep("character", length(cols))
	}
	if(missing(column_names)){
		column_names <- paste0("V", seq_len(length(cols)))
	}
	
	laf <- LaF::laf_open_fwf(..., column_widths=cols, column_types=column_types, column_names=column_names) #
	# laf_open_fwf("inst/extdata/newf.zip", column_widths=cols, column_types=column_types, column_names=column_names)
	laf <- as.data.table(laf[,])
	return(laf)
}

# fread.fwf("newf-stratum_areas.ver2.fwf")




# ===================================
# = Function to read files from zip =
# ===================================
#' Read Zip
#' 
#' Read in all files contained in a zip file
#' 
#' @param zipfile name and path of a zip file containing files to read in
#' @param pattern regular expression indicating the file extensions to be read. Default is .csv files
#' @param SIMPLIFY logical, compress the contents into 2 data.table on output? To be safe, set FALSE
#' @param use.fwf logical, indicating whether or not to read in as fixed width file; is overridden if file extensions are .fwf
#' @param \code{...} other arguments to be passed on; in particular, the \code{cols} argument of column width if a fixed width file
#' 
#' @details 
#' The function works by unzipping a file into a temporary directory, reading in all the files in that temporarily unzipped folder, and then deleting the temporary folder and all of its contents. Thus, is a handy way of keep all of your files stored as compressed files, but being able to access them. Obviously comes at the performance cost of having to do all of the unzipping and deleting.
#' Uses data.table's \code{\link{fread}} in most cases, except for .fwf formats, in which case \code{\link{fread.fwf}} is used.
#' 
#' @return a data.table, or list of data.tables. The name of each element of the list is the name of the file within the .zip file.
#' 
#' @export read.zip
read.zip <- function(zipfile, pattern="\\.csv$", SIMPLIFY=TRUE, use.fwf=FALSE, ...){

	# Create a name for the dir where we'll unzip
	zipdir <- tempfile()
	
	# Number of file types to be read
	n.pat <- length(pattern)
	
	# Create the dir using that name
	dir.create(zipdir)
	
	# Unzip the file into the dir
	unzip(zipfile, exdir=zipdir)
	
	# Set up a list to store output for
	# different file types, if needed
	if(n.pat>1){
		data.out <- list()
		if(length(SIMPLIFY)!=n.pat){
			stopifnot(length(SIMPLIFY)==1)
			SIMPLIFY <- rep(SIMPLIFY, n.pat)
		}
		if(length(use.fwf)!=n.pat){
			stopifnot(length(use.fwf)==1)
			use.fwf <- rep(use.fwf, n.pat)
		}
	}
	
	# Loop through different file types
	for(i in 1:n.pat){
		
		# Get a list of csv files in the dir
		files <- list.files(zipdir, rec=TRUE, pattern=pattern[i])
		
		if(grepl("\\.fwf$", pattern[i]) | use.fwf[i]){
			fread2 <- fread.fwf
		}else{
			fread2 <- function(..., cols)fread(...)
		}
		
		# Create a list of the imported files
		if(SIMPLIFY[i]){
			file.data <- sapply(files, 
				function(f){
				    fp <- file.path(zipdir, f)
					dat <- fread2(fp, ...)
				    return(dat)
				}
			)
		}else{
			file.data <- lapply(files, 
				function(f){
				    fp <- file.path(zipdir, f)
					dat <- fread2(fp, ...)
				    return(dat)
				}
			)
		}
		
		# Use csv names to name list elements
		names(file.data) <- basename(files)
		
		# Modify output depending on whether we're reading in different file types
		if(n.pat>1){
			data.out[[i]] <- file.data
		}else{
			data.out <- file.data
		}
	}
	
	
	# Delete temporary folder
	unlink(zipdir)
	
	# Return data
	return(data.out)
}

# data.fwf <- read.zip("newf.zip", pattern=c("\\.fwf$", "\\.DAT"), SIMPLIFY=FALSE)