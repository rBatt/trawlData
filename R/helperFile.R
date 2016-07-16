
#' FWF Fread
#' 
#' Fast read for fixed width format files
#' 
#' @param ... file names to open
#' @param cols vector of integer column widths
#' @param column_types character vector indicating classes of columns
#' @param column_names column_names character vector of column headers
#' 
#' @details uses data.table and LaF packages. The read is performed entirely by \code{LaF:laf_open_fwf}, but the output is converted to a data.table.
#' 
#' @return a data.table
#' 
#' @seealso \code{\link{read.zip}} \code{\link{read.trawl}}
#' 
#' @export fread.fwf
fread.fwf <- function(..., cols, column_types, column_names){
	# if(!requireNamespace("LaF", quietly = TRUE)){
	# 	stop("LaF needed for this function to work. Please install it.", call. = FALSE)
	# }
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
#' @param ... other arguments to be passed on; in particular, the \code{cols} argument of column width if a fixed width file
#' 
#' @details 
#' The function works by unzipping a file into a temporary directory, reading in all the files in that temporarily unzipped folder, and then deleting the temporary folder and all of its contents. Thus, is a handy way of keep all of your files stored as compressed files, but being able to access them. Obviously comes at the performance cost of having to do all of the unzipping and deleting.
#' No longer uses data.table's \code{\link{fread}} because of a long-standing issue with quoted elements (comment fields in raw data files often have strange characters); now using \code{\link{read.csv}}. For .fwf formats \code{\link{fread.fwf}} is used.
#' 
#' @return a data.table, or list of data.tables. The name of each element of the list is the name of the file within the .zip file.
#' 
#' @seealso \code{\link{fread.fwf}} \code{\link{read.trawl}}
#' 
#' @export read.zip
read.zip <- function(zipfile, pattern="\\.csv$", SIMPLIFY=TRUE, use.fwf=FALSE, ...){

	zipdir <- tempfile()# Create a name for the dir where we'll unzip
	n.pat <- length(pattern)# Number of file types to be read	
	dir.create(zipdir)# Create the dir using that name
	unzip(zipfile, exdir=zipdir)# Unzip the file into the dir
	
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
		files <- list.files(zipdir, recursive=TRUE, pattern=pattern[i])
		
		# determine read function
		if(grepl("\\.fwf$", pattern[i]) | use.fwf[i]){
			read_func <- fread.fwf # needed for newfoundland format
		}else{
			read_func <- function(fp, ..., cols){as.data.table(read.csv(fp, ...))} # all other regs
		}
		
		# Create a list of the imported files
		fp <- sapply(files, function(x,y)file.path(y,x), y=zipdir) #file.path(zipdir, f)
		if(SIMPLIFY[i]){
			file.data <- sapply(fp, read_func, ...)
		}else{
			file.data <- lapply(fp, read_func, ...)
		}
		
		# Modify output depending on whether we're reading in different file types
		names(file.data) <- basename(files) # Use csv names to name list elements
		if(n.pat>1){
			data.out[[i]] <- file.data
		}else{
			data.out <- file.data
		}
	} # end loop through patterns
	
	# clean up and return
	unlink(zipdir)# Delete temporary folder
	return(data.out)# Return data
}

# data.fwf <- read.zip("newf.zip", pattern=c("\\.fwf$", "\\.DAT"), SIMPLIFY=FALSE)