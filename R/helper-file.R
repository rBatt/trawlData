

fread.fwf <- function(..., cols){
	# library(magrittr)
	if(missing(cols)){
		cols <- c(3,4,4,3) # for newf data
	}
	data.initial <- fread(..., sep="\n", header=F)

	end_col <- cumsum(cols)
	start_col <- end_col - cols + 1
	start_end <- cbind(start_col, end_col) # matrix of start and end positions
 
	text <- data.initial[ , apply(start_end, 1, function(y) substr(V1, y[1], y[2]))] %>% data.table(.)
	
	return(text)
}

# fread.fwf("newf-stratum_areas.ver2.fwf")




# ===================================
# = Function to read files from zip =
# ===================================
read.zip <- function(zipfile, pattern="\\.csv$", SIMPLIFY=TRUE, use.fwf=FALSE, ...){
	
	# dots <- list(...)
	# colClasses <- dots$colClasses
	# drop <- dots$drop
	# select <- dots$select
	# print(select)
	
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
			# fread2 <- function(..., cols, colClasses, drop, select)fread.fwf(...,colClasses=colClasses[[i]], drop=drop[[i]], select=select[[i]])
		}else{
			fread2 <- function(..., cols)fread(...)
			# fread2 <- function(..., cols, colClasses, drop, select)fread(...,colClasses=colClasses[[i]], drop=drop[[i]], select=select[[i]])
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