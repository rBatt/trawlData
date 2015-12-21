
# ===========================
# = Load Necessary Packages =
# ===========================
library(raster)
library(ncdf)
library(ncdf4)


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))


# =======================
# = Names of SODA Files =
# =======================
# soda.files <- c(
# 	"./trawl/Data/SODA/soda_temp_180W-20W_10N-85N_1958-1978.nc",
# 	"./trawl/Data/SODA/soda_temp_180W-20W_10N-85N_1979-1999.nc",
# 	"./trawl/Data/SODA/soda_temp_180W-20W_10N-85N_2000-2008.nc",
# )

# soda.files <- c(
# 	"./trawl/Data/SODA/soda_temp_160E-20W_0N-90N_1958-1968.nc",
# 	"./trawl/Data/SODA/soda_temp_160E-20W_0N-90N_1969-1979.nc",
# 	"./trawl/Data/SODA/soda_temp_160E-20W_0N-90N_1980-1990.nc",
# 	"./trawl/Data/SODA/soda_temp_160E-20W_0N-90N_1991-2001.nc",
# 	"./trawl/Data/SODA/soda_temp_160E-20W_0N-90N_2002-2008.nc"
# )

soda.files <- c(
	"./trawl/Data/SODA/soda_temp_200W-20E_0N-90N_1958-1968.nc",
	"./trawl/Data/SODA/soda_temp_200W-20E_0N-90N_1969-1979.nc",
	"./trawl/Data/SODA/soda_temp_200W-20E_0N-90N_1980-1990.nc",
	"./trawl/Data/SODA/soda_temp_200W-20E_0N-90N_1991-2001.nc",
	"./trawl/Data/SODA/soda_temp_200W-20E_0N-90N_2002-2008.nc"
)


# =========================================
# = Function to Read in SODA, Grab Bottom =
# =========================================
get.soda <- function(file){

	soda.info <- open.ncdf(file)
	name.soda.sizes <- sapply(soda.info$var$temp$dim, function(x)x$name)
	soda.sizes <- soda.info$var$temp$size
	dim.units <- sapply(soda.info$var$temp$dim, function(x)x$units)
	print(dim.units)
	stopifnot(grepl("months since ", dim.units[4])) # make sure time is in correct units and in right place
	names(soda.sizes) <- name.soda.sizes
	ntime <- soda.sizes["time"]
	ndepth <- soda.sizes["depth"]

	soda.time0 <- soda.info$var$temp$dim[[4]]$vals
	ref.date <- as.Date(gsub("months since ", "", dim.units[4]))
	start.before.ref <- grepl("-", soda.time0[1]) # is the first date before ref.date?
	n.month.before <- ceiling(abs(soda.time0[1])) + as.integer(start.before.ref)
	start.increment <- ifelse(start.before.ref, "-1 month", "1 month")
	time.start <- rev(seq.Date(ref.date, by=start.increment, length.out=n.month.before))[1]
	soda.time <- seq.Date(time.start, by="1 month", length.out=ntime)


	pb <- txtProgressBar(min=1, max=ntime, style=3)
	for(i in 1:ntime){
		t.soda <- brick(file, lvar=4, level=i)
		# need to switch missing value to actual NA # TODO is this automatically done by brick()? I think so.
		
		soda.depths <- as.numeric(gsub("X", "", names(t.soda))) # does weird rounding that I don't understand
	
		# get the deepest available temperature in each gridd cell
		t.soda.bot <- do.call(cover, unstack(subset(t.soda, length(soda.depths):1)))
		names(t.soda.bot) <- soda.time[i]
		# the subsetting piece flips the order of the layers (so that deepest is first layer)
		# by unstacking I reformat from raster with layers, to list of rasters
		# that list can be used as the list of arguments to a function taking ...
		# cover keeps the values in the first object of the ..., and replaces the NA's with values from the second ...
		# that process repeats until through all object listed in the ...
		# in other words, the final value will be the value in the first layer that has a non-NA value in that cell
		
		# accumulate over time periods (monthly in original data sets I'm using)
		# this will be for the first time period (1958-1978)
		if(i==1){
			soda.bot <- t.soda.bot
		}else{
			soda.bot <- addLayer(soda.bot, t.soda.bot)
		}
		setTxtProgressBar(pb, i)
	}
	close(pb)
	
	return(soda.bot)
	
}


# ===============================================
# = Read in the SODA Files, Get Bottom For Each =
# ===============================================
soda.list <- vector("list", length(soda.files))
for(i in 1:length(soda.files)){
	soda.list[[i]] <- get.soda(soda.files[i])
}


# =================================================
# = Make Sure Coordinates Are Correct in WH Units =
# =================================================
fixExtent <- function(x){
	if(xmin(x) > 0 & xmax(x) > 0 ){
		xmin(x) <- xmin(x) - 360
		xmax(x) <- xmax(x) - 360
	}
	
	if(xmin(x) < 0 & xmax(x) > 0 ){
		xmin(x) <- xmin(x) - 180
		xmax(x) <- xmax(x) - 180
	}
	
	x
}

for(i in 1:length(soda.files)){
	soda.list[[i]] <- fixExtent(soda.list[[i]])
}


# ====================
# = Combine All SODA =
# ====================
soda <- do.call(stack, soda.list)
soda.years <- regmatches(names(soda),regexpr("[0-9]{4}", names(soda)))


# ===============
# = Aggregate 1 =
# ===============
# Take annual maximum of the monthly means
soda.annMax <- stackApply(soda, as.numeric(as.factor(soda.years)), max, na.rm=TRUE)
names(soda.annMax) <- unique(soda.years)

# Take annual mean of the monthly means
soda.annMean <- stackApply(soda, as.numeric(as.factor(soda.years)), mean, na.rm=TRUE)
names(soda.annMean) <- unique(soda.years)


# ===============
# = Aggregate 2 =
# ===============
# Take the grand mean of the annual max/mean
soda.annMax.mu <- calc(soda.annMax, mean)
soda.annMean.mu <- calc(soda.annMean, mean)

blah <- calc(soda.annMax, timeSlope)


# ========
# = Save =
# ========
# save(soda, file="./trawl/Data/SODA/soda_180W-20W_10N-85N_1958-2008.RData")
# save(soda.annMax, soda.annMax.mu, file="./trawl/Data/SODA/soda.annMax_180W-20W_10N-85N_1958-2008.RData")
# save(soda.annMean, soda.annMean.mu, file="./trawl/Data/SODA/soda.annMean_180W-20W_10N-85N_1958-2008.RData")

save(soda, file="./trawl/Data/SODA/soda_200W-20E_0N-90N_1958-2008.RData")
save(soda.annMax, soda.annMax.mu, file="./trawl/Data/SODA/soda.annMax_200W-20E_0N-90N_1958-2008.RData")
save(soda.annMean, soda.annMean.mu, file="./trawl/Data/SODA/soda.annMean_200W-20E_0N-90N_1958-2008.RData")


