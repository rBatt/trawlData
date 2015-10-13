
# Read in and trim HadISST (sea surf temp)

# ==================
# = Load Libraries =
# ==================
# http://stackoverflow.com/a/24115597/2343633
library(raster)
# library(xts)
# library(caTools)


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# =====================
# = 1) Read the brick =
# =====================
sst000 <- brick("./trawl/Data/raw_data/HadISST/HadISST_sst.nc")


# =========================
# = 2) Trim brick by date =
# =========================
# Define desired starting and stopping Dates
startYear <- 1968   # start of the period
endYear <- 2013     # end of the period
subp <- '1968-01-01/2012-12-01'   # period for the climatology calculation

# Define dates in brick
Date <- substr(names(sst000),2,11) # get brick dates
Date <- gsub('\\.', '\\-', Date) # format brick dates by replacing "." with "-"
Date <- as.Date(Date) # format brick dates as a date object

# Starting date
dstart.pat <- paste(startYear,'01','[0-9]{2}',sep='-') # define the start date pattern to be used in a regular expression
dstart <- grep(dstart.pat, as.character(Date)) # find the starting date pattern in the brick dates

# Ending date
dend.pat <- paste(endYear,'12','[0-9]{2}',sep='-') #define the end date pattern for the regex
dend <- grep(dend.pat, Date) # find the ending date pattern in the brick dates

# Trim by date
sst00 <- subset(sst000, dstart:dend) # subset the brick to the dates within (inclusively) the start and end dates
Date <- Date[dstart:dend] # trim the brick dates to match the brick


# ============================
# = 3) Trim brick by lat/lon =
# ============================
# Do Latitude trimming
newExtent <- extent(c(-180, 180, 20, 65)) # define the new "extent" for latitude trimming
sst01 <- crop(sst00, newExtent) # trim latitude

# Do longitude trimming
sst02 <- as.array(sst01) # change to array for easy manipulation
sst02[,1:360,] <- as.array(sst01)[,c(181:360,1:180),] # everything east of the PM, put to the west, yielding WH coords
sst03 <- brick(sst02, xmn=-360, xmx=0, ymn=20, ymx=65) # convert to a raster brick, defining coordinates
names(sst03) <- names(sst01) # get old names (dates)
sst0 <- crop(sst03, extent(c(-190, -40, 20, 65))) # trim longitude


# ===============================
# = 4) Convert ice & land to NA =
# ===============================
# http://stackoverflow.com/questions/11966503/how-to-replace-nas-in-a-raster-object
sst <- reclassify(sst0, matrix(c(-1000, -32768, NA, NA), ncol=2))


# ===================
# = 5) Annual means =
# ===================
sst.ann <- stackApply(sst, indices=rep(1:(dim(sst)[3]/12),each=12), fun=mean)


# ============================
# = 6) Grand (temporal) mean =
# ============================
sst.mu <- stackApply(sst.ann, indices=rep(1, length(sst.ann)), fun=mean)


# plot(sst.mu)


# ================
# = Save Results =
# ================
save(sst, sst.ann, sst.mu, file="./trawl/Data/HadISST.RData")