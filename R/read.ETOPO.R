
# NOAA depth data
# http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/netCDF/
# in netCDF format
# found nice guidelines here: https://www.image.ucar.edu/GSP/Software/Netcdf/
# however, it doesn't appear to be necessary for me to download the netcdf libraries
# In fact, install.packages("netcdf") worked fine on my iMac and on amphiprion server (Linux)
# Ah, but reading my notes for read.HadISST, I see that I can use the same method here. I'd forgotten I'd used raster/ brick()
# http://stackoverflow.com/questions/24115110/importing-sea-surface-temperature-text-files-in-ascii-format-into-r/24115597#24115597


# =================
# = Load Packages =
# =================
# library(ncdf)
library(raster)


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# =============
# = Load data =
# =============
depth00 <- raster("./trawl/Data/raw_data/depthData/ETOPO2v2c_f4.nc")


# ============================
# = 3) Trim brick by lat/lon =
# ============================
# NOTE: This is copied from read.HadISST.R
# Do Latitude trimming
newExtent <- extent(c(-180, 180, 20, 65)) # define the new "extent" for latitude trimming
depth01 <- crop(depth00, newExtent) # trim latitude

# Do longitude trimming
# Note that I just matched the trimming used for HadISST; the *30 you see is b/c these are 2 minute data, and HadISST was 1º
depth02 <- as.array(depth01) # change to array for easy manipulation
depth02[,1:(360*30),] <- as.array(depth01)[,c((180*30+1):(360*30),1:(180*30)),] # everything east of the PM, put to the west, yielding WH coords
depth03 <- brick(depth02, xmn=-(360), xmx=0, ymn=(20), ymx=(65)) # convert to a raster brick, defining coordinates
names(depth03) <- names(depth01) # get old names
depth0 <- crop(depth03, extent(c(-190, -40, 20, 65))) # trim longitude
depth <- depth0


# ========
# = Save =
# ========
save(depth, file="./trawl/Data/ETOPO.RData")


# library(rasterVis)
# levelplot(depth0, par.settings=PuOrTheme())
# levelplot(depth0, par.settings=rasterTheme())
