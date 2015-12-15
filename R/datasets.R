#' Raw Aleutian Islands  
#'   
#' Raw data from the Aleutian Islands bottom trawl  
#'   
#' @format
#' A dim = 118661 x 18 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab STRATUM \tab integer \tab cleans up to \code{stratum}, the statistical stratum of the haul\cr
#' [,2] \tab LATITUDE \tab numeric \tab cleans up to \code{lat}, latitude of the haul\cr
#' [,3] \tab LONGITUDE \tab numeric \tab cleans up to \code{lon}, longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,4] \tab STATION \tab character \tab cleans up to \code{station}, the station ID for the haul\cr
#' [,5] \tab YEAR \tab integer \tab cleans up to \code{year}, year of haul\cr
#' [,6] \tab DATETIME \tab character \tab cleans up to \code{datetime}, the day and time of the haul\cr
#' [,7] \tab WTCPUE \tab numeric \tab cleans up to \code{wtcpue}, weight (mass) of the catch\cr
#' [,8] \tab NUMCPUE \tab numeric \tab cleans up to \code{cntcpue}, number of individuals caught per hectare in the haul\cr
#' [,9] \tab COMMON \tab character \tab cleans up to \code{common}, the common name of the organism sampled\cr
#' [,10] \tab SCIENTIFIC \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,11] \tab SID \tab integer \tab species identification number\cr
#' [,12] \tab BOT_DEPTH \tab integer \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,13] \tab BOT_TEMP \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,14] \tab SURF_TEMP \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,15] \tab VESSEL \tab integer \tab cleans up to \code{vessel}, vessel ID\cr
#' [,16] \tab CRUISE \tab integer \tab cleans up to \code{cruise}, cruise ID\cr
#' [,17] \tab HAUL        \tab character \tab cleans up to \code{haul}, the integer haul number within a cruise\cr
#' [,18] \tab Areakm2 \tab integer \tab cleans up to \code{stratumarea}, the area of the statistical stratum (km2)\cr
#' }
"raw.ai"


#' Raw Eastern Berring Sea  
#'   
#' Raw data set for the Eastern Berring Sea bottom trawl survey  
#'   
#' @format
#' A dim = 380027 x 18 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab STRATUM \tab integer \tab cleans up to \code{stratum}, the statistical stratum of the haul\cr
#' [,2] \tab LATITUDE \tab numeric \tab cleans up to \code{lat}, latitude of the haul\cr
#' [,3] \tab LONGITUDE \tab numeric \tab cleans up to \code{lon}, longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,4] \tab STATION \tab character \tab cleans up to \code{station}, the station ID for the haul\cr
#' [,5] \tab YEAR \tab integer \tab cleans up to \code{year}, year of haul\cr
#' [,6] \tab DATETIME \tab character \tab cleans up to \code{datetime}, the day and time of the haul\cr
#' [,7] \tab WTCPUE \tab numeric \tab cleans up to \code{wtcpue}, weight (mass) of the catch\cr
#' [,8] \tab NUMCPUE \tab numeric \tab cleans up to \code{cntcpue}, number of individuals caught per hectare in the haul\cr
#' [,9] \tab COMMON \tab character \tab cleans up to \code{common}, the common name of the organism sampled\cr
#' [,10] \tab SCIENTIFIC \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,11] \tab SID \tab integer \tab species identification number\cr
#' [,12] \tab BOT_DEPTH \tab integer \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,13] \tab BOT_TEMP \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,14] \tab SURF_TEMP \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,15] \tab VESSEL \tab integer \tab cleans up to \code{vessel}, vessel ID\cr
#' [,16] \tab CRUISE \tab integer \tab cleans up to \code{cruise}, cruise ID\cr
#' [,17] \tab HAUL \tab character \tab cleans up to \code{haul}, the integer haul number within a cruise\cr
#' [,18] \tab Areakm2 \tab integer \tab cleans up to \code{stratumarea}, the area of the statistical stratum (km2)\cr
#' }
"raw.ebs"


#' Raw Gulf of Mexico  
#'   
#' Raw data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 716550 x 37 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab CRUISEID \tab integer \tab cleans up to \code{cruise}, cruise ID\cr
#' [,2] \tab VESSEL \tab integer \tab cleans up to \code{vessel}, vessel ID\cr
#' [,3] \tab BIO_BGS \tab character \tab cleans up to \code{SID}, species identification number\cr
#' [,4] \tab STATIONID \tab character \tab cleans up to \code{station}, the station ID for the haul\cr
#' [,5] \tab CRUISE_NO \tab numeric \tab cruise ID as number\cr
#' [,6] \tab P_STA_NO \tab numeric \tab insert_description_here\cr
#' [,7] \tab BGSID \tab integer \tab record ID number\cr
#' [,8] \tab CATEGORY \tab integer \tab insert_description_here\cr
#' [,9] \tab GENUS_BGS \tab character \tab cleans up to \code{genus}, the genus of the species\cr
#' [,10] \tab SPEC_BGS \tab character \tab cleans up to \code{species}, the species name of the species\cr
#' [,11] \tab BGSCODE \tab character \tab flags information about the biological sample\cr
#' [,12] \tab CNTEXP \tab integer \tab cleans up to \code{cnt}, number of individuals in the whole net (may be extrapolated)\cr
#' [,13] \tab SELECT_BGS \tab numeric \tab cleans up to \code{weight}, the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,14] \tab INVRECID \tab integer \tab insert_description_here\cr
#' [,15] \tab GEAR_SIZE \tab integer \tab cleans up to \code{gearsize}, the dimension of the gear; for trawl net, the width of the mouth in ft\cr
#' [,16] \tab GEAR_TYPE \tab character \tab cleans up to \code{geartype}, code for the type of gear used\cr
#' [,17] \tab MESH_SIZE \tab numeric \tab cleans up to \code{meshsize}, the size of the net mesh (inches of stretch)\cr
#' [,18] \tab OP \tab character \tab insert_description_here\cr
#' [,19] \tab MIN_FISH \tab integer \tab cleans up to \code{towduration}, the duration (time) for the tow\cr
#' [,20] \tab TIME_ZN \tab character \tab cleans up to \code{timezone}, time zone\cr
#' [,21] \tab TIME_MIL \tab character \tab cleans up to \code{time}, starting time of the tow\cr
#' [,22] \tab S_LATD \tab character \tab insert_description_here\cr
#' [,23] \tab S_LATM \tab numeric \tab insert_description_here\cr
#' [,24] \tab S_LOND \tab integer \tab insert_description_here\cr
#' [,25] \tab S_LONM \tab numeric \tab insert_description_here\cr
#' [,26] \tab DEPTH_SSTA \tab numeric \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,27] \tab MO_DAY_YR \tab character \tab cleans up to \code{date}, date of the tow\cr
#' [,28] \tab E_LATD \tab integer \tab insert_description_here\cr
#' [,29] \tab E_LATM \tab numeric \tab insert_description_here\cr
#' [,30] \tab E_LOND \tab integer \tab insert_description_here\cr
#' [,31] \tab E_LONM \tab numeric \tab insert_description_here\cr
#' [,32] \tab TEMP_SSURF \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,33] \tab TEMP_BOT \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,34] \tab VESSEL_SPD \tab numeric \tab cleans up to \code{towspeed}, the speed of the vessel\cr
#' [,35] \tab COMSTAT \tab character \tab insert_description_here\cr
#' [,36] \tab TAXONOMIC \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,37] \tab TITLE \tab character \tab insert_description_here\cr
#' }
"raw.gmex"


#' Raw Gulf of Alaska  
#'   
#' Raw data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 197026 x 18 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab STRATUM \tab integer \tab cleans up to \code{stratum}, the statistical stratum of the haul\cr
#' [,2] \tab LATITUDE \tab numeric \tab cleans up to \code{lat}, latitude of the haul\cr
#' [,3] \tab LONGITUDE \tab numeric \tab cleans up to \code{lon}, longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,4] \tab STATION \tab character \tab cleans up to \code{station}, the station ID for the haul\cr
#' [,5] \tab YEAR \tab integer \tab cleans up to \code{year}, year of haul\cr
#' [,6] \tab DATETIME \tab character \tab cleans up to \code{datetime}, the day and time of the haul\cr
#' [,7] \tab WTCPUE \tab numeric \tab cleans up to \code{wtcpue}, weight (mass) of the catch\cr
#' [,8] \tab NUMCPUE \tab numeric \tab cleans up to \code{cntcpue}, number of individuals caught per hectare in the haul\cr
#' [,9] \tab COMMON \tab character \tab cleans up to \code{common}, the common name of the organism sampled\cr
#' [,10] \tab SCIENTIFIC \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,11] \tab SID \tab character \tab species identification number\cr
#' [,12] \tab BOT_DEPTH \tab character \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,13] \tab BOT_TEMP \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,14] \tab SURF_TEMP \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,15] \tab VESSEL \tab numeric \tab cleans up to \code{vessel}, vessel ID\cr
#' [,16] \tab CRUISE \tab numeric \tab cleans up to \code{cruise}, cruise ID\cr
#' [,17] \tab HAUL \tab character \tab cleans up to \code{haul}, the integer haul number within a cruise\cr
#' [,18] \tab Areakm2 \tab integer \tab cleans up to \code{stratumarea}, the area of the statistical stratum (km2)\cr
#' }
"raw.goa"


#' Raw Northeast US  
#'   
#' Raw data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 2523907 x 25 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab CRUISE6 \tab integer \tab cleans up to \code{cruise}, cruise ID\cr
#' [,2] \tab STATION \tab integer \tab cleans up to \code{station}, the station ID for the haul\cr
#' [,3] \tab STRATUM \tab integer \tab cleans up to \code{stratum}, the statistical stratum of the haul\cr
#' [,4] \tab SVSPP \tab integer \tab cleans up to \code{SID}, species identification number\cr
#' [,5] \tab CATCHSEX \tab integer \tab cleans up to \code{sex}, the gender of the catch\cr
#' [,6] \tab SVVESSEL \tab character \tab cleans up to \code{vessel}, vessel ID\cr
#' [,7] \tab YEAR \tab integer \tab cleans up to \code{year}, year of haul\cr
#' [,8] \tab SEASON \tab character \tab insert_description_here\cr
#' [,9] \tab LAT \tab numeric \tab cleans up to \code{lat}, latitude of the haul\cr
#' [,10] \tab LON \tab numeric \tab cleans up to \code{lon}, longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,11] \tab EST_TOWDATE \tab c("POSIXct", "POSIXt") \tab cleans up to \code{datetime}, the day and time of the haul\cr
#' [,12] \tab DEPTH \tab integer \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,13] \tab SURFTEMP \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,14] \tab SURFSALIN \tab numeric \tab cleans up to \code{ssalin}, surface salinity\cr
#' [,15] \tab BOTTEMP \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,16] \tab BOTSALIN \tab numeric \tab cleans up to \code{bsalin}, bottom salinity\cr
#' [,17] \tab ABUNDANCE \tab numeric \tab cleans up to \code{cnt}, number of individuals in the whole net (may be extrapolated)\cr
#' [,18] \tab BIOMASS \tab numeric \tab cleans up to \code{weight}, the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,19] \tab LENGTH \tab numeric \tab cleans up to \code{length}, length of catch (typically of an individual, but may be average for some regions)\cr
#' [,20] \tab NUMLEN \tab integer \tab number of individuals used to ascertain catch length\cr
#' [,21] \tab SCINAME \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,22] \tab ITISSPP \tab integer \tab insert_description_here\cr
#' [,23] \tab COMNAME \tab character \tab cleans up to \code{common}, the common name of the organism sampled\cr
#' [,24] \tab AUTHOR \tab character \tab insert_description_here\cr
#' [,25] \tab Areanmi2 \tab integer \tab cleans up to \code{stratumarea}, the area of the statistical stratum (km2)\cr
#' }
"raw.neus"


#' Raw Raw Newfoundland  
#'   
#' Raw data set for the Newfoundland bottom trawl survey  
#'   
#' @format
#' A dim = 547790 x 40 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab sppcode \tab integer \tab insert_description_here\cr
#' [,2] \tab recordtype \tab integer \tab insert_description_here\cr
#' [,3] \tab vessel \tab integer \tab vessel ID\cr
#' [,4] \tab trip \tab integer \tab insert_description_here\cr
#' [,5] \tab set \tab integer \tab insert_description_here\cr
#' [,6] \tab yearl \tab integer \tab cleans up to \code{year}, year of haul\cr
#' [,7] \tab monthl \tab integer \tab insert_description_here\cr
#' [,8] \tab dayl \tab integer \tab insert_description_here\cr
#' [,9] \tab settype \tab integer \tab insert_description_here\cr
#' [,10] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,11] \tab nafo \tab character \tab insert_description_here\cr
#' [,12] \tab unitarea \tab character \tab insert_description_here\cr
#' [,13] \tab light \tab numeric \tab insert_description_here\cr
#' [,14] \tab winddir \tab numeric \tab insert_description_here\cr
#' [,15] \tab windforce \tab numeric \tab cleans up to \code{wind}, wind speed (?)\cr
#' [,16] \tab sea \tab numeric \tab insert_description_here\cr
#' [,17] \tab bottom \tab numeric \tab insert_description_here\cr
#' [,18] \tab timel \tab character \tab cleans up to \code{time}, starting time of the tow\cr
#' [,19] \tab duration \tab numeric \tab duration of the haul (how long the net was being towed)\cr
#' [,20] \tab distance \tab numeric \tab insert_description_here\cr
#' [,21] \tab operation \tab numeric \tab insert_description_here\cr
#' [,22] \tab depth \tab character \tab the maximum depth of the water at the location of the haul\cr
#' [,23] \tab depthmin \tab character \tab insert_description_here\cr
#' [,24] \tab depthmax \tab character \tab insert_description_here\cr
#' [,25] \tab depthbottom \tab character \tab insert_description_here\cr
#' [,26] \tab surftemp \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,27] \tab bottemp \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,28] \tab latstart \tab character \tab insert_description_here\cr
#' [,29] \tab lonstart \tab character \tab insert_description_here\cr
#' [,30] \tab posmethod \tab numeric \tab insert_description_here\cr
#' [,31] \tab gear \tab numeric \tab cleans up to \code{geartype}, code for the type of gear used\cr
#' [,32] \tab num \tab numeric \tab cleans up to \code{cnt}, number of individuals in the whole net (may be extrapolated)\cr
#' [,33] \tab wgt \tab numeric \tab cleans up to \code{weight}, the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,34] \tab latend \tab character \tab insert_description_here\cr
#' [,35] \tab lonend \tab character \tab insert_description_here\cr
#' [,36] \tab bottempmeth \tab numeric \tab insert_description_here\cr
#' [,37] \tab geardevice \tab numeric \tab insert_description_here\cr
#' [,38] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,39] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,40] \tab season \tab character \tab insert_description_here\cr
#' }
"raw.newf"


#' Raw South Atlantic (Southeast US)  
#'   
#' Raw data set for the South Atlantic (Southeast US) bottom trawl survey  
#'   
#' @format
#' A dim = 351427 x 62 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab COLLECTIONNUMBER \tab integer \tab insert_description_here\cr
#' [,2] \tab PROJECTNAME \tab character \tab insert_description_here\cr
#' [,3] \tab PROJECTAGENCY \tab character \tab insert_description_here\cr
#' [,4] \tab DATE \tab character \tab cleans up to \code{date}, date of the tow\cr
#' [,5] \tab EVENTNAME \tab integer \tab cleans up to \code{haulid}, a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,6] \tab VESSELNAME \tab character \tab cleans up to \code{vessel}, vessel ID\cr
#' [,7] \tab GEARNAME \tab character \tab insert_description_here\cr
#' [,8] \tab GEARCODE \tab integer \tab insert_description_here\cr
#' [,9] \tab SPECIESCODE \tab integer64 \tab insert_description_here\cr
#' [,10] \tab MRRI_CODE \tab character \tab insert_description_here\cr
#' [,11] \tab SPECIESSCIENTIFICNAME \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,12] \tab SPECIESCOMMONNAME \tab character \tab cleans up to \code{common}, the common name of the organism sampled\cr
#' [,13] \tab NUMBERTOTAL \tab integer \tab cleans up to \code{cnt}, number of individuals in the whole net (may be extrapolated)\cr
#' [,14] \tab SPECIESTOTALWEIGHT \tab numeric \tab cleans up to \code{weight}, the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,15] \tab SPECIESSUBWEIGHT \tab numeric \tab insert_description_here\cr
#' [,16] \tab SPECIESWGTPROCESSED \tab numeric \tab insert_description_here\cr
#' [,17] \tab WEIGHTMETHODDESC \tab character \tab insert_description_here\cr
#' [,18] \tab ORGWTUNITS \tab character \tab insert_description_here\cr
#' [,19] \tab EFFORT \tab numeric \tab cleans up to \code{effort}, some measure of the effort for this catch; could be area, time, speed, net size, or some combination. When possible, was converted to area swept by net during the entire tow\cr
#' [,20] \tab CATCHSUBSAMPLED \tab logical \tab insert_description_here\cr
#' [,21] \tab CATCHWEIGHT \tab numeric \tab insert_description_here\cr
#' [,22] \tab CATCHSUBWEIGHT \tab numeric \tab insert_description_here\cr
#' [,23] \tab TIMESTART \tab character \tab cleans up to \code{time}, starting time of the tow\cr
#' [,24] \tab DURATION \tab integer \tab cleans up to \code{towduration}, the duration (time) for the tow\cr
#' [,25] \tab TOWTYPETEXT \tab character \tab insert_description_here\cr
#' [,26] \tab LOCATION \tab character \tab insert_description_here\cr
#' [,27] \tab REGION \tab character \tab insert_description_here\cr
#' [,28] \tab DEPTHZONE \tab character \tab insert_description_here\cr
#' [,29] \tab ACCSPGRIDCODE \tab character \tab insert_description_here\cr
#' [,30] \tab STATIONCODE \tab character \tab cleans up to \code{stratum}, the statistical stratum of the haul\cr
#' [,31] \tab EVENTTYPEDESCRIPTION \tab character \tab insert_description_here\cr
#' [,32] \tab TEMPSURFACE \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,33] \tab TEMPBOTTOM \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,34] \tab SALINITYSURFACE \tab numeric \tab cleans up to \code{ssalin}, surface salinity\cr
#' [,35] \tab SALINITYBOTTOM \tab numeric \tab cleans up to \code{bsalin}, bottom salinity\cr
#' [,36] \tab SDO \tab logical \tab cleans up to \code{sdo}, surface dissolved oxygen\cr
#' [,37] \tab BDO \tab logical \tab cleans up to \code{bdo}, bottom dissolved oxygen\cr
#' [,38] \tab TEMPAIR \tab numeric \tab cleans up to \code{airtemp}, air temperature\cr
#' [,39] \tab LATITUDESTART \tab numeric \tab insert_description_here\cr
#' [,40] \tab LATITUDEEND \tab numeric \tab insert_description_here\cr
#' [,41] \tab LONGITUDESTART \tab numeric \tab insert_description_here\cr
#' [,42] \tab LONGITUDEEND \tab numeric \tab insert_description_here\cr
#' [,43] \tab SPECSTATUSDESCRIPTION \tab character \tab insert_description_here\cr
#' [,44] \tab LASTUPDATED \tab character \tab insert_description_here\cr
#' [,45] \tab LIGHTPHASE \tab character \tab insert_description_here\cr
#' [,46] \tab TIMEZONE \tab character \tab cleans up to \code{timezone}, time zone\cr
#' [,47] \tab DEPTHSTART \tab integer \tab insert_description_here\cr
#' [,48] \tab DEPTHEND \tab integer \tab insert_description_here\cr
#' [,49] \tab PRESSURE \tab numeric \tab insert_description_here\cr
#' [,50] \tab WINDSPEED \tab integer \tab cleans up to \code{wind}, wind speed (?)\cr
#' [,51] \tab WINDDIRECTION \tab integer \tab insert_description_here\cr
#' [,52] \tab WAVEHEIGHT \tab integer \tab insert_description_here\cr
#' [,53] \tab PRECIPITATION \tab logical \tab insert_description_here\cr
#' [,54] \tab ESTIMATEDLOC \tab logical \tab insert_description_here\cr
#' [,55] \tab SEDSIZEDESC \tab character \tab insert_description_here\cr
#' [,56] \tab BTMCOMPDESC \tab logical \tab insert_description_here\cr
#' [,57] \tab WEATHERDESC \tab logical \tab insert_description_here\cr
#' [,58] \tab WATERLVLDESC \tab logical \tab insert_description_here\cr
#' [,59] \tab ALTERATIONDESC \tab logical \tab insert_description_here\cr
#' [,60] \tab ACTIVITYDESC \tab logical \tab insert_description_here\cr
#' [,61] \tab NUMBERREP \tab logical \tab insert_description_here\cr
#' [,62] \tab COMMENTS \tab logical \tab insert_description_here\cr
#' }
"raw.sa"


#' Raw Southern Gulf of St. Lawrence  
#'   
#' Raw data set for the Southern Gulf of St. Lawrence bottom trawl survey  
#'   
#' @format
#' A dim = 180583 x 28 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab stratum \tab integer \tab the statistical stratum of the haul\cr
#' [,2] \tab vessel \tab character \tab vessel ID\cr
#' [,3] \tab cruise \tab integer \tab cruise ID\cr
#' [,4] \tab set \tab integer \tab insert_description_here\cr
#' [,5] \tab year \tab integer \tab year of haul\cr
#' [,6] \tab species \tab integer \tab the species name of the species\cr
#' [,7] \tab catch \tab numeric \tab cleans up to \code{cntcpue}, number of individuals caught per hectare in the haul\cr
#' [,8] \tab biomass \tab numeric \tab cleans up to \code{wtcpue}, weight (mass) of the catch\cr
#' [,9] \tab month \tab integer \tab insert_description_here\cr
#' [,10] \tab day \tab integer \tab insert_description_here\cr
#' [,11] \tab expt \tab integer \tab insert_description_here\cr
#' [,12] \tab time \tab integer \tab starting time of the tow\cr
#' [,13] \tab temperature \tab numeric \tab insert_description_here\cr
#' [,14] \tab depthst \tab integer \tab insert_description_here\cr
#' [,15] \tab depthend \tab integer \tab insert_description_here\cr
#' [,16] \tab dtow \tab numeric \tab insert_description_here\cr
#' [,17] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,18] \tab latitude \tab numeric \tab cleans up to \code{lat}, latitude of the haul\cr
#' [,19] \tab longitude \tab numeric \tab cleans up to \code{lon}, longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,20] \tab latin_name \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,21] \tab name \tab character \tab cleans up to \code{common}, the common name of the organism sampled\cr
#' [,22] \tab N \tab integer \tab cleans up to \code{cnt}, number of individuals in the whole net (may be extrapolated)\cr
#' [,23] \tab kg \tab numeric \tab cleans up to \code{weight}, the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,24] \tab t_surface \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,25] \tab salin_bottom \tab numeric \tab cleans up to \code{bsalin}, bottom salinity\cr
#' [,26] \tab t_bottom \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,27] \tab trawlableunits \tab integer \tab insert_description_here\cr
#' [,28] \tab stratarea \tab numeric \tab cleans up to \code{stratumarea}, the area of the statistical stratum (km2)\cr
#' }
"raw.sgulf"


#' Raw Scotian Shelf  
#'   
#' Raw data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 184631 x 42 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab CODE \tab integer \tab insert_description_here\cr
#' [,2] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,3] \tab MISSION \tab character \tab insert_description_here\cr
#' [,4] \tab SETNO \tab integer \tab insert_description_here\cr
#' [,5] \tab MARKET \tab logical \tab insert_description_here\cr
#' [,6] \tab SAMPWGT \tab numeric \tab insert_description_here\cr
#' [,7] \tab ADJ_TOTWGT \tab numeric \tab cleans up to \code{wtcpue}, weight (mass) of the catch\cr
#' [,8] \tab ADJ_TOTNO \tab numeric \tab cleans up to \code{cntcpue}, number of individuals caught per hectare in the haul\cr
#' [,9] \tab CALWT \tab integer \tab insert_description_here\cr
#' [,10] \tab REMARKS \tab character \tab insert_description_here\cr
#' [,11] \tab SIZE_CLASS \tab integer \tab insert_description_here\cr
#' [,12] \tab SDATE \tab character \tab cleans up to \code{date}, date of the tow\cr
#' [,13] \tab TIME \tab integer \tab cleans up to \code{time}, starting time of the tow\cr
#' [,14] \tab SLAT \tab numeric \tab insert_description_here\cr
#' [,15] \tab SLONG \tab numeric \tab insert_description_here\cr
#' [,16] \tab ELAT \tab numeric \tab insert_description_here\cr
#' [,17] \tab ELONG \tab numeric \tab insert_description_here\cr
#' [,18] \tab AREA \tab integer \tab insert_description_here\cr
#' [,19] \tab DUR \tab integer \tab cleans up to \code{towduration}, the duration (time) for the tow\cr
#' [,20] \tab DIST \tab numeric \tab insert_description_here\cr
#' [,21] \tab HOWD \tab integer \tab insert_description_here\cr
#' [,22] \tab SPEED \tab numeric \tab cleans up to \code{towspeed}, the speed of the vessel\cr
#' [,23] \tab HOWS \tab integer \tab insert_description_here\cr
#' [,24] \tab DMIN \tab integer \tab insert_description_here\cr
#' [,25] \tab DMAX \tab integer \tab insert_description_here\cr
#' [,26] \tab WIND \tab integer \tab cleans up to \code{wind}, wind speed (?)\cr
#' [,27] \tab FORCE \tab integer \tab insert_description_here\cr
#' [,28] \tab CURNT \tab integer \tab insert_description_here\cr
#' [,29] \tab TYPE \tab integer \tab insert_description_here\cr
#' [,30] \tab GEAR \tab integer \tab insert_description_here\cr
#' [,31] \tab AUX \tab integer \tab insert_description_here\cr
#' [,32] \tab DEPTH \tab integer \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,33] \tab ETIME \tab character \tab insert_description_here\cr
#' [,34] \tab START_DEPTH \tab integer \tab insert_description_here\cr
#' [,35] \tab END_DEPTH \tab integer \tab insert_description_here\cr
#' [,36] \tab SURFACE_TEMPERATURE \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,37] \tab BOTTOM_TEMPERATURE \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,38] \tab BOTTOM_SALINITY \tab numeric \tab cleans up to \code{bsalin}, bottom salinity\cr
#' [,39] \tab depthzone_fathoms \tab character \tab insert_description_here\cr
#' [,40] \tab stratarea_nmi2 \tab integer \tab cleans up to \code{stratumarea}, the area of the statistical stratum (km2)\cr
#' [,41] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,42] \tab common \tab character \tab the common name of the organism sampled\cr
#' }
"raw.shelf"


#' Raw West Coast Annual  
#'   
#' Raw data set for the West Coast Annual bottom trawl survey  
#'   
#' @format
#' A dim = 217514 x 19 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab Trawl Id \tab integer64 \tab cleans up to \code{haulid}, a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,2] \tab Species \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,3] \tab Haul Weight (kg) \tab numeric \tab cleans up to \code{weight}, the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,4] \tab Individual Average Weight (kg) \tab numeric \tab insert_description_here\cr
#' [,5] \tab Survey \tab character \tab insert_description_here\cr
#' [,6] \tab Survey Cycle \tab character \tab cleans up to \code{year}, year of haul\cr
#' [,7] \tab Vessel \tab character \tab cleans up to \code{vessel}, vessel ID\cr
#' [,8] \tab Cruise Leg \tab integer \tab insert_description_here\cr
#' [,9] \tab Trawl Performance \tab character \tab insert_description_here\cr
#' [,10] \tab Trawl Date \tab character \tab cleans up to \code{date}, date of the tow\cr
#' [,11] \tab Trawl Start Time \tab character \tab cleans up to \code{datetime}, the day and time of the haul\cr
#' [,12] \tab Best Latitude (dd) \tab numeric \tab cleans up to \code{lat}, latitude of the haul\cr
#' [,13] \tab Best Longitude (dd) \tab numeric \tab cleans up to \code{lon}, longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,14] \tab Best Position Type \tab character \tab insert_description_here\cr
#' [,15] \tab Best Depth (m) \tab numeric \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,16] \tab Best Depth Type \tab character \tab insert_description_here\cr
#' [,17] \tab Trawl Duration (min) \tab numeric \tab cleans up to \code{towduration}, the duration (time) for the tow\cr
#' [,18] \tab Area Swept by the Net (hectares) \tab numeric \tab insert_description_here\cr
#' [,19] \tab Temperature At the Gear (degs C) \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' }
"raw.wcann"


#' Raw West Coast Triennial  
#'   
#' Raw data set for the West Coast Triennial bottom trawl survey  
#'   
#' @format
#' A dim = 108138 x 42 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab SPECIES_CODE \tab integer \tab cleans up to \code{SID}, species identification number\cr
#' [,2] \tab CRUISEJOIN \tab integer \tab insert_description_here\cr
#' [,3] \tab HAULJOIN \tab integer \tab insert_description_here\cr
#' [,4] \tab REGION \tab character \tab insert_description_here\cr
#' [,5] \tab VESSEL \tab integer \tab cleans up to \code{vessel}, vessel ID\cr
#' [,6] \tab CRUISE \tab integer \tab cleans up to \code{cruise}, cruise ID\cr
#' [,7] \tab HAUL \tab integer \tab cleans up to \code{haul}, the integer haul number within a cruise\cr
#' [,8] \tab CATCHJOIN \tab integer \tab insert_description_here\cr
#' [,9] \tab WEIGHT \tab numeric \tab cleans up to \code{weight}, the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,10] \tab NUMBER_FISH \tab integer \tab cleans up to \code{cnt}, number of individuals in the whole net (may be extrapolated)\cr
#' [,11] \tab SUBSAMPLE_CODE \tab logical \tab insert_description_here\cr
#' [,12] \tab VOUCHER \tab integer \tab insert_description_here\cr
#' [,13] \tab AUDITJOIN \tab integer \tab insert_description_here\cr
#' [,14] \tab HAUL_TYPE \tab integer \tab insert_description_here\cr
#' [,15] \tab PERFORMANCE \tab numeric \tab insert_description_here\cr
#' [,16] \tab START_TIME \tab character \tab cleans up to \code{datetime}, the day and time of the haul\cr
#' [,17] \tab DURATION \tab numeric \tab cleans up to \code{towduration}, the duration (time) for the tow\cr
#' [,18] \tab DISTANCE_FISHED \tab numeric \tab insert_description_here\cr
#' [,19] \tab NET_WIDTH \tab numeric \tab cleans up to \code{gearsize}, the dimension of the gear; for trawl net, the width of the mouth in ft\cr
#' [,20] \tab NET_MEASURED \tab character \tab insert_description_here\cr
#' [,21] \tab NET_HEIGHT \tab numeric \tab insert_description_here\cr
#' [,22] \tab STRATUM \tab integer \tab cleans up to \code{stratum}, the statistical stratum of the haul\cr
#' [,23] \tab START_LATITUDE \tab numeric \tab insert_description_here\cr
#' [,24] \tab END_LATITUDE \tab numeric \tab insert_description_here\cr
#' [,25] \tab START_LONGITUDE \tab numeric \tab insert_description_here\cr
#' [,26] \tab END_LONGITUDE \tab numeric \tab insert_description_here\cr
#' [,27] \tab STATIONID \tab character \tab cleans up to \code{station}, the station ID for the haul\cr
#' [,28] \tab GEAR_DEPTH \tab integer \tab insert_description_here\cr
#' [,29] \tab BOTTOM_DEPTH \tab integer \tab cleans up to \code{depth}, the maximum depth of the water at the location of the haul\cr
#' [,30] \tab BOTTOM_TYPE \tab character \tab insert_description_here\cr
#' [,31] \tab SURFACE_TEMPERATURE \tab numeric \tab cleans up to \code{stemp}, water temperature at the surface at the location of the haul\cr
#' [,32] \tab GEAR_TEMPERATURE \tab numeric \tab cleans up to \code{btemp}, water temperature at the bottom at the location of the haul\cr
#' [,33] \tab WIRE_LENGTH \tab integer \tab insert_description_here\cr
#' [,34] \tab GEAR \tab character \tab cleans up to \code{geartype}, code for the type of gear used\cr
#' [,35] \tab ACCESSORIES \tab character \tab insert_description_here\cr
#' [,36] \tab SUBSAMPLE \tab character \tab insert_description_here\cr
#' [,37] \tab SPECIES_NAME \tab character \tab cleans up to \code{spp}, species scientific name; Genus species\cr
#' [,38] \tab COMMON_NAME \tab character \tab cleans up to \code{common}, the common name of the organism sampled\cr
#' [,39] \tab REVISION \tab character \tab insert_description_here\cr
#' [,40] \tab BS \tab character \tab insert_description_here\cr
#' [,41] \tab GOA \tab character \tab insert_description_here\cr
#' [,42] \tab WC \tab character \tab insert_description_here\cr
#' }
"raw.wctri"


#' Clean Aleutian Islands  
#'   
#' Clean data from the Aleutian Islands bottom trawl  
#'   
#' @format
#' A dim = 118661 x 47 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,3] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,4] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,5] \tab station \tab character \tab the station ID for the haul\cr
#' [,6] \tab year \tab character \tab year of haul\cr
#' [,7] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,8] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,9] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,10] \tab SID \tab integer \tab species identification number\cr
#' [,11] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,12] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,13] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,14] \tab vessel \tab character \tab vessel ID\cr
#' [,15] \tab cruise \tab character \tab cruise ID\cr
#' [,16] \tab haul \tab character \tab the integer haul number within a cruise\cr
#' [,17] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,18] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,19] \tab season \tab character \tab insert_description_here\cr
#' [,20] \tab reg \tab character \tab survey region\cr
#' [,21] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,22] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,23] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,24] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,25] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,26] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,27] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,28] \tab species \tab character \tab the species name of the species\cr
#' [,29] \tab genus \tab character \tab the genus of the species\cr
#' [,30] \tab family \tab character \tab taxonomic family\cr
#' [,31] \tab order \tab character \tab taxonomic order\cr
#' [,32] \tab class \tab character \tab taxonomic class\cr
#' [,33] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,34] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,35] \tab phylum \tab character \tab insert_description_here\cr
#' [,36] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,37] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,38] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,39] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,40] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,41] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,42] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,43] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,44] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,45] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,46] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,47] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.ai"


#' Clean Eastern Berring Sea  
#'   
#' Clean data set for the Eastern Berring Sea bottom trawl survey  
#'   
#' @format
#' A dim = 380027 x 47 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,3] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,4] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,5] \tab station \tab character \tab the station ID for the haul\cr
#' [,6] \tab year \tab character \tab year of haul\cr
#' [,7] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,8] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,9] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,10] \tab SID \tab integer \tab species identification number\cr
#' [,11] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,12] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,13] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,14] \tab vessel \tab character \tab vessel ID\cr
#' [,15] \tab cruise \tab character \tab cruise ID\cr
#' [,16] \tab haul \tab character \tab the integer haul number within a cruise\cr
#' [,17] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,18] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,19] \tab season \tab character \tab insert_description_here\cr
#' [,20] \tab reg \tab character \tab survey region\cr
#' [,21] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,22] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,23] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,24] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,25] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,26] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,27] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,28] \tab species \tab character \tab the species name of the species\cr
#' [,29] \tab genus \tab character \tab the genus of the species\cr
#' [,30] \tab family \tab character \tab taxonomic family\cr
#' [,31] \tab order \tab character \tab taxonomic order\cr
#' [,32] \tab class \tab character \tab taxonomic class\cr
#' [,33] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,34] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,35] \tab phylum \tab character \tab insert_description_here\cr
#' [,36] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,37] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,38] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,39] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,40] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,41] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,42] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,43] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,44] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,45] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,46] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,47] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.ebs"


#' Clean Gulf of Mexico  
#'   
#' Clean data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 716550 x 74 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab cruise \tab character \tab cruise ID\cr
#' [,3] \tab vessel \tab character \tab vessel ID\cr
#' [,4] \tab SID \tab character \tab species identification number\cr
#' [,5] \tab station \tab character \tab the station ID for the haul\cr
#' [,6] \tab CRUISE_NO \tab numeric \tab cruise ID as number\cr
#' [,7] \tab P_STA_NO \tab numeric \tab insert_description_here\cr
#' [,8] \tab BGSID \tab integer \tab record ID number\cr
#' [,9] \tab unknown \tab integer \tab insert_description_here\cr
#' [,10] \tab BGSCODE \tab character \tab flags information about the biological sample\cr
#' [,11] \tab cnt \tab numeric \tab number of individuals in the whole net (may be extrapolated)\cr
#' [,12] \tab weight \tab numeric \tab the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,13] \tab towID \tab character \tab insert_description_here\cr
#' [,14] \tab gearsize \tab integer \tab the dimension of the gear; for trawl net, the width of the mouth in ft\cr
#' [,15] \tab geartype \tab character \tab code for the type of gear used\cr
#' [,16] \tab meshsize \tab numeric \tab the size of the net mesh (inches of stretch)\cr
#' [,17] \tab OP \tab character \tab insert_description_here\cr
#' [,18] \tab towduration \tab numeric \tab the duration (time) for the tow\cr
#' [,19] \tab timezone \tab character \tab time zone\cr
#' [,20] \tab time \tab character \tab starting time of the tow\cr
#' [,21] \tab lat.deg.start \tab numeric \tab insert_description_here\cr
#' [,22] \tab lat.min.start \tab numeric \tab insert_description_here\cr
#' [,23] \tab lon.deg.start \tab numeric \tab insert_description_here\cr
#' [,24] \tab lon.min.start \tab numeric \tab insert_description_here\cr
#' [,25] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,26] \tab date \tab character \tab date of the tow\cr
#' [,27] \tab lat.deg.end \tab numeric \tab insert_description_here\cr
#' [,28] \tab lat.min.end \tab numeric \tab insert_description_here\cr
#' [,29] \tab lon.deg.end \tab numeric \tab insert_description_here\cr
#' [,30] \tab lon.min.end \tab numeric \tab insert_description_here\cr
#' [,31] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,32] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,33] \tab towspeed \tab numeric \tab the speed of the vessel\cr
#' [,34] \tab station.comment \tab character \tab insert_description_here\cr
#' [,35] \tab survey.name \tab character \tab insert_description_here\cr
#' [,36] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,37] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,38] \tab year \tab character \tab year of haul\cr
#' [,39] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,40] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,41] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,42] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,43] \tab towarea \tab numeric \tab insert_description_here\cr
#' [,44] \tab effort \tab numeric \tab some measure of the effort for this catch; could be area, time, speed, net size, or some combination. When possible, was converted to area swept by net during the entire tow\cr
#' [,45] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,46] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,47] \tab reg \tab character \tab survey region\cr
#' [,48] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,49] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,50] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,51] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,52] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,53] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,54] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,55] \tab species \tab character \tab the species name of the species\cr
#' [,56] \tab genus \tab character \tab the genus of the species\cr
#' [,57] \tab family \tab character \tab taxonomic family\cr
#' [,58] \tab order \tab character \tab taxonomic order\cr
#' [,59] \tab class \tab character \tab taxonomic class\cr
#' [,60] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,61] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,62] \tab phylum \tab character \tab insert_description_here\cr
#' [,63] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,64] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,65] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,66] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,67] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,68] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,69] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,70] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,71] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,72] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,73] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,74] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.gmex"


#' Clean Gulf of Alaska  
#'   
#' Clean data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 197026 x 47 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,3] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,4] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,5] \tab station \tab character \tab the station ID for the haul\cr
#' [,6] \tab year \tab character \tab year of haul\cr
#' [,7] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,8] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,9] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,10] \tab SID \tab character \tab species identification number\cr
#' [,11] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,12] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,13] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,14] \tab vessel \tab character \tab vessel ID\cr
#' [,15] \tab cruise \tab character \tab cruise ID\cr
#' [,16] \tab haul \tab character \tab the integer haul number within a cruise\cr
#' [,17] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,18] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,19] \tab season \tab character \tab insert_description_here\cr
#' [,20] \tab reg \tab character \tab survey region\cr
#' [,21] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,22] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,23] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,24] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,25] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,26] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,27] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,28] \tab species \tab character \tab the species name of the species\cr
#' [,29] \tab genus \tab character \tab the genus of the species\cr
#' [,30] \tab family \tab character \tab taxonomic family\cr
#' [,31] \tab order \tab character \tab taxonomic order\cr
#' [,32] \tab class \tab character \tab taxonomic class\cr
#' [,33] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,34] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,35] \tab phylum \tab character \tab insert_description_here\cr
#' [,36] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,37] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,38] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,39] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,40] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,41] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,42] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,43] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,44] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,45] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,46] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,47] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.goa"


#' Clean Northeast US  
#'   
#' Clean data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 2523907 x 55 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab cruise \tab character \tab cruise ID\cr
#' [,3] \tab station \tab character \tab the station ID for the haul\cr
#' [,4] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,5] \tab SID \tab integer \tab species identification number\cr
#' [,6] \tab sex \tab integer \tab the gender of the catch\cr
#' [,7] \tab vessel \tab character \tab vessel ID\cr
#' [,8] \tab year \tab character \tab year of haul\cr
#' [,9] \tab season \tab character \tab insert_description_here\cr
#' [,10] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,11] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,12] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,13] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,14] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,15] \tab ssalin \tab numeric \tab surface salinity\cr
#' [,16] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,17] \tab bsalin \tab numeric \tab bottom salinity\cr
#' [,18] \tab cnt \tab numeric \tab number of individuals in the whole net (may be extrapolated)\cr
#' [,19] \tab weight \tab numeric \tab the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,20] \tab length \tab numeric \tab length of catch (typically of an individual, but may be average for some regions)\cr
#' [,21] \tab NUMLEN \tab integer \tab number of individuals used to ascertain catch length\cr
#' [,22] \tab ITISSPP \tab integer \tab insert_description_here\cr
#' [,23] \tab AUTHOR \tab character \tab insert_description_here\cr
#' [,24] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,25] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,26] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,27] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,28] \tab reg \tab character \tab survey region\cr
#' [,29] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,30] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,31] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,32] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,33] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,34] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,35] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,36] \tab species \tab character \tab the species name of the species\cr
#' [,37] \tab genus \tab character \tab the genus of the species\cr
#' [,38] \tab family \tab character \tab taxonomic family\cr
#' [,39] \tab order \tab character \tab taxonomic order\cr
#' [,40] \tab class \tab character \tab taxonomic class\cr
#' [,41] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,42] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,43] \tab phylum \tab character \tab insert_description_here\cr
#' [,44] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,45] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,46] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,47] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,48] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,49] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,50] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,51] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,52] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,53] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,54] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,55] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.neus"


#' Clean Clean Newfoundland  
#'   
#' Clean data set for the Newfoundland bottom trawl survey  
#'   
#' @format
#' A dim = 547790 x 75 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab sppcode \tab integer \tab insert_description_here\cr
#' [,3] \tab recordtype \tab integer \tab insert_description_here\cr
#' [,4] \tab vessel \tab character \tab vessel ID\cr
#' [,5] \tab trip \tab integer \tab insert_description_here\cr
#' [,6] \tab set \tab character \tab insert_description_here\cr
#' [,7] \tab year \tab numeric \tab year of haul\cr
#' [,8] \tab month \tab character \tab insert_description_here\cr
#' [,9] \tab day \tab character \tab insert_description_here\cr
#' [,10] \tab settype \tab integer \tab insert_description_here\cr
#' [,11] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,12] \tab nafo \tab character \tab insert_description_here\cr
#' [,13] \tab unitarea \tab character \tab insert_description_here\cr
#' [,14] \tab light \tab numeric \tab insert_description_here\cr
#' [,15] \tab winddir \tab numeric \tab insert_description_here\cr
#' [,16] \tab wind \tab numeric \tab wind speed (?)\cr
#' [,17] \tab sea \tab numeric \tab insert_description_here\cr
#' [,18] \tab bottom \tab numeric \tab insert_description_here\cr
#' [,19] \tab time \tab character \tab starting time of the tow\cr
#' [,20] \tab towduration \tab numeric \tab the duration (time) for the tow\cr
#' [,21] \tab towdistance \tab numeric \tab insert_description_here\cr
#' [,22] \tab operation \tab numeric \tab insert_description_here\cr
#' [,23] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,24] \tab depth.min \tab numeric \tab insert_description_here\cr
#' [,25] \tab depth.max \tab numeric \tab insert_description_here\cr
#' [,26] \tab depthbottom \tab character \tab insert_description_here\cr
#' [,27] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,28] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,29] \tab lat.start \tab numeric \tab insert_description_here\cr
#' [,30] \tab lon.start \tab numeric \tab insert_description_here\cr
#' [,31] \tab posmethod \tab numeric \tab insert_description_here\cr
#' [,32] \tab geartype \tab character \tab code for the type of gear used\cr
#' [,33] \tab cnt \tab numeric \tab number of individuals in the whole net (may be extrapolated)\cr
#' [,34] \tab weight \tab numeric \tab the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,35] \tab lat.end \tab numeric \tab insert_description_here\cr
#' [,36] \tab lon.end \tab numeric \tab insert_description_here\cr
#' [,37] \tab bottempmeth \tab numeric \tab insert_description_here\cr
#' [,38] \tab geardevice \tab numeric \tab insert_description_here\cr
#' [,39] \tab season \tab character \tab insert_description_here\cr
#' [,40] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,41] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,42] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,43] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,44] \tab towarea \tab numeric \tab insert_description_here\cr
#' [,45] \tab effort \tab numeric \tab some measure of the effort for this catch; could be area, time, speed, net size, or some combination. When possible, was converted to area swept by net during the entire tow\cr
#' [,46] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,47] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,48] \tab reg \tab character \tab survey region\cr
#' [,49] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,50] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,51] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,52] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,53] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,54] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,55] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,56] \tab species \tab character \tab the species name of the species\cr
#' [,57] \tab genus \tab character \tab the genus of the species\cr
#' [,58] \tab family \tab character \tab taxonomic family\cr
#' [,59] \tab order \tab character \tab taxonomic order\cr
#' [,60] \tab class \tab character \tab taxonomic class\cr
#' [,61] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,62] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,63] \tab phylum \tab character \tab insert_description_here\cr
#' [,64] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,65] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,66] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,67] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,68] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,69] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,70] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,71] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,72] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,73] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,74] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,75] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.newf"


#' Clean South Atlantic (Southeast US)  
#'   
#' Clean data set for the South Atlantic (Southeast US) bottom trawl survey  
#'   
#' @format
#' A dim = 351427 x 97 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab COLLECTIONNUMBER \tab integer \tab insert_description_here\cr
#' [,3] \tab PROJECTNAME \tab character \tab insert_description_here\cr
#' [,4] \tab PROJECTAGENCY \tab character \tab insert_description_here\cr
#' [,5] \tab date \tab character \tab date of the tow\cr
#' [,6] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,7] \tab vessel \tab character \tab vessel ID\cr
#' [,8] \tab GEARNAME \tab character \tab insert_description_here\cr
#' [,9] \tab GEARCODE \tab character \tab insert_description_here\cr
#' [,10] \tab SPECIESCODE \tab integer64 \tab insert_description_here\cr
#' [,11] \tab MRRI_CODE \tab character \tab insert_description_here\cr
#' [,12] \tab cnt \tab numeric \tab number of individuals in the whole net (may be extrapolated)\cr
#' [,13] \tab weight \tab numeric \tab the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,14] \tab SPECIESSUBWEIGHT \tab numeric \tab insert_description_here\cr
#' [,15] \tab SPECIESWGTPROCESSED \tab numeric \tab insert_description_here\cr
#' [,16] \tab WEIGHTMETHODDESC \tab character \tab insert_description_here\cr
#' [,17] \tab ORGWTUNITS \tab character \tab insert_description_here\cr
#' [,18] \tab effort \tab numeric \tab some measure of the effort for this catch; could be area, time, speed, net size, or some combination. When possible, was converted to area swept by net during the entire tow\cr
#' [,19] \tab CATCHSUBSAMPLED \tab logical \tab insert_description_here\cr
#' [,20] \tab CATCHWEIGHT \tab numeric \tab insert_description_here\cr
#' [,21] \tab CATCHSUBWEIGHT \tab numeric \tab insert_description_here\cr
#' [,22] \tab time \tab character \tab starting time of the tow\cr
#' [,23] \tab towduration \tab numeric \tab the duration (time) for the tow\cr
#' [,24] \tab TOWTYPETEXT \tab character \tab insert_description_here\cr
#' [,25] \tab LOCATION \tab character \tab insert_description_here\cr
#' [,26] \tab REGION \tab character \tab insert_description_here\cr
#' [,27] \tab DEPTHZONE \tab character \tab insert_description_here\cr
#' [,28] \tab ACCSPGRIDCODE \tab character \tab insert_description_here\cr
#' [,29] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,30] \tab EVENTTYPEDESCRIPTION \tab character \tab insert_description_here\cr
#' [,31] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,32] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,33] \tab ssalin \tab numeric \tab surface salinity\cr
#' [,34] \tab bsalin \tab numeric \tab bottom salinity\cr
#' [,35] \tab sdo \tab logical \tab surface dissolved oxygen\cr
#' [,36] \tab bdo \tab numeric \tab bottom dissolved oxygen\cr
#' [,37] \tab airtemp \tab numeric \tab air temperature\cr
#' [,38] \tab lat.start \tab numeric \tab insert_description_here\cr
#' [,39] \tab lat.end \tab numeric \tab insert_description_here\cr
#' [,40] \tab lon.start \tab numeric \tab insert_description_here\cr
#' [,41] \tab lon.end \tab numeric \tab insert_description_here\cr
#' [,42] \tab SPECSTATUSDESCRIPTION \tab character \tab insert_description_here\cr
#' [,43] \tab LASTUPDATED \tab character \tab insert_description_here\cr
#' [,44] \tab LIGHTPHASE \tab character \tab insert_description_here\cr
#' [,45] \tab timezone \tab character \tab time zone\cr
#' [,46] \tab depth.start \tab numeric \tab insert_description_here\cr
#' [,47] \tab depth.end \tab numeric \tab insert_description_here\cr
#' [,48] \tab PRESSURE \tab numeric \tab insert_description_here\cr
#' [,49] \tab wind \tab integer \tab wind speed (?)\cr
#' [,50] \tab WINDDIRECTION \tab integer \tab insert_description_here\cr
#' [,51] \tab WAVEHEIGHT \tab integer \tab insert_description_here\cr
#' [,52] \tab PRECIPITATION \tab logical \tab insert_description_here\cr
#' [,53] \tab ESTIMATEDLOC \tab logical \tab insert_description_here\cr
#' [,54] \tab SEDSIZEDESC \tab character \tab insert_description_here\cr
#' [,55] \tab BTMCOMPDESC \tab logical \tab insert_description_here\cr
#' [,56] \tab WEATHERDESC \tab logical \tab insert_description_here\cr
#' [,57] \tab WATERLVLDESC \tab logical \tab insert_description_here\cr
#' [,58] \tab ALTERATIONDESC \tab logical \tab insert_description_here\cr
#' [,59] \tab ACTIVITYDESC \tab logical \tab insert_description_here\cr
#' [,60] \tab NUMBERREP \tab logical \tab insert_description_here\cr
#' [,61] \tab comment \tab character \tab insert_description_here\cr
#' [,62] \tab year \tab integer \tab year of haul\cr
#' [,63] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,64] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,65] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,66] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,67] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,68] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,69] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,70] \tab reg \tab character \tab survey region\cr
#' [,71] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,72] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,73] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,74] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,75] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,76] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,77] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,78] \tab species \tab character \tab the species name of the species\cr
#' [,79] \tab genus \tab character \tab the genus of the species\cr
#' [,80] \tab family \tab character \tab taxonomic family\cr
#' [,81] \tab order \tab character \tab taxonomic order\cr
#' [,82] \tab class \tab character \tab taxonomic class\cr
#' [,83] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,84] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,85] \tab phylum \tab character \tab insert_description_here\cr
#' [,86] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,87] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,88] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,89] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,90] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,91] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,92] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,93] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,94] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,95] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,96] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,97] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.sa"


#' Clean Southern Gulf of St. Lawrence  
#'   
#' Clean data set for the Southern Gulf of St. Lawrence bottom trawl survey  
#'   
#' @format
#' A dim = 180583 x 58 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,3] \tab vessel \tab character \tab vessel ID\cr
#' [,4] \tab cruise \tab character \tab cruise ID\cr
#' [,5] \tab set \tab character \tab insert_description_here\cr
#' [,6] \tab year \tab character \tab year of haul\cr
#' [,7] \tab species.code \tab integer \tab insert_description_here\cr
#' [,8] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,9] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,10] \tab month \tab character \tab insert_description_here\cr
#' [,11] \tab day \tab character \tab insert_description_here\cr
#' [,12] \tab expt \tab integer \tab insert_description_here\cr
#' [,13] \tab time \tab character \tab starting time of the tow\cr
#' [,14] \tab temperature \tab numeric \tab insert_description_here\cr
#' [,15] \tab depth.start \tab numeric \tab insert_description_here\cr
#' [,16] \tab depth.end \tab numeric \tab insert_description_here\cr
#' [,17] \tab dtow \tab numeric \tab insert_description_here\cr
#' [,18] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,19] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,20] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,21] \tab cnt \tab numeric \tab number of individuals in the whole net (may be extrapolated)\cr
#' [,22] \tab weight \tab numeric \tab the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,23] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,24] \tab bsalin \tab numeric \tab bottom salinity\cr
#' [,25] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,26] \tab trawlableunits \tab integer \tab insert_description_here\cr
#' [,27] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,28] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,29] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,30] \tab season \tab character \tab insert_description_here\cr
#' [,31] \tab reg \tab character \tab survey region\cr
#' [,32] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,33] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,34] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,35] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,36] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,37] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,38] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,39] \tab species \tab character \tab the species name of the species\cr
#' [,40] \tab genus \tab character \tab the genus of the species\cr
#' [,41] \tab family \tab character \tab taxonomic family\cr
#' [,42] \tab order \tab character \tab taxonomic order\cr
#' [,43] \tab class \tab character \tab taxonomic class\cr
#' [,44] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,45] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,46] \tab phylum \tab character \tab insert_description_here\cr
#' [,47] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,48] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,49] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,50] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,51] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,52] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,53] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,54] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,55] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,56] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,57] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,58] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.sgulf"


#' Clean Scotian Shelf  
#'   
#' Clean data set for the Gulf of Mexico bottom trawl survey  
#'   
#' @format
#' A dim = 184631 x 75 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab CODE \tab integer \tab insert_description_here\cr
#' [,3] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,4] \tab MISSION \tab character \tab insert_description_here\cr
#' [,5] \tab SETNO \tab integer \tab insert_description_here\cr
#' [,6] \tab MARKET \tab logical \tab insert_description_here\cr
#' [,7] \tab SAMPWGT \tab numeric \tab insert_description_here\cr
#' [,8] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,9] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,10] \tab CALWT \tab integer \tab insert_description_here\cr
#' [,11] \tab comment \tab character \tab insert_description_here\cr
#' [,12] \tab SIZE_CLASS \tab integer \tab insert_description_here\cr
#' [,13] \tab date \tab c("POSIXct", "POSIXt") \tab date of the tow\cr
#' [,14] \tab time \tab character \tab starting time of the tow\cr
#' [,15] \tab lat.start \tab numeric \tab insert_description_here\cr
#' [,16] \tab lon.start \tab numeric \tab insert_description_here\cr
#' [,17] \tab lat.end \tab numeric \tab insert_description_here\cr
#' [,18] \tab lon.end \tab numeric \tab insert_description_here\cr
#' [,19] \tab towarea \tab numeric \tab insert_description_here\cr
#' [,20] \tab towduration \tab numeric \tab the duration (time) for the tow\cr
#' [,21] \tab towdistance \tab numeric \tab insert_description_here\cr
#' [,22] \tab HOWD \tab integer \tab insert_description_here\cr
#' [,23] \tab towspeed \tab numeric \tab the speed of the vessel\cr
#' [,24] \tab HOWS \tab integer \tab insert_description_here\cr
#' [,25] \tab depth.min \tab numeric \tab insert_description_here\cr
#' [,26] \tab depth.max \tab numeric \tab insert_description_here\cr
#' [,27] \tab wind \tab integer \tab wind speed (?)\cr
#' [,28] \tab FORCE \tab integer \tab insert_description_here\cr
#' [,29] \tab CURNT \tab integer \tab insert_description_here\cr
#' [,30] \tab TYPE \tab integer \tab insert_description_here\cr
#' [,31] \tab GEAR \tab integer \tab insert_description_here\cr
#' [,32] \tab AUX \tab integer \tab insert_description_here\cr
#' [,33] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,34] \tab date.end \tab c("POSIXct", "POSIXt") \tab insert_description_here\cr
#' [,35] \tab depth.start \tab numeric \tab insert_description_here\cr
#' [,36] \tab depth.end \tab numeric \tab insert_description_here\cr
#' [,37] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,38] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,39] \tab bsalin \tab numeric \tab bottom salinity\cr
#' [,40] \tab depthzone_fathoms \tab character \tab insert_description_here\cr
#' [,41] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,42] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,43] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,44] \tab year \tab numeric \tab year of haul\cr
#' [,45] \tab season \tab character \tab insert_description_here\cr
#' [,46] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,47] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,48] \tab reg \tab character \tab survey region\cr
#' [,49] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,50] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,51] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,52] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,53] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,54] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,55] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,56] \tab species \tab character \tab the species name of the species\cr
#' [,57] \tab genus \tab character \tab the genus of the species\cr
#' [,58] \tab family \tab character \tab taxonomic family\cr
#' [,59] \tab order \tab character \tab taxonomic order\cr
#' [,60] \tab class \tab character \tab taxonomic class\cr
#' [,61] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,62] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,63] \tab phylum \tab character \tab insert_description_here\cr
#' [,64] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,65] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,66] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,67] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,68] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,69] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,70] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,71] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,72] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,73] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,74] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,75] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.shelf"


#' Clean West Coast Annual  
#'   
#' Clean data set for the West Coast Annual bottom trawl survey  
#'   
#' @format
#' A dim = 217514 x 53 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,3] \tab weight \tab numeric \tab the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,4] \tab Individual.Average.Weight..kg. \tab numeric \tab insert_description_here\cr
#' [,5] \tab Survey \tab character \tab insert_description_here\cr
#' [,6] \tab year \tab character \tab year of haul\cr
#' [,7] \tab vessel \tab character \tab vessel ID\cr
#' [,8] \tab Cruise.Leg \tab integer \tab insert_description_here\cr
#' [,9] \tab Trawl.Performance \tab character \tab insert_description_here\cr
#' [,10] \tab date \tab character \tab date of the tow\cr
#' [,11] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,12] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,13] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,14] \tab Best.Position.Type \tab character \tab insert_description_here\cr
#' [,15] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,16] \tab Best.Depth.Type \tab character \tab insert_description_here\cr
#' [,17] \tab towduration \tab numeric \tab the duration (time) for the tow\cr
#' [,18] \tab towarea \tab numeric \tab insert_description_here\cr
#' [,19] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,20] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,21] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,22] \tab effort \tab numeric \tab some measure of the effort for this catch; could be area, time, speed, net size, or some combination. When possible, was converted to area swept by net during the entire tow\cr
#' [,23] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,24] \tab cnt \tab numeric \tab number of individuals in the whole net (may be extrapolated)\cr
#' [,25] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,26] \tab reg \tab character \tab survey region\cr
#' [,27] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,28] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,29] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,30] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,31] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,32] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,33] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,34] \tab species \tab character \tab the species name of the species\cr
#' [,35] \tab genus \tab character \tab the genus of the species\cr
#' [,36] \tab family \tab character \tab taxonomic family\cr
#' [,37] \tab order \tab character \tab taxonomic order\cr
#' [,38] \tab class \tab character \tab taxonomic class\cr
#' [,39] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,40] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,41] \tab phylum \tab character \tab insert_description_here\cr
#' [,42] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,43] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,44] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,45] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,46] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,47] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,48] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,49] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,50] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,51] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,52] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,53] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.wcann"


#' Clean West Coast Triennial  
#'   
#' Clean data set for the West Coast Triennial bottom trawl survey  
#'   
#' @format
#' A dim = 108138 x 79 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab SID \tab integer \tab species identification number\cr
#' [,3] \tab CRUISEJOIN \tab integer \tab insert_description_here\cr
#' [,4] \tab HAULJOIN \tab integer \tab insert_description_here\cr
#' [,5] \tab REGION \tab character \tab insert_description_here\cr
#' [,6] \tab vessel \tab character \tab vessel ID\cr
#' [,7] \tab cruise \tab character \tab cruise ID\cr
#' [,8] \tab haul \tab character \tab the integer haul number within a cruise\cr
#' [,9] \tab CATCHJOIN \tab integer \tab insert_description_here\cr
#' [,10] \tab weight \tab numeric \tab the weight (mass) of all items in the net (may be extrapolated)\cr
#' [,11] \tab cnt \tab numeric \tab number of individuals in the whole net (may be extrapolated)\cr
#' [,12] \tab SUBSAMPLE_CODE \tab logical \tab insert_description_here\cr
#' [,13] \tab VOUCHER \tab integer \tab insert_description_here\cr
#' [,14] \tab AUDITJOIN \tab integer \tab insert_description_here\cr
#' [,15] \tab HAUL_TYPE \tab integer \tab insert_description_here\cr
#' [,16] \tab PERFORMANCE \tab numeric \tab insert_description_here\cr
#' [,17] \tab datetime \tab c("POSIXct", "POSIXt") \tab the day and time of the haul\cr
#' [,18] \tab towduration \tab numeric \tab the duration (time) for the tow\cr
#' [,19] \tab towdistance \tab numeric \tab insert_description_here\cr
#' [,20] \tab gearsize \tab numeric \tab the dimension of the gear; for trawl net, the width of the mouth in ft\cr
#' [,21] \tab NET_MEASURED \tab character \tab insert_description_here\cr
#' [,22] \tab NET_HEIGHT \tab numeric \tab insert_description_here\cr
#' [,23] \tab stratum \tab character \tab the statistical stratum of the haul\cr
#' [,24] \tab lat.start \tab numeric \tab insert_description_here\cr
#' [,25] \tab lat.end \tab numeric \tab insert_description_here\cr
#' [,26] \tab lon.start \tab numeric \tab insert_description_here\cr
#' [,27] \tab lon.end \tab numeric \tab insert_description_here\cr
#' [,28] \tab station \tab character \tab the station ID for the haul\cr
#' [,29] \tab GEAR_DEPTH \tab integer \tab insert_description_here\cr
#' [,30] \tab depth \tab numeric \tab the maximum depth of the water at the location of the haul\cr
#' [,31] \tab BOTTOM_TYPE \tab character \tab insert_description_here\cr
#' [,32] \tab stemp \tab numeric \tab water temperature at the surface at the location of the haul\cr
#' [,33] \tab btemp \tab numeric \tab water temperature at the bottom at the location of the haul\cr
#' [,34] \tab WIRE_LENGTH \tab integer \tab insert_description_here\cr
#' [,35] \tab geartype \tab character \tab code for the type of gear used\cr
#' [,36] \tab ACCESSORIES \tab character \tab insert_description_here\cr
#' [,37] \tab SUBSAMPLE \tab character \tab insert_description_here\cr
#' [,38] \tab REVISION \tab character \tab insert_description_here\cr
#' [,39] \tab BS \tab character \tab insert_description_here\cr
#' [,40] \tab GOA \tab character \tab insert_description_here\cr
#' [,41] \tab WC \tab character \tab insert_description_here\cr
#' [,42] \tab haulid \tab character \tab a unique identifier for the haul; vessel ID - cruise ID - haul number\cr
#' [,43] \tab year \tab numeric \tab year of haul\cr
#' [,44] \tab season \tab character \tab insert_description_here\cr
#' [,45] \tab lat \tab numeric \tab latitude of the haul\cr
#' [,46] \tab lon \tab numeric \tab longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)\cr
#' [,47] \tab stratumarea \tab numeric \tab the area of the statistical stratum (km2)\cr
#' [,48] \tab towarea \tab numeric \tab insert_description_here\cr
#' [,49] \tab effort \tab numeric \tab some measure of the effort for this catch; could be area, time, speed, net size, or some combination. When possible, was converted to area swept by net during the entire tow\cr
#' [,50] \tab wtcpue \tab numeric \tab weight (mass) of the catch\cr
#' [,51] \tab cntcpue \tab numeric \tab number of individuals caught per hectare in the haul\cr
#' [,52] \tab reg \tab character \tab survey region\cr
#' [,53] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,54] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,55] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,56] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,57] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,58] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,59] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,60] \tab species \tab character \tab the species name of the species\cr
#' [,61] \tab genus \tab character \tab the genus of the species\cr
#' [,62] \tab family \tab character \tab taxonomic family\cr
#' [,63] \tab order \tab character \tab taxonomic order\cr
#' [,64] \tab class \tab character \tab taxonomic class\cr
#' [,65] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,66] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,67] \tab phylum \tab character \tab insert_description_here\cr
#' [,68] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,69] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,70] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,71] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,72] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,73] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,74] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,75] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,76] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,77] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,78] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' [,79] \tab keep.row \tab logical \tab Column indicating whether or not the row show likely be excluded\cr
#' }
"clean.wctri"


#' Common Names  
#'   
#' Species's common names matched to scientific names  
#'   
#' @format
#' A dim = 3548 x 2 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab sppCorr \tab character \tab insert_description_here\cr
#' [,2] \tab common \tab character \tab the common name of the organism sampled\cr
#' }
"getCmmnData"


#' Spp Names  
#'   
#' Scientific names matched to raw taxonomic name entries  
#'   
#' @format
#' A dim = 6016 x 2 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,2] \tab sppCorr \tab character \tab insert_description_here\cr
#' }
"getSppData"


#' Taxonomic Classification  
#'   
#' Taxonomic classification matched to scientific names  
#'   
#' @format
#' A dim = 3548 x 11 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab sppCorr \tab character \tab insert_description_here\cr
#' [,2] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,3] \tab species \tab character \tab the species name of the species\cr
#' [,4] \tab genus \tab character \tab the genus of the species\cr
#' [,5] \tab family \tab character \tab taxonomic family\cr
#' [,6] \tab order \tab character \tab taxonomic order\cr
#' [,7] \tab class \tab character \tab taxonomic class\cr
#' [,8] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,9] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,10] \tab phylum \tab character \tab insert_description_here\cr
#' [,11] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' }
"getTaxData"


#' Corrected Species Names (legacy)  
#'   
#' Species names that had been corrected in older version of code  
#'   
#' @format
#' A dim = 3990 x 2 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,2] \tab sppCorr \tab character \tab insert_description_here\cr
#' }
"spp.corr1"


#' Taxonomic Information (legacy)  
#'   
#' Taxonomic classification and ecological information  
#'   
#' @format
#' A dim = 4483 x 20 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab raw.spp \tab character \tab insert_description_here\cr
#' [,2] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,3] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,4] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,5] \tab species \tab character \tab the species name of the species\cr
#' [,6] \tab genus \tab character \tab the genus of the species\cr
#' [,7] \tab family \tab character \tab taxonomic family\cr
#' [,8] \tab order \tab character \tab taxonomic order\cr
#' [,9] \tab class \tab character \tab taxonomic class\cr
#' [,10] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,11] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,12] \tab phylum \tab character \tab insert_description_here\cr
#' [,13] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,14] \tab isSpecies \tab logical \tab insert_description_here\cr
#' [,15] \tab correctSpp \tab logical \tab insert_description_here\cr
#' [,16] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,17] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,18] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,19] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,20] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' }
"taxInfo"


#' Species Key  
#'   
#' Key to taxonomic and ecological information for all species surveyed  
#'   
#' @format
#' A dim = 7146 x 27 data.table data.frame:  
#' \tabular{rlll}{
#' [,1] \tab ref \tab character \tab reference taxonomic ID from raw data\cr
#' [,2] \tab val.src \tab character \tab indicates the degree of 'fuzziness' required to find a match to ref in a data.base of taxonomic information; m1 indicates perfect match, m2 indicates that capitalization, whitespace, etc (see \code{\link{cull}}) needed to be adjusted, m3 indicates that \code{\link{agrep}} was used, and m4 means that measures in both m2 and m3 were taken to find the match\cr
#' [,3] \tab tbl.row \tab integer \tab the row in the taxonomic data base where a match was found\cr
#' [,4] \tab mtch.src \tab integer \tab the database containing the match; 1 is taxInfo, 2 is spp.corr1, 3 is getSppData; if matches are found in multiple sources, a match to 1 takes precedence over 1 & 2, and 2 over 3\cr
#' [,5] \tab tax.src \tab character \tab informs source of taxonomic correction of ref to spp and other tax info; is taxInfo if found from manually checked spreadsheet\cr
#' [,6] \tab spp \tab character \tab species scientific name; Genus species\cr
#' [,7] \tab common \tab character \tab the common name of the organism sampled\cr
#' [,8] \tab taxLvl \tab character \tab the most specific level of classification indicated by spp\cr
#' [,9] \tab species \tab character \tab the species name of the species\cr
#' [,10] \tab genus \tab character \tab the genus of the species\cr
#' [,11] \tab family \tab character \tab taxonomic family\cr
#' [,12] \tab order \tab character \tab taxonomic order\cr
#' [,13] \tab class \tab character \tab taxonomic class\cr
#' [,14] \tab superclass \tab character \tab taxonomic superclass\cr
#' [,15] \tab subphylum \tab character \tab taxonomic subphylum\cr
#' [,16] \tab phylum \tab character \tab insert_description_here\cr
#' [,17] \tab kingdom \tab character \tab taxonomic kingdom\cr
#' [,18] \tab trophicDiet \tab character \tab source of trophic level from Fish Base or Sea Life Base; 'y' means it was from this source\cr
#' [,19] \tab trophicOrig \tab character \tab from Fish Base or Sea Life Base; was the trophic level estimated from an 'Original sample'?\cr
#' [,20] \tab Picture \tab character \tab Is there a picture of this critter assoicated with the package? Note: this isn't always accurate\cr
#' [,21] \tab trophicLevel \tab numeric \tab the trophic level from Fish Base or Sea Life Base\cr
#' [,22] \tab trophicLevel.se \tab numeric \tab the standard error of the trophic level\cr
#' [,23] \tab tax.src2 \tab character \tab informs source of taxonomic correct; the name of a source of taxonomic information other than taxInfo (other than manual entries)\cr
#' [,24] \tab conflict \tab logical \tab for a given 'spp' value in spp.key, was there a conflict in the other taxonomic columns? E.g., a single spp corresponding to multiple common names; also TRUE if different ref values were found in different databases (affecting the val.src, tbl.row, mtch.src, tax.src, etc columns), but then the refs converged to same spp -- that would not necessarily be an error, but might deserve checking\cr
#' [,25] \tab flag \tab character \tab flag related to correcting taxonomic information; relates to automated input, potential errors, and signature of people or methods that have made corrections to spp.key\cr
#' [,26] \tab website \tab character \tab URL reference for taxonomic information\cr
#' [,27] \tab website2 \tab character \tab secondary URL reference for taxonomic information; often used when website finds the name for a spp name as entered, but website2 points to the most up-to-date scientific name\cr
#' }
"spp.key"



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
