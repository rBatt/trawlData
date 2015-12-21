library(hypervolume)
library(spatstat)
library(SpatialTools)

# ---- data ----
tr <- trawlTrim(clean.neus)
tr_a <- trawlAgg(tr, meanna, bio_lvl="spp",space_lvl="haulid",time_lvl="haulid",bioCols="wtcpue",metaCols=c("year","lon","lat"), meta.action="unique1")

# ---- chull ----
ch <- tr_a[chull(lon,lat),list(lon,lat)]
plot(rbind(ch,ch[1]), type="l")

# ---- spatstat ----
tr_owin <- tr_a[,owin(range(lon), range(lat))]
tr_ppp <- tr_a[!duplicated(haulid) & !is.na(btemp) & !duplicated(paste(lon,lat)), ppp(lon, lat, window=tr_owin, marks=btemp)]

# spatstat 1
tr_smoo <- Smooth(tr_ppp, sigma=1.5)
plot(tr_smoo)

# spatstat 2
tr_idw <- idw(tr_ppp, power=2)
plot(tr_idw)

# ---- hypervolume ----
xy_whole <- tr_a[!duplicated(haulid),list(lon,lat)]
xy_1spp <- tr_a[pick(spp,1,w=T),list(lon,lat)]

bw_w <- estimate_bandwidth(xy_whole)
bw_1 <- estimate_bandwidth(xy_1spp)

hv_w <- hypervolume(xy_whole, bandwidth=bw_w/5, repsperpoint=100, quan=0.5)
hv_1 <- hypervolume(xy_1spp, bandwidth=bw_1/5, repsperpoint=100, quan=0.5)
hv <- hypervolume_join(hv_w, hv_1)

plot(hv, pair=TRUE, contour.filled=T)


xyt <- tr_a[pick(spp,1,w=T),list(lon,lat,year=as.integer(year))]
bw_xyt1 <- estimate_bandwidth(xyt)
hv_t <- hypervolume(xyt, bandwidth=bw_xyt1, repsperpoint=50)
plot(hv_t, pair=F)


# ---- use hypervolume range for spatstat window ----
hv_w_rpoints <- hv_w@RandomUniformPointsThresholded
hv_w_kd <- kde2d(hv_w_rpoints[,1], hv_w_rpoints[,2])
hv_w_contLevel <- min(hv_w_kd$z) + diff(range(hv_w_kd$z)) * 0.5
hv_w_cLines <- contourLines(hv_w_kd, level=hv_w_contLevel)

hv_w_cLines_mat <- lapply(hv_w_cLines, function(x)get.contours(list(x)))
hv_w_cLines_mat <- lapply(hv_w_cLines_mat, function(x)x[nrow(x):1,])

hv_w_owin <- owin(xy_whole[,range(lon)], xy_whole[,range(lat)], poly=hv_w_cLines_mat)

tr_ppp_hv <- tr_a[!duplicated(haulid) & !is.na(btemp) & !duplicated(paste(lon,lat)), ppp(lon, lat, window=hv_w_owin, marks=btemp)]

tr_idw_hv <- idw(tr_ppp_hv, power=2)
plot(tr_idw_hv)
lapply(hv_w_cLines_mat, lines)

