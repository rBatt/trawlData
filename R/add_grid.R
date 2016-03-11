#' Add Gridded Data
#' 
#' Added a gridded environmental data set to a data.table of clean trawl survey data
#' 
#' @param x_grid gridded environmental data of class raster
#' @param x_dt trawl data.table
#' 
#' @export
add_grid <- function(x_grid, x_dt){
	xg_dt0 <- data.table(as.data.frame(x_grid))
	nms <- names(xg_dt0)
	nms_yr_l <- grepl("X[0-9]{4}", nms)
	if(any(nms_yr_l)){
		nrep <- nrow(xg_dt0)
		xg_dt0 <- reshape2::melt(xg_dt0, id.vars=nms[!nms_yr_l], measure.vars=nms[nms_yr_l])
		xg_dt0[,year:=as.numeric(as.character(rep(gsub("^X", "", nms[nms_yr_l]),each=nrep)))]
		xg_dt0[,variable:=NULL]
	}
	
	xg_lon <- xFromCell(x_grid, 1:ncell(x_grid))
	xg_lat <- yFromCell(x_grid, 1:ncell(x_grid))
	
	xg_dt <- data.table(lon=xg_lon, lat=xg_lat, xg_dt0)
	
	res_x_tbl <- table(x_dt[,diff(sort(unique(lon)))])
	x_res <- as.numeric(names(res_x_tbl)[which.max(res_x_tbl)])
	x_res <- max(c(x_res, res(x_grid)[1]))
	res_y_tbl <- table(x_dt[,diff(sort(unique(lat)))])
	y_res <- as.numeric(names(res_y_tbl)[which.max(res_y_tbl)])
	y_res <- max(c(y_res, res(x_grid)[2]))
	
	old_lon <- x_dt[,lon]
	old_lat <- x_dt[,lat]
	x_dt[,c("lon","lat"):=list(roundGrid(old_lon, x_res), roundGrid(old_lat, y_res))]
	
	xg_dt[,c("lon","lat"):=list(roundGrid(lon, x_res), roundGrid(lat, y_res))]
	
	mby <- c("lon","lat","year")
	mby <- mby[mby%in%names(xg_dt) & mby%in%names(x_dt)]
	merged_g <- merge(x_dt, xg_dt, by=mby, all.x=TRUE, all.y=FALSE)
	merged_g[,c("lon","lat"):=list(old_lon, old_lat)]
	
	return(merged_g)
	
}