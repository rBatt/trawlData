#' Check Strata
#' 
#' Visualize which strata were visited in each year, and how many strata were visited in all but N years
#' 
#' @param X A trawl data.table
#' @param reg region name
#' @param gridSize grid size to be passed to \code{\link{ll2strat}}
#' @param append_keep_strat Logical, whether to add the \code{keep_strat} column to \code{X}
#' @param prompt_strat_tol Logical, if in \code{\link{interactive}} mode, prompt user for tolerance? If not, and if \code{append_keep_strat} is TRUE and \code{strat_tol} is left \code{\link{missing}}, then a default will be selected for \code{strat_tol}
#' @param strat_tol The maximum number of unsampled years that is tolerated for any stratum before all rows corresponding to that stratum have their value in the "keep_strat" column set to FALSE
#' @param plot Logical, visualize strata over time and the number of strata sampled in all but N years?
#' 
#' @details
#' The aim of the function is to guide the selection of which strata to exclude from analysis because they are not sampled often enough. Having fewer gaps in your data set is better, but sometimes tolerating a tiny amount of missingness can result in huge increases in data; the visualization provided by this funciton will help gauge that tradeoff.
#' 
#' @return Nothing. However, potentially modifies X, and may create a figure.
#' 
#' @examples
#' # quick example for goa
#' check_strat(clean.goa[,c("year"):=list(as.integer(year))], "goa")
#' 
#' # a more thorough data processing example for shelf
#' \dontrun{
#' 	# trim shelf
#' 	shelf <- trawlTrim("shelf", c.add=c("val.src", "flag"))
#' 	shelf <- shelf[
#' 		(taxLvl=="species" |taxLvl=="subspecies") & 
#' 		(flag!="bad" | is.na(flag)) & 
#' 		(val.src!="m3" | (!is.na(flag) & flag!="bad"))
#' 	]
#' 
#' 	# aggregate species within a haul (among individuals)
#' 	# this means taking the sum of many bio metrics
#' 	shelf <- trawlAgg(
#' 		X=shelf,
#' 		bioFun=sumna,
#' 		envFun=meanna,
#' 		bio_lvl="spp", space_lvl="haulid", time_lvl="haulid",
#' 		bioCols=c("wtcpue"),
#' 		envCols=c("btemp"),
#' 		metaCols=c("reg","common","year","datetime","stratum", "lon", "lat"),
#' 		meta.action=c("unique1")
#' 	)
#' 
#' 	# aggregate within a species within stratum
#' 	# refer to the time_lvl column from previous trawlAgg()
#' 	# can use mean for both bio and env
#' 	shelf[,stratum:=ll2strat(lon, lat)]
#' 	shelf <- trawlAgg(
#' 		X=shelf,
#' 		FUN=meanna,
#' 		bio_lvl="spp", space_lvl="stratum", time_lvl="year",
#' 		bioCols=c("wtcpue"),
#' 		envCols=c("btemp"),
#' 		metaCols=c("reg","common", "lon", "lat"),
#' 		meta.action=c("FUN"),
#' 		metaFun=list(reg=unique, common=unique, lon=mean, lat=mean)
#' 	)
#' 	setnames(shelf, "time_lvl", "year")
#' 	shelf[,year:=as.integer(as.character(year))]
#' 	setcolorder(shelf, c(
#' 		"reg", "year", "stratum", "lon", "lat", 
#' 		"spp", "common", "btemp", "wtcpue", "nAgg"
#' 	))
#' 	setkey(shelf, reg, year, stratum, spp, common)
#' }
#' 
#' @export
check_strat <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "sa", "sgulf", "shelf", "wcann", "wctri"), gridSize=1, append_keep_strat=FALSE, prompt_strat_tol=FALSE, strat_tol, plot=TRUE){
	
	stopifnot(is.data.table(X))
	stopifnot(X[,is.numeric(year)])
	stopifnot(X[,is.numeric(lon)])
	stopifnot(X[,is.numeric(lat)])
	stopifnot("stratum"%in%names(X))
	
	reg <- match.arg(reg)
	
	if(prompt_strat_tol){
		prompt_strat_tol <- interactive()
	}
	if(missing(strat_tol) & !prompt_strat_tol & append_keep_strat){
		strat_tol <- c(ai=3, ebs=5, gmex=4, goa=3, neus=5, newf=4, sa=0, sgulf=2, shelf=6, wcann=2, wctri=3)[reg]
	}
	
	
	
	# ==================
	# = Create Stratum =
	# ==================
	nT <- X[,length(unique(year, na.rm=TRUE))]

	# ===============
	# = Make Figure =
	# ===============
	lat.range <- X[,range(lat, na.rm=TRUE)]
	lon.range <- X[,range(lon, na.rm=TRUE)]
	
	strat_table <- X[,colSums(table(year, stratum)>0)]
	nstrata <- c()
	for(i in 0:(nT-1)){
		nstrata[i+1] <- X[,sum(strat_table>=(nT-i))]
	}

	if(plot){
		# Initialize graphical device
		layout(matrix(c(rep(1,3), rep(2,3), rep(1,3), rep(2,3), 3:8),ncol=3))
		par(mar=c(2.0,1.75,1,0.1), mgp=c(1,0.15,0), tcl=-0.15, ps=8, cex=1, family="Times")

		# Tolerance vs. Missingness Panels
		plot(0:(nT-1), nstrata, type="o", xlab="N years missing", ylab="Number of strata missing in fewer than N years", main="# strata vs. tolerance of missingness")
		image(x=X[,sort(unique(year))], y=X[,(1:length(unique(stratum)))], z=X[,table(year, stratum)>0], xlab="year", ylab="stratum", main="stratum presence vs. time; red is absent")

		# Tolerance Maps
		par(mar=c(1.25,1.25,0.1,0.1), mgp=c(1,0.15,0), tcl=-0.15, ps=8, cex=1, family="Times")
	
		tol0_ind <- strat_table >= (nT-0)

		tol_plot <- function(lon,lat){
			col <- 1+(!paste(lon,lat)%in%tol0[,paste(lon,lat)])
			plot(lon, lat, xlab="", ylab="", xlim=lon.range, ylim=lat.range, col=col)
		}
		tol0 <- X[stratum%in%X[,names(strat_table)[tol0_ind]]]
		tol0[,c("lat","lon"):=list(roundGrid(lat, gridSize),roundGrid(lon, gridSize))]
		for(i in 1:6){
			name_i <- names(strat_table)[strat_table>=(nT-i)]
			tolC <- X[(stratum %in% name_i)]
			tolC[,c("lat","lon"):=list(roundGrid(lat, gridSize),roundGrid(lon, gridSize))]
			setkey(tolC, stratum, lat, lon)
			tolC <- unique(tolC)
			tolC[,tol_plot(lon,lat)]
			legend("topleft", paste("missing years =",i), inset=c(-0.1, -0.12), bty="n")
		
			tol0 <- tolC
		}
	
	}
	
	
	if(append_keep_strat){
		if(prompt_strat_tol){
			strat_tol <- readline(paste("Select your tolerance for missingness in",reg,":\n"))
		}
	
		goodStrat2 <- X[,names(strat_table)[strat_table>=(nT-strat_tol)]]
		X[,keep_strat:=(stratum%in%goodStrat2)]
		
	}
	
	invisible(NULL)
	
}
