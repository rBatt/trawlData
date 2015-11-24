#' msomData
#' 
#' Format Data for MSOM
#' 
#' @param Data A data.table containing response variable and covariates
#' @param n0 An integer indicating the number of never-observed species to add
#' @param formula A formula indicating the dimensions of the output array (only Default tested)
#' @param cov.vars A named character vector of covariates, where character elements correspond to columns \code{Data}, and names will be predictor names in model
#' @param u.form Formula for covariates to be included in U, the matrix of presence covariates
#' @param v.form Formula for covariates to be included in V, the matrix of detection covariates
#' @param valueName Character indicating column name containing response in \code{Data}
#' @param cov.by Chracter vector of dimension names for covariates; order matters, specify most specific last. Should be elements in \code{formula}, and columns in \code{Data}.
#' 
#' @details
#' Only the defaults have been tested. Arguments that should be manipulated include \code{u.form}, \code{v.form}, \code{cov.vars}, and \code{n0}. Other values can be changed if they are only being adjusted to conform with column names in \code{Data}. Otherwise, it is unlikely that this function will behave as desired. In particular, the nesting of year, stratum, and K has to be the same (or whatever names are used).
#' 
#' Casting of \code{Data} is done via \code{trawlCast}. 0's are filled where appropriate, and NA's indicate no sampling. In the output, all NA's are replaced with 0's. However, the \code{nK} element of the output list indicates how many of the K are actually samples, and because K is not ordered, any 0's beyond the magnitude indicated in nK for that year-stratum combination are actually the NA's.
#' 
#' @return
#' A named list appropriate for use with msomStatic.stan
#' 
#' @imports data.table
#' 
#' @export
msomData <- function(Data, n0=10, formula=year~stratum~K~spp, cov.vars=c(bt="btemp",doy="doy",yr="year"), u.form=~bt+I(bt^2), v.form=~doy+I(doy^2)+yr, valueName="abund", cov.by=c("year","stratum","K")){
	
	cov.vars <<- cov.vars # stupid bug
	
	if(class(formula)=="formula"){
		gno <- unlist(strsplit(deparse(formula), "\\s*~\\s*"))
	}else if(class(formula)=="character"){
		gno <- unlist(strsplit(formula, "\\s*~\\s*"))
	}
	
	stopifnot(n0>=1)
	stopifnot(all(gno%in%names(Data)))
	stopifnot(all(cov.vars%in%names(Data)))
	stopifnot(valueName%in%names(Data))
	stopifnot(all(cov.by%in%gno))
	
	nK.form <- paste(gno[1:2], collapse="~")
	
	Xc <- trawlCast(x=Data, formula=formula, valueName=valueName, grandNamesOut=gno)
	nK <- trawlCast(Data, 
		nK.form, 
		valueName=tail(gno,2)[1], #"K", 
		fixAbsent=FALSE, 
		fun.aggregate=max, 
		valFill=0, 
		grandNamesOut=head(gno, length(gno)-2)# c("j","t")
	) # used to indicate which values in Xc are NA, basically


	# Get covariates for U
	# Choose names of covariates
	# The names of the vector are the names you'll get in the end
	# The character elements of the vector should correspond to columns
	# cov.vars <- c(bt="btemp",doy="doy",yr="year") # order does not matter
	# cov.vars <- c(btemp="bt",doy="doy",year="yr") # if wanted opposite convention

	# Grouping for covariates
	# Correspond to column names
	# cov.by <- c("year","stratum","K")# the order matters! most specific last

	# Aggregate covariates
	# Define expression to agg by 
	# applying una() function
	# Sets names, too
	una.cov <- expression({
		structure(lapply(eval(s2c(cov.vars)), una, na.rm=TRUE),.Names=names(cov.vars))
	})
	cov.tjk <- Data[,eval(una.cov), keyby=cov.by] # also setting key

	# Fill in NA covariates
	# Define expression & functions to fill using mean
	# It is implied that to get the mean, you look 1 level higher
	# than the most specific favor in cov.by
	# This is why the order of cov.by matters, a lot
	# PRETTX PRESUMPTUOUS / FRAGILE CODE
	is.ci <- function(x)is.numeric(x) | is.integer(x)
	fm2 <- function(x){
		if(!any(is.na(x))){
			return(x)
		}
		if(!is.ci(x)){
			warning("Covariate contains NA's, but fill.mean needs numeric or integer")
			return(x)
		}else{
			cl <- class(x)
			as(fill.mean(x), cl)
		}	
	}
	fillMean.cov <- expression({
		structure(
			c(
				lapply(eval(s2c(names(cov.vars))), fm2)
			),
			.Names=names(cov.vars)
		)
	})

	cov.tjk[,c(names(cov.vars)):=eval(fillMean.cov),by=c(cov.by[-length(cov.by)])]



	# Check
	stopifnot(!any(is.na(cov.tjk))) # can't have any NA's with my current simple approach

	# Set up template for expanding covariates
	template <- unique(data.table(reshape2:::melt(Xc), key=c(cov.by)))[,eval(s2c(cov.by))]
	template[,c(names(template)):=lapply(.SD, as.character)]
	cov.tjk[,c(cov.by):=lapply(.SD[,eval(s2c(cov.by))], as.character)]

	# Fill out (expand) covariate data.table
	cov.f <- merge(template, cov.tjk, all=TRUE, by=cov.by) # filled cov
	
	
	# Get Covariates (U and V)
	getUV <- function(form){
		UV.m <- model.matrix(form, model.frame(form,data=cov.f, na.action=na.pass))
		ncUV <- ncol(UV.m)
		nrUV <- nrow(UV.m)
		namesUV <- gsub("I\\(|\\(|\\)|\\^", "", colnames(UV.m))
		colnames(UV.m) <- namesUV

		UV.dt0 <- data.table(cov.f[,eval(s2c(cov.by))],UV.m)
		UV.dt <- data.table:::melt.data.table(UV.dt0, id.var=cov.by)
		UV.form <- paste(c(cov.by,"variable"),collapse="~")
	
		reshape2::acast(UV.dt, formula=UV.form, value.var="value")
	}

	U <- getUV(u.form)
	nU <- tail(dim(U),1)

	V <- getUV(v.form)
	nV <- tail(dim(V),1)


	# Add never-observed species to array
	add_neverObs <- function(x, n0){
		fillA <- do.call(`[`, c(list(x), rep(TRUE, length(dim(x))-1), 1))
		fillA[!is.na(fillA)] <- 0
		X0 <- replicate(n0, fillA)
	
		outDim <- c(head(dim(x),-1), tail(dim(x),1)+n0)
		outA <- array(c(x, X0), dim=outDim)
	
		return(outA)
	
	}
	X <- add_neverObs(Xc, n0=n0)


	# test if it'd work in stan
	nT <- length(dimnames(Xc)$year)
	Jmax <- length(dimnames(Xc)$stratum)
	Kmax <- length(dimnames(Xc)$K)
	N <- length(dimnames(Xc)$spp)
	nS <- N + n0

	# for(t in 1:nT){
	# 	for(j in 1:Jmax){
	# 		t.K <- nK[t,j]
	# 		if(t.K){
	# 			t.dat <- unname(Xc[t,j,,])
	# 			print(t.dat)
	# 			# for(k in 1:t.K){
	# #
	# # 			}
	# 		}
	# 	}
	# }


	# Convert NA's to 0's, so they Stan doesn't get mad.
	# But don't worry, these converts will be skipped by loop,
	# thanks to indexing provided by nK. 
	# Reminder: nK tells us how many reps, 
	# or if 0, that a J-T combo doesn't exist
	X[is.na(X)] <- 0

	U[is.na(U)] <- 0
	V[is.na(V)] <- 0

	out <- list(
		X=X,
		U=U,
		V=V,
		nU=nU,
		nV=nV,
		nK=nK,
		nT=nT,
		Jmax=Jmax,
		Kmax=Kmax,
		N=N,
		nS=nS
	)
	
	return(out)
}