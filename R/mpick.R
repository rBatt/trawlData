#' Subsetting by picking random levels from multiple factors
#' 
#' Like \code{\link{pick}}, but allows specifying multiple factors (columns) at the same time, trying hard to return the desired result. You want 2 species from the same 3 strata during the same 4 years? Use \code{mpick}. Want just one of those? Use \code{pick}.
#' 
#' @param X A data.table 
#' @param A named vector of integers. Names are columns in \code{X}, the integers are the number of levels to select
#' @param weight Logical, default FALSE. Same as \code{w} in \code{\link{pick}}; weight selection of factor level by its relevative frequency of occurrence in \code{X}. Could have performance implications, see 'Details'.
#' @param limit Time limit for searching, in seconds
#' @param screen Logical If TRUE (default) then before random searching, will screen out factor levels that definitely cannot satisfy the full sweet of conditions in \code{p}. Can be a little slow, but is extremely effect when most combinations of factors in \code{p} do not exist. See 'Details'.
#' @param dt Logical, if TRUE, returns a data.table; if FALSE (default), returns an index of that data.table?
#' 
#' @details
#' This problem may ultimately be better suited for a real optimization algorithm. Right now, relies and arbitrary guess-and-check. Does not "forget" failed guesses (only specific combinations are worth forgetting, and for large data sets there's a very low probability of happening upon same combination). Thus, this is a very brute-force approach, with the exception of the checking done when \code{screen=TRUE}.
#' 
#' It is highly recommended that \code{limit} be set to allow for a couple minutes of searching. Of course, this depends on the size of \code{X} and the details of \code{p}.
#' 
#' \code{screen} is very effective when many possible factor levels in \code{p} can be ruled out based on their overall scarcity. Consider the example of 2 spp, 3 stratum, 4 year. If a given level of spp does not occur at least 3*4=12 times in the data set, it can be ruled out. Because very rare species comprise the majority of unique spp in trawl data, this screening can be outstandingly effective.
#' 
#' Be aware that it is easy to accidentally ask a lot of this function, and don't be surprised when it doesn't give you an answer quickly, or at all. For example, asking for 10 spp 5 stratum 5 year might seem meager for a data set observed over 30 years for 100 strata and 800 spp. However, this is a big ask: 10 species found together in the same 5 places in each of 5 years. If the average stratum has about 30 species, you're requesting that a 3rd of the local biodiversity constitute the same species 25 separate times. If a stratum is small or if species are cosmopolitan, you might get a good result; but that'd be lucky.
#' 
#' @section Warning:
#' This function is still experimental. See \url{http://stackoverflow.com/q/33714985/2343633} for possible updates (but this was not a popular question).
#' 
#' @return
#' A data.table that is a subset of \code{X}.
#' 
#' @examples
#' # simple and fast example
#' set.seed(1337)
#' mpick(clean.ebs, p=c(spp=2, year=1), weight=TRUE, screen=TRUE, dt=TRUE)
#' 
#' # More complex example
#' # if we want 5 spp that are
#' # found in the same 5 strat in
#' # at least 1 year; but then
#' # we want to allow for +/- 2 years
#' # on either side of that shared year
#' # First we get the 5-5-1 subset index,
#' # Then we search for those chosen spp-stratum-year,
#' # but then we also search for the additional years
#' \dontrun{
#' set.seed(1337)
#' ind <- mpick(clean.ebs, p=c(spp=5, stratum=5, year=1), weight=TRUE, limit=60)
#' logic <- expression(
#' 	spp%in%spp[ind]
#' 	& stratum%in%stratum[ind]
#' 	& as.integer(year)%in%(as.integer(unique(year[ind])) + (-2:2))
#' )
#' clean.ebs[eval(logic)]
#' }
#' 
#' @export
mpick <- function(X, p, weight=FALSE, limit=10, screen=TRUE, dt=FALSE){
	
	# Names of factors
	pName <- names(p)
	
	if(screen){
		# Reduce data set size
		setkeyv(X, pName)
		X1 <- unique(X)

		# Count combinations
		t0 <- X1[,table(.SD[,eval(s2c(pName))])]

		# Which combinations occur at least once?
		t1 <- t0>0

		# Convent vector
		ps <- 1:length(p)

		# First round of checks
		# Take F-1 factors at a time, and
		# find which combinations
		# occur in at least N[i]
		# levels of the 1 remaining factor,
		# where N[i] is the desired number of levels
		# for that remaining factor
		checks <- list()
		for(i in ps){
			# Grab dimensions for storing
			tdim <- c(dim(t1)[ps[-i]], dim(t1)[i])

			# Grab a vector that will help put back in order
			un.ind <- c(ps[ps[-i]], ps[i])

			# Count occurrences in combinations of remaining factors
			cnt <- apply(t1, ps[-i], sum)

			# At least N[-1] combinations?
			# Store so repeated across levels of remaining factor
			logic <- array(cnt >= p[i], dim=tdim)

			# Rearrange back to original array dim, and store
			checks[[i]] <- aperm(logic, order(un.ind))
		}

		# For each combination of factors,
		# How many of the leave-one-out tests did it pass?
		nFinds <- Reduce("+", checks)

		# Which combinations passed each of the tests?
		minPass <- which(nFinds==length(p), arr.ind=T)
	
		if(length(minPass)==0){
			stop("Pre-screen has determined the desired combinations do not exist. Lower the number of factor levels.")
		}

		# What are the names of those combinations?
		checked <- t(apply(minPass, 1, function(x)mapply("[", dimnames(t0), x)))

		# Second round of checks
		# Taking each factor separately,
		# make sure it occurs in at least
		# prod(N[-i]) combinations of the other
		# factors, where the desired number
		# of levels for those factors is N[-i]
		sub.frame <- as.data.frame(checked) # for subsetting
		sub.frame[] <- 0 # replace with some arb value
		for(i in ps){ # for each factor
			tt <- table(checked[,i]) #
			sub.frame[,i] <- checked[,i]%in%(names(tt)[tt>=prod(p[-i])])

		}
		checked2 <- as.data.table(checked[apply(sub.frame, 1, all),])

		ref <- copy(X)
		for(i in ps){
			tn <- pName[i]
			ind <- ref[,eval(s2c(tn))[[1]]]%in%checked2[,eval(s2c(tn))[[1]]]
			ref <- ref[ind]
		}
	}else{
		ref <- copy(X)
	}
	
	
	f <- function(x){
		i1 <- x[,sapply(eval(s2c(pName)), lu)]
		# print(i1)
		# flush.console()
		!all(x[,table(eval(s2c(pName)))]>0) | !all(i1==p)
	}
	
	start <- Sys.time()
	i <- 0
	ind0 <- ref[,as.data.frame(mapply(pick, eval(s2c(pName)), p, MoreArgs=list(w=weight)))]
	ind <- apply(ind0, 1, all)
	x <- ref[ind]
	elapsed <- difftime(Sys.time(), start, units="sec")
	while(f(x) & (elapsed<limit)){
		ind0 <- ref[,as.data.frame(mapply(pick, eval(s2c(pName)), p, MoreArgs=list(w=weight)))]
		ind <- apply(ind0, 1, all)
		x <- ref[ind]
		# if(!f(x)){
		# 	X <- X[!ind]
		# }
		i <- i+1
	
		elapsed <- difftime(Sys.time(), start, units="sec")
	}
	
	message(paste0("Time elapsed: ", elapsed,"\n","Combinations tried: ", i,"\n\n"))
	if(f(x) & elapsed>=limit){
		stop("Time limit reached, no complete subsets match found. Try increasing limit or lowering the number of factor levels.")
	}
	
	if(f(x)){
		stop("No complete matches found. Lower the number of factor levels.")
	}
	
	if(!dt){
		ind <- Reduce("&", 
			as.data.frame(mapply("%in%", 
					X[,eval(s2c(pName))], 
					x[,eval(s2c(pName))]
			))
		)
		return(ind)
	}else{
		return(x)
	}
	

}
