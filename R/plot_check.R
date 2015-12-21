#' Plot Check
#' 
#' Check raw data by plotting against time, lon, and lat
#' 
#' @param X A trawl data.table
#' @param check_cols The name of the column(s) to check
#' @param by A column named to be used for grouping
#' @param min_n_obs minimum number of non-NA values in \code{check_col} in each group specified in \code{by}
#' 
#' @details
#' Checks raw data by creating figures
#' 
#' @return Nothing
#' 
#' 
#' @examples
#' 
#' ol_plot <- plot_check(clean.sgulf)
#' clean.sgulf[ol_plot$outliers]
#' pairs(ol_plot)
plot_check <- function(X, check_cols=c("depth","btemp","stemp"), by=c("spp"), na.handle=c("rm","fill_0","fill_mean","fill_sample"), ...){
	
	check_cols <- match.arg(check_cols, choices=names(X), several.ok=TRUE)
	
	ccX <- complete.cases(X)
	d <- X[, eval(s2c(check_cols))]
	if(length(check_cols)==1L){
		d <- cbind(d, sample(d[[1]], replace=TRUE, size=length(d)))
	}
	
	ol <- mvoutlier::uni.plot(d)
	
	return(list(ol, sub_ind=ccX, d))
	
}





