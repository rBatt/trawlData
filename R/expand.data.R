


#'@param comD the "compact" data set, supplied as a data.table

#'@param keyID columns in keyID will either define the arr.dim, the elements of the output array, or, for those elements not in either of those, will be aggreated over

#'@param keyValue the name of the column whose values, along with those in fillValue, will form the elements of the output array

#'@param arr.dim character vector specifying the names of columns that would specify the dimensions of the output array

#'@param scope character vector specifying columns. The number of unique combinations of factors among those columns will specify the number of output arrays. If left NULL, there will be 1 output array with dimension sizes equal to the number of factor levels in the respective elements of comD.

#'@param redID a list of the categories to uniquely define redValue; can only include values in arr.dim, but must not include all of them

#'@param redValue a list of character vectors whose elements correspond to column names whose levels are redundant with some subset of keyID; e.g., if a keyID is "species", then redValue would include columns like Genus and Family, specified as list(c("Genus","Family")), and the corresponding redID would be list(c("species")).

#'@param fillID a list of character vectors, each of which is a subset of scope, and when missing levels of these factor/s are added for existing combinations of setdiff(arr.dim, fillID[[i]]), should be filled in with the corresponding value of fillValue. Note that the last element of fillID will always contain the full set in arr.dim, because the resulting array cannot be ragged.

#'@param fillValue a vector of values that should be used to fill in missing combinations of arr.dim. The class of fillValue should match the class of keyValue.

#'@param arrayOut logical; Defaults to FALSE, in which case the output is a data.table, with number of rows corresponding to the product of the number of unique values in each of arr.dim (in the case where scope=NULL), or the sum of that product for each of the unique combinations of the levels in scope. If TRUE, the output is an array (if scope=NULL), or list of arrays.

#'@param aggFun Function used to aggregate among levels of columns specified in keyID, but not present in arr.dim.

#'@param maxOut the maximum number of allowable elements in the output array or data.table. In place to prevent early detection of a huge number of combinations that might use up a larger-than-expected amount of memory.


expand.data <- function(comD, arr.dim, keyValue="value", fillID=NULL, fillValue=NA, Rule=NULL, keyID=NULL, gScope=NULL, fScope=NULL, vScope=NULL, redID=NULL, redValue=NULL, arrayOut=FALSE, aggFun=NULL, maxOut=Inf){

	# =========
	# = Setup =
	# =========
	if(is.null(keyID)){
		stop("there is a bug in setting keyID from the key of comD ... I do not understand it either")
		keyID <- key(comD)
	}
	comD <- copy(comD)
	setnames(comD, keyValue, "value")
	# comD0 <- copy(comD)


	# ==========
	# = Checks =
	# ==========
	stopifnot(require(data.table))
	stopifnot(is.data.table(comD))
	stopifnot(class(fillValue)==class(comD[,value]))
	stopifnot(length(redID)==length(redValue))
	stopifnot(length(fillID)==length(fillValue))
	nrow.out <- comD[,prod(sapply(eval(s2c(keyID)), lu))]
	size.out <- nrow.out * length(arr.dim)
	if(arrayOut){
		stopifnot(nrow.out<=maxOut)
	}else{
		stopifnot(size.out<=maxOut)
	}
	stopifnot(is.null(gScope) | !gScope%in%arr.dim)
	if(is.null(fillID)){
		stopifnot(is.null(unlist(list(fillID, fScope, vScope, Rule))))
	}
	# TODO  should probably implement a check to make sure that the values in redID don't imply that the output should contain more columns than what are suggested by arr.dim, gScope, and fScope combined.
	
	
	# ========================
	# = A few initial values =
	# ========================
	IDs <- unique(c(gScope, unlist(fScope), arr.dim)) # basically the keyID of the *output* data.table. arr.dim defines the keyID of each array in the list, but the levels of the list (or data.tables stacked on each other, as opposed to crossed) are defined by gScope and fScope
	aggID <- setdiff(keyID, IDs)
	aggFun <- match.fun(aggFun)
	
	
	# ===========================================
	# = Aggregate redValues over marginal keyID =
	# ===========================================
	if(!is.null(redID)){ # if there are redID's that should be added back in ...
		redFill <- vector("list", length(redID))
		for(i in 1:length(redID)){ # for each redundant id/ value ...
			trID <- redID[[i]]
			trV <- redValue[[i]]
			rN <- c(trID, trV) # get the names of the redundant ID (the value chosen to represent others), and the redundant values
			
			setkeyv(comD, trID) # set the key of expD to be the redID
			
			redSet <- unique(data.table(comD[,eval(s2c(rN))], key=c(rN)))
			
			# Check to see if redID uniquely identifies the contents of redValue
			isUnique.red <- nrow(redSet) == comD[,(.GRP),by=c(trID)][,max(V1)]
			if(!isUnique.red){
				if(redSet[,any(sapply(eval(s2c(trV)), class)%in%c("factor","character"))]){
					print(redSet[,{if(.N>1){.SD}},by=c(trID)]) # give user a hint at what went wrong
					stop(paste0("redID ", "\"", trID, "\" ", "does not uniquely define redValue, and redValue contains factors and characters"))
				}else{
					# warning(paste0("redID ", "\"", trID, "\" ", "does not uniquely define redValue, using aggFun"))
					redSet <- redSet[, lapply(eval(s2c(trV)), aggFun), keyby=c(trID)] # aggregate numerics using something like mean
					setnames(redSet, names(redSet), c(rN)) # change oclumn names
				}
			}
			
			redFill[[i]] <- redSet
		} # end loop through redID
	} # end redID if
	
	
	
	
	
	# ============================================
	# = Aggregate keyValue over marginal keyID's =
	# ============================================

	if(length(aggID)>0){ # determines if it's necessary to aggregate
		if(is.null(aggFun)){stop("arr.dim is a subset of names in keyID; must provide an aggregation function via aggFun")}
		# comD <- comD[,value:=eval(s2c(keyValue))] # I overwrite comD to save memory
		comD <- comD[,list(value=aggFun(value)), by=IDs] # aggregate step: used when not all of the values in keyID are part of arr.dim
	}else{ # if it's not necessary to aggregate, still need to format a bit and drop extra columns
		# comD <- comD[,value:=eval(s2c(keyValue))] # TODO should avoid creating a duplicate column just for naming convenience. either delete the old column then name it back later, or just stick to using the actual column name. The former is probably preferable b/c it would save computing eval(s2c()) over and over again
		comD <- comD[,eval(s2c(c(IDs,"value")))]
	}
	
	
	
	
	
	# ===============================
	# = Cross all IDs not in gScope =
	# ===============================
	# I think of this as "exploding" the data.table (but uses gScope to "contain" the explosion)
	# The explosion (id.dt) only contains the combinations of IDs, not the values
	id.dt <- comD[,
		j={
			idset <- do.call(CJ, lapply(eval(s2c(setdiff(IDs,gScope))), unique))
			setnames(idset, names(idset), setdiff(IDs,gScope))
			idset
		},
		by=c(gScope)
	]
	
	
	# =======================================
	# = Merge crossed IDs with data (value) =
	# =======================================
	setkeyv(comD, IDs)
	setkeyv(id.dt, IDs)
	expD <- comD[id.dt] # the merge
	# note that an error messages in the previous line can result from not having the correct keyID (not specific enough keyID)
	
		
	# ==================================================
	# = Trim and polish explosion in fillID dimensions =
	# ==================================================
	# The trim is if Rule = scope, and involves deleting some unecessary combinations
	# The polish is if Rule = value, and involves altering the value of missings created by the explosion
	if(length(fillID)>0){
		
		# First, have to safeguard against polishing gone awry (changing an intentional NA to fillValue)
		# Note that this next line needs to be executed before expD is altered (which is why it isn't under Rule[i]=="value")
		keepNA <- expD[is.na(value)&!is.na(comD[id.dt, which=TRUE])] # keeping track of where NA's are in original data set
		
		for(i in 1:length(fillID)){ # For each dimension of the explosion that is to be tamed ...
			
			t.fID <- fillID[i] # assign the temporary ID of the dimension
			
			if(Rule[i]=="scope"){ # Remove unnecessary (wrong) combinations
				t.cols <- c(fScope[[i]], t.fID)
				orig <- unique(data.table(comD[,eval(s2c(t.cols))], key=c(t.cols))) # the original combinations of IDs
		
				setkeyv(expD, key(orig))
				expD <- expD[orig] # merge (join, subset, really)
			}
			
			if(Rule[i]=="value"){ # Change exploded NA's to fillValue
				t.cols <- vScope[[i]]
				orig <- unique(data.table(comD[,eval(s2c(t.cols))], key=c(t.cols))) # the original combinations of IDs
				
				setkeyv(expD, key(orig))
				expD[orig, t.fill:=fillValue[i]] # new column w/ NA or, if orig IDs found, fillValue
				# expD <- expD[is.na(value), value:=t.fill] # # Switch (all!) NA's to fillValue
				expD[is.na(value), value:=t.fill] # # Switch (all!) NA's to t.fill
				expD[,t.fill:=NULL]
				
				if(nrow(keepNA)>0){ # NAs in original data set will (or can) be replaced by t.fill value, so changing back to NA's
					setkeyv(expD, IDs)
					setkeyv(keepNA, IDs)
					expD[keepNA[,eval(s2c(IDs))],value:=NA] # Change original NA's back to NA
				}
				
			}
		}
	}
	
	# ==============================
	# = Return array or data.table =
	# ==============================
	if(arrayOut){ # return if array
		
		outScope <- union(unlist(gScope), unlist(fScope)) # the output scope determiens the number of arrays that need to be formed
		if(is.null(outScope)){
			outsize <- 1
		}else{
			outsize <- expD[,(.GRP),by=c(outScope)][,max(V1)] # the number of arrays
		}
		
		array.list <- vector("list", outsize) # preallocate data array
		array.key <- vector("list", outsize) # the list of arrays can be long and hard to navigate; this key will supply the outScope combinations present in each element of the array.list output list
		
		# ======================================
		# = Array: Format keyValue into arrays =
		# ======================================
		setkeyv(expD, c(outScope,rev(arr.dim)))
		invisible(expD[, # within the j of this data.table, build each element of the output array list
			j={
				# dim.names <- lapply(test[,eval(s2c(arr.dim))], unique)
				dim.names <- lapply(.SD[,eval(s2c(arr.dim))], unique)
				# array.list[[.GRP]] <<- array(test[,value], dim=sapply(dim.names, length), dimnames=dim.names)
				array.list[[.GRP]] <<- array(.SD[,value], dim=sapply(dim.names, length), dimnames=dim.names)
				# array.list[[.GRP]] <<- cast(.SD, stratum~K~spp)
				
				if(!is.null(outScope)){
					array.key[[.GRP]] <<- c(unlist(.BY),.GRP) # grab the names of the by= groups, add them to the output key
				}
				
			},
			by=c(outScope)
		])
		
		# ===========================================
		# = Array: Build the red values into arrays =
		# ===========================================
		if(!is.null(redID)){ # if there are redID's that should be added back in ...
			red.list <- vector("list", length(unlist(redValue, F, F))) # for final output if using array out format
			for(i in 1:length(red.list)){red.list[[i]]<-vector("list",outsize)}
			ctr <- 0
				
			for(i in 1:length(redID)){ # for each redundant id/ value ...
				setkeyv(expD, redID[[i]])
				setkeyv(redFill[[i]], redID[[i]])
				# expD <- redFill[[i]][expD,][,eval(s2c(c(names(expD),redValue[[i]])))] # 1st part merges, 2nd reorders columns
				t.red <- setcolorder(redFill[[i]][expD,], c(names(expD), redValue[[i]])) # this should be faster
				setkeyv(t.red, c(outScope,rev(arr.dim)))
				
				for(r in 1:length(redValue[[i]])){
					ctr <- ctr + 1 # have to increment counter according to unique redValues (total, not just for this redID); grouping by redID makes the merges more efficient (fewer), but then it also confuses the nesting of lists – just because 2 redValues can be unique identified by the same redID doesn't mean that they should be nested in the array output. Instead, the array output will be a list of 3 elements: a list of n=outsize arrays containing keyValue, a data.table with n=outsize rows detailing what each element of the aforementioned list is obtained from what combinations of outScope, and a list of n=length(unlist(redValue)) lists that each contain n=outsize arrays. So if we want both btemp and depth, they can be uniquely identified by the same ID's, but they each get their own array in the red list.
					t.rv <- redValue[[i]][[r]]
					invisible(t.red[, # within the j of this data.table, build each element of the output array list
						j={
							dim.names <- lapply(.SD[,eval(s2c(arr.dim))], unique)
							t.values <- unlist(.SD[,eval(s2c(t.rv))],use.names=FALSE)
							red.list[[ctr]][[.GRP]] <<- array(t.values, dim=sapply(dim.names, length), dimnames=dim.names)
							if(is.null(NULL)){}
						},
						by=c(outScope)
					])
					
					# testing
					# invisible(test.sd[, # within the j of this data.table, build each element of the output array list
# 						j={
# 							dim.names <- lapply(.SD[,eval(s2c(arr.dim))], unique)
# 							red.list[[r]][[1]] <<- array(.SD[,eval(s2c(t.rv))], dim=sapply(dim.names, length), dimnames=dim.names)
# 							dim.names <- lapply(test.sd[,eval(s2c(arr.dim))], unique)
# 							array(unlist(test.sd[,eval(s2c(t.rv))],use.names=F), dim=sapply(dim.names, length), dimnames=dim.names)
# 							if(is.null(NULL)){}
# 						}
# 					])
					# end testing
						
				} # end looping through the r red values in redID i	
				
			} # end loop through redID
		
			
		
		} # end redID if


		
		if(!is.null(outScope)){
			array.key <- data.table(matrix(unlist(array.key), ncol=length(outScope)+1, byrow=TRUE)) # build the key to the output array
			setnames(array.key, names(array.key), c(outScope, "num")) # key the key
		}
		
		if(!is.null(redID)){
			return(list(array.list=array.list, array.key=array.key, red.list=red.list))
		}else{
			return(list(array.list=array.list, array.key=array.key)) # return the output array and its key (stops function)
		}
		
		
	}else{ # end first half of if(arrayOut), begin alternative
		# ==================================================================
		# = If not an array, return formatted data table with "red" values =
		# ================================================================== 
		if(!is.null(redID)){ # if there are redID's that should be added back in ...
			for(i in 1:length(redID)){ # for each redundant id/ value ...
				setkeyv(expD, redID[[i]])
				setkeyv(redFill[[i]], redID[[i]])
				# expD <- redFill[[i]][expD,][,eval(s2c(c(names(expD),redValue[[i]])))] # 1st part merges, 2nd reorders columns
				expD <- setcolorder(redFill[[i]][expD,], c(names(expD), redValue[[i]])) # this should be faster
			} # end loop through redID
		} # end redID if
	
		setkeyv(expD, IDs)
		setnames(expD, "value", keyValue)
		return(expD) # return data.table 
	
	} # end if(){}else{} for arrayOut
		
} # end expand.data()



# ============
# = Examples =
# ============
# testT2 <- as.data.table(melt(trawl2, id.vars=c("s.reg","year","stratum","K","correctSpp","taxLvl","phylum","spp","common"), measure.vars=c("wtcpue","stemp","btemp","lat","lon","depth")))
#
# testT.sub <- testT2[variable=="wtcpue" & s.reg=="gmex" & (!is.na(taxLvl)&taxLvl=="Species") & correctSpp==TRUE]
# setkey(testT.sub, stratum, K, year, spp)
#
# testT.sub <- testT2[variable=="wtcpue" & (!is.na(taxLvl)&taxLvl=="Species") & correctSpp==TRUE]
# setkey(testT.sub, stratum, K, year, spp)
#
# msom.array <- expand.data( # takes ~1.5 seconds (just gmex, wtcpue, Species, correctSpp), original code took 63.6 seconds
# 	comD = testT.sub,
# 	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue when combined with fScope
# 	fillID=c("spp","K"),
# 	fillValue=c(0,NA), # values to fill with, for a fillID
# 	Rule=c("value", "scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# 	keyID=key(comD), # column names whose values uniquely identify rows
# 	keyValue="value", # the column whose values would fill the array
# 	gScope="s.reg", # global scope
# 	fScope=list("s.reg", c("s.reg","year")), #
# 	vScope=list(c("s.reg","stratum","year","K"), NULL),
# 	redID=list(c("spp")), redValue=list(c("correctSpp","taxLvl","phylum","common")),
# 	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
# )
#
#
# array.filled <- expand.data( # this test the aggregate functionality, data.table output, non-NA fill, 1 fillID
# 	comD = testT.sub,
# 	arr.dim = c("stratum", "year", "spp"), # should uniquely define the keyValue when combined with fScope
# 	fillID=c("spp"),
# 	fillValue=c(0), # values to fill with, for a fillID
# 	Rule=c("value"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# 	keyID=NULL, #c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
# 	keyValue="value", # the column whose values would fill the array
# 	gScope="s.reg", # global scope
# 	fScope=list("s.reg"), #
# 	vScope=list(c("s.reg","stratum","year")),
# 	redID=list(c("spp")), redValue=list(c("correctSpp","taxLvl","phylum","common")),
# 	arrayOut=FALSE, aggFun=meanna, maxOut=Inf
# )
#
#
# array.filled[sample(1:nrow(array.filled), 100),]