CleanPBDB <- function(dat){
	# omit missing taxa
	datNoNAGEN <- dat[!is.na(dat$genus.proper), ]

	#remove ?, cf., n. gen., aff. "" in genus name 
	spName <- gsub('^.* ([[:alnum:]]+\\.?\\.?)$', '\\1', datNoNAGEN$identified_name)
	genName <- gsub('(^.*) [[:alnum:]]+\\.?$', '\\1', datNoNAGEN$identified_name)
	
	#uncertain gn name. 
	n <- grep('aff\\.|\\?|cf\\.|n\\. gen\\.|\\"', genName)
	if(length(n) > 0) datNoNAGEN <- datNoNAGEN[-n,]
	
	# omit missing stages
	datNoNASTG <- datNoNAGEN[!is.na(datNoNAGEN$stg), ]
	
	# omit missing geographic
	datNoCoords <- datNoNASTG[!is.na(datNoNASTG$lat)&!is.na(datNoNASTG$lng), ]

	# omit invalid taxon
	datOK <- datNoCoords[!datNoCoords$genus.proper=="", ]

	# omit Holocene
	datPast <- datOK[datOK$stg!=95, ]

	# omit permian
	datFinal <- datPast[datPast$stg>52, ]

	# return
	return(datFinal)	
}

#' @param dat A dataset downloaded from the Paleobiology Database 
# More about this on the divDyn github repository.
AssignStages <- function(dat){

	# load necessary data from divDyn
	data(stages, package="divDyn")
	data(keys, package="divDyn")
	
	
	# the 'stg' entries (lookup)
	stgMin <- divDyn::categorize(dat[ ,"early_interval"], keys$stgInt)
	stgMax <- divDyn::categorize(dat[ ,"late_interval"], keys$stgInt)

	# convert to numeric
	  stgMin <- as.numeric(stgMin)
	  stgMax <- as.numeric(stgMax)

	# convert to numeric
	stgMin <- as.numeric(stgMin)
	stgMax <- as.numeric(stgMax)

	# empty container
	dat$stg <- rep(NA, nrow(dat))

	# select entries, where
	stgCondition <- c(
	# the early and late interval fields indicate the same stg
	  which(stgMax==stgMin),
	# or the late_intervar field is empty
	  which(stgMax==-1))

	# in these entries, use the stg indicated by the early_interval
	  dat$stg[stgCondition] <- stgMin[stgCondition] 

	return(dat)
}

Coordinates <- function(dat, long="plng", lat="plat"){
	unique(dat[,c(long, lat)])
}
