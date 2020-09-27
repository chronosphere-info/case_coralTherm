# maximum and minimum latitudinal range over time
#' @dat The databas subset
#' @bin column name temporal containers
#' @lat Column name of the assessed variables (eg. latitude)
LatRanges <- function(dat, bin="stg", lat="plat"){
	# make sure there are no NAs
	dat <-dat[!is.na(dat[, lat]), ]

	# vector of all valid bin entires
	allSTG <- sort(unique(dat[,bin]))

	# output container
	ranges <- matrix(NA, ncol=2, nrow=length(allSTG))
	rownames(ranges) <- allSTG

	# iterate for every sampled bin
	for(i in 1:length(allSTG)){
		# latitude values in the bin
		latInBin<- dat[dat[, bin]==allSTG[i], lat]
		# minimum stored
		ranges[i,] <- range(latInBin, na.rm=TRUE)
	}

	return(ranges)
}

# Function to calculate the median niche of a taxon in a time slice
#' @param dat The databas subset
#' @param bin column name of the bins
#' @param tax column name of the taxa
#' @param var Column name of the focal variable 
GenusNiche <- function(dat, bin="stg", tax="genus.proper", var="SST"){
	allTax <- unique(dat[,tax])

	# container for every entry, stage by taxon
	allMed <- matrix(NA, ncol=nrow(stages), nrow=length(allTax))
	colnames(allMed) <- 1:nrow(stages)
	rownames(allMed) <- allTax

	# iterate for all time slices
	for(i in 1:length(allTax)){
		# taxon-specific subset
		taxDat <- dat[dat[, tax]==allTax[i],]

		# taxon's niche in every time slice
		taxMed <- tapply(INDEX=taxDat[,bin], X=taxDat[,var], median, na.rm=T)
		# stored in container
		allMed[i, as.numeric(names(taxMed))] <- taxMed
	}

	return(allMed)
}


# assume a single value for one genus, niche for every entry.
#' @param dat The databas subset
#' @param bin column name of the bins
#' @param tax column name of the taxa
#' @param var Column name of the focal variable 
GenusNicheOverTime <- function(dat, tax="genus.proper", var="SST", bin="stg"){
	# median of a sinle taxon's variable, i.e. SST
	genMed <- tapply(INDEX=dat[,tax], X=dat[, var], median, na.rm=TRUE)

	# calculate the FADLAD matrix from divDyn
	fl <- divDyn::fadlad(dat, tax=tax, bin=bin)

	# add the var values and return
	fl <- cbind(fl, var=genMed[rownames(fl)])
	return(fl)
}



# calculate the mean over time
#' @param genusNicheOverTime the output of GenusNicheOverTime
MeanOverTime <- function(genusNicheOverTime){
	meanVal<- rep(NA, max(genusNicheOverTime$LAD))
	names(meanVal) <- 1:max(genusNicheOverTime$LAD)

	for(i in 1:length(meanVal)){
		bWithin <- genusNicheOverTime$FAD<=i & genusNicheOverTime$LAD>=i
		if(sum(bWithin)>0) meanVal[i] <- mean(genusNicheOverTime$var[bWithin])
	}
	return(meanVal)
}


# Repeat analyses on a different subset of the PBDB
#' @param pbdb The pbdb data.
Repeat <-function(pbdb){
	corals <- pbdb[pbdb$order =="Scleractinia",] #only scleractinia

	# filter that too reef corals
	traits <- fetch(dat="som", var="kiessling-coralgenera",
		datadir=file.path(workDir, "data/chronosphere"))

	# assign the symbiotic status
	corals <- merge(corals, 
		unique(traits[, c("genus_detail", "genus.proper", "ECOLOGY")]), 
		by.x="genus", by.y="genus_detail", all.x=TRUE)

	# 2. Assign stage to PBDB entries
	corals <- AssignStages(corals)

	# clean occurrence data (NUSS could you do this?)
	corals <- CleanPBDB(corals)

	# number of occurrences in a bin
	table(corals$stg)

	###############################################################################
	# SPATIAL DATA AND SST RECONSTRUCTIONS
	# 3. Reconstruct coordinates
	# extrapolated, masked - hence the missing rasters (PaleoCoastlines are matched with stages)
	tos <- fetch(dat="had-stage", var="tos-extra", res=1, datadir=chronodat)

	# download a model
	mod <- fetch(dat="paleomap",var="model", datadir=chronodat)

	# separate the collection data
	collections <- corals[,c("collection_no", "lng", "lat", "stg", "paleolat" ,"paleolng")]
	collections <- unique(collections)

	# omit cases where something is missing
	collections <- collections[!is.na(collections$stg) & !is.na(collections$lng) & !is.na(collections$lat), ]

	# II. The sst reconstructions
	# Order the TOS series
	tosord <- matchtime(tos, stages$mid)

	# assign a reconstruction to every collection
	collections$mapage <- names(tosord)[collections$stg]

	# Offline reconstruction - might take a minute or two
	reconstructed <- reconstruct(collections[, c("lng", "lat")], 
		age=collections[, "mapage"], enumerate=FALSE, model = mod)

	# bind the new coordinates to the previous
	colnames(reconstructed) <- c("plng", "plat")
	collections <- cbind(collections, reconstructed)

	# extract the SST values
	collections$SST <- chronosphere::extract(tosord, collections, by="mapage")

	# omit everything with no SST data
	collSST <- collections[!is.na(collections$SST), ]

	# remerge with occurrences
	corals <- merge(corals, collSST[, c("collection_no", "plat", "plng", "SST")], by="collection_no")

	return(corals)
}
