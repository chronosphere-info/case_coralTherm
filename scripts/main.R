# Chronosphere-case study
# Ádám T. Kocsis, Paulina Nätscher, Nussaïbah B. Raja
# 2020, Erlangen, Germany
# CC-BY 3.0

#### NUSS, Before I forget:
# Custom Functions always start with an Uppercase Letter.
# Print out a particular function before invoking it. They are not long.

# Adam's directory
# working directory
# workDir <-file.path(Sys.getenv("WorkSpace"), "2019-07-08_chronosphere", "chrono_case")
# setwd(workDir)

# function libraries
library(chronosphere)
library(divDyn)

# functions
source("scripts/methods/utils.R")
funky() # reads in all the function files

# 0.  The necessary data
allDat <- datasets()

# data of the chronosphere
chronodat <- file.path("data", "chronosphere")


###############################################################################
# I. The PBDB.
# The paleobiology database
pbdb <- fetch(dat="pbdb",datadir=chronodat)
corals <- pbdb[pbdb$order =="Scleractinia",] #only scleractinia

# filter that too reef corals
traits <- fetch(dat="som", var="kiessling-coralgenera",datadir=chronodat)

# assign the symbiotic status
corals <- merge(
	corals, 
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


#& # Plots
plot(0,0, xlim=c(-180,180), ylim=c(-90,90), type="n", xaxs="i", yaxs="i",
     xlab="", ylab="")
mapplot(tos["130"], add=TRUE)
points(collSST[collSST$mapage==130, c("plng", "plat")],pch=16, col="black")

# remerge with occurrences
corals <- merge(corals, collSST[, c("collection_no", "plat", "plng", "SST", "mapage")], by="collection_no")

 ##########################################################
# # Analysis:
# # latRange <- LatRanges(corals2, bin="stg", lat="plat")
# zRange <-LatRanges(zCorals)
# 
# # Latitudinal range
# tp(ylim=c(-90,90))
# polyplot(latRange, col="blue")
# polyplot(zRange, col="yellow")
# 

# compare Z with AZ corals
zCorals <- corals[corals$ECOLOGY=="z", ]
azCorals <- corals[corals$ECOLOGY=="az" | corals$ECOLOGY=="ap", ]

##########################################################
# SIMILAR vs different (Should be Fig. 1) - quick, ugly drafts.
# I'll leave the aesthetics to you :), maybe with just a breadth indicating arrow this will be better!
savepdf("Fig_similarMap.pdf", width=12, height=7.5)
	PlotMapWithRanges(zCorals, azCorals, i=75,zcol=pal["z"], azcol=pal["az"], alpha=60)
	dev.off()
  
savepdf("Fig_differentMap.pdf", width=12, height=7.5)
	# This plot is nice.
	PlotMapWithRanges(zCorals, azCorals, i=93, zcol=pal["z"], azcol=pal["az"], alpha=60)
dev.off()

# FOR SOM:
zMed <- tapply(INDEX=zCorals$stg, X=zCorals$SST, median, na.rm=T)
azMed <- tapply(INDEX=azCorals$stg, X=azCorals$SST, median, na.rm=T) 
# zMAD <- tapply(INDEX=zCorals$stg, X=zCorals$SST, mad, na.rm=T)
# azMAD <- tapply(INDEX=azCorals$stg, X=azCorals$SST, mad, na.rm=T)

Tp(ylim=c(0,40))
lines(stages$mid[as.numeric(names(zMed))],zMed, col=pal["z"])
lines(stages$mid[as.numeric(names(azMed))],azMed, col=pal["az"])

##########################################################
#checking for outliers
# out <- boxplot(
#   list("z-corals"=zCorals$SST, "az-corals"=azCorals$SST),
#   names=c("Z-corals", "AZ-corals"),
#   col=c("#cc2f04", "#013a94"),
#   ylab=expression("Sea Surface Temperature ("*~degree*C*")") )
# 
# 
# df.out <- corals[corals$SST %in% out$out & corals$ECOLOGY == "z",]
# 
# write.csv(df.out[order(df.out$SST)
#             , c("family", "genus", "genus.proper", "identified_name", "stg", "lat", "lng", "plat", "plng", "SST", "ECOLOGY")], 
#             "z_outliers.csv", row.names = FALSE)

# significant median difference across the meso-cenozoic
WT <- wilcox.test(zCorals$SST, azCorals$SST)

savepdf("Fig_boxplot.pdf", width=7, height=6)

CompareSST(zSST=zCorals$SST, azSST= azCorals$SST,
           col = pal, wilcox=WT)

dev.off()


# much greater T variation, even with drastically lower sample size

# # the total range
# totalRangeZ <- zRange[,2]-zRange[,1]
# 
# temper <- LatRanges(zCorals, bin="stg", lat="SST")
# temperRange <- temper[,2]-temper[,1]
# 
# 
# 
# lines(stages$mid[as.numeric(names(totalRangeZ))],totalRangeZ/mean(totalRangeZ), col="blue")
# lines(stages$mid[as.numeric(names(temperRange))],temperRange/mean(temperRange), col="red")


# # thermal niche
# tp(ylim=c(0,40))
# polyplot(temper, col="yellow")
# lines(stages$mid[as.numeric(names(zMed))],zMed, col="blue")


# Genus median niche
allMedZ <- GenusNiche(zCorals)
allMedAZ <- GenusNiche(azCorals)


# time-specific occupied niche - heavilly affected by sampling
# Plot for the SOM
Tp(ylim=c(0,40),ylab=c("Sea Surface Temperature (C)"))
LinesNiche(allMedZ, col=paste0(pal["z"], 55))
LinesNiche(allMedAZ, col=paste0(pal["az"], 55))

# ASSUMPTION: genus did not change its thermal niche
# Calculate the median niche of the genus with its FAD and LAD
flZ <- GenusNicheOverTime(zCorals)
flAZ <- GenusNicheOverTime(azCorals)

savepdf("Fig_genusNiche.pdf", width=7, height=6)
# plot them as lines (Figure 3.)
Tp(ylim=c(0,40), ylab=expression("Sea Surface Temperature ("*~degree*C*")"))
segments(x0=stages$mid[flZ$FAD], x1=stages$mid[flZ$LAD], y0=flZ$var, y1=flZ$var, col=paste0(pal["z"], 22) , lwd=2)
segments(x0=stages$mid[flAZ$FAD], x1=stages$mid[flAZ$LAD], y0=flAZ$var, y1=flAZ$var, col=paste0(pal["az"], 22)  , lwd=2)

# DISTINCTION IS MUCH CLEARER THIS WAY.

# change is much easier to see, calculate mean over time
# DEPART from each other from the mid-Cretaceous
# SAMPLING CAUSES THE decrease from the Triassic?
meanZ<- MeanOverTime(flZ)
meanAZ <- MeanOverTime(flAZ)

lines(stages[names(meanZ), "mid"], meanZ, col=pal["z"], lwd=5)
lines(stages[names(meanAZ), "mid"], meanAZ, col=pal["az"], lwd=5)

legend(
	"bottomleft", legend=c("Z-corals", "AZ-corals"), inset=c(0.05, 0.1), 
	lwd=4, col=c(pal[c("z", "az")]), bg="white")
dev.off()

# ############################################################################################
# # repeat the whole process on an older version of the PBDB
# pbdbDat <- datasets("pbdb")$ver
# 
# # version from last november
# oldPBDB <- fetch("pbdb", ver="20191122", datadir=chronodat)
# oldCorals <- Repeat(oldPBDB)
# 
# table(oldCorals$stg)
# table(corals$stg)-table(oldCorals$stg)
# # bin 76
# corals76 <- corals[corals$stg==76,]
# oldCorals76 <- oldCorals[oldCorals$stg==76,]
# 
# boxplot(corals76$SST, oldCorals76$SST)




