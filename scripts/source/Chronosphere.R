#library(divDyn)
#library(chronosphere)
#library(shades)

# # access cnidaria server --------------------------------------------------
# 

# 
# 
# # download data -----------------------------------------------------------
# 
# ind <- datasets()
# 
# datN <- fetch(dat="pbdb")
# datN <- datN[dat$order =="Scleractinia",] #only scleractinia
# tos <- fetch(dat="had-stage", var="tos")
# pr <- fetch(dat="paleomap", var="paleoatlas", res=0.5)
# data(stages)


# assign stg --------------------------------------------------------------

#stg <- rep(NA, nrow(datN))
#
#for (i in c(1:nrow(stages))){
#  n <- which(datN$max_ma < stages$bottom[i] & datN$min_ma > stages$top[i])
#  stg[n] <- stages$stg[i]
#}
#
#datN <- cbind(datN, stg)
#datN <- datN[!is.na(datN$stg),]
#
#sort(unique(datN$stg))

# reconstruct coordinates (Paleomap model) --------------------------------

#collSST <- datN[datN$stg%in%unique(datN$stg),c("collection_no", "genus", "lng", "lat", "stg", "paleolat" ,"paleolng")]
#collSST <- unique(collSST)

#tosord <- matchtime(tos, stages$mid)
#collSST$mapage <- names(tosord)[collSST$stg]

# palN_coo <- reconstruct(collSST[, c("lng", "lat")], age=collSST[, "mapage"], enumerate=FALSE, 
#                         verbose=FALSE, model = "PALEOMAP")

#colnames(palN_coo) <- c("plng", "plat")
#collSST <- cbind(collSST, palN_coo)

#completeFun <- function(data, desiredCols) {
#  completeVec <- complete.cases(data[, desiredCols])
#  return(data[completeVec, ])
#}
#
#collSST <- completeFun(collSST, c("lat","lng","mapage","stg","plng", "plat"))
#
#
## extract temperature values for collections ------------------------------
#
#collSST$SST <- extract(tosord, collSST, by="mapage")
#collSST <- completeFun(collSST, c("SST"))


# median temperature per stage --------------------------------------------
nic <- as.data.frame(matrix(nrow = length(stages$stg), ncol = 5))
colnames(nic) <- c("stg", "median", "max", "min", "mad")
nic$stg <- stages$stg

# 
allSlices <- sort(unique(collections$stg))

for (i in 1:length(allSlices)){
  
  nic$median[nic$stg==allSlices[i]] <- median(collSST$SST[collSST$stg == allSlices[i]])
  nic$max[nic$stg==allSlices[i]] <- max(collSST$SST[collSST$stg == allSlices[i]])
  nic$min[nic$stg==allSlices[i]] <- min(collSST$SST[collSST$stg == allSlices[i]])
  nic$mad[nic$stg==allSlices[i]] <- mad(collSST$SST[collSST$stg == allSlices[i]])

}

nic$max[nic$max == "-Inf"] <- NA
nic$min[nic$min == "Inf"] <- NA

# latitude per stage ------------------------------------------------------

lat <- as.data.frame(matrix(nrow = length(stages$stg), ncol = 7))
colnames(lat) <- c("stg", "latN", "maxN", "minN", "latS", "maxS", "minS")
lat$stg <- stages$stg

for (i in c(lat$stg)){
  
  lat$latN[i] <- median(collSST$plat[collSST$stg == lat$stg[i] & collSST$plat > 0])
  lat$maxN[i] <- max(collSST$plat[collSST$stg == lat$stg[i] & collSST$plat > 0])
  lat$minN[i] <- min(collSST$plat[collSST$stg == lat$stg[i] & collSST$plat > 0])
  
  lat$latS[i] <- median(collSST$plat[collSST$stg == lat$stg[i] & collSST$plat < 0])
  lat$maxS[i] <- max(collSST$plat[collSST$stg == lat$stg[i] & collSST$plat < 0])
  lat$minS[i] <- min(collSST$plat[collSST$stg == lat$stg[i] & collSST$plat < 0])
  
}

lat[lat == "Inf"] <- NA
lat[lat == "-Inf"] <- NA


# plots -----------------------------------------------------------

# * latitude through time -------------------------------------------------

windows(h=6,w=10)
tsplot(stages, boxes=c("sys"), shading="sys", boxes.col="systemCol", ylim=c(-80,80),
       ylab="latitude", xlim=52:95)

polygon(c(stages$mid, stages$mid[95:1]), c(lat$minN, lat$maxN[95:1]),
        col = adjustcolor("slategray2", alpha.f = 0.5), border = NA)
lines(stages$mid, lat$latN, col= "steelblue4", lwd= 3)

polygon(c(stages$mid, stages$mid[95:1]), c(lat$minS, lat$maxS[95:1]),
        col = adjustcolor("slategray2", alpha.f = 0.5), border = NA)
lines(stages$mid, lat$latS, col= "steelblue4", lwd= 3)

abline(h=0)
abline(v=0)

legend(105,80, legend = c("median latitude", "range"), 
       fill =c("steelblue4", "slategray2"),
       border =c("steelblue4", "slategray2"), bty="n",
       cex=1)

# * temperature through time ----------------------------------------------

windows(h=6,w=10)
tsplot(stages, boxes=c("sys"), shading="sys", boxes.col="systemCol", ylim=c(0,62),
       ylab="SST [°C]")

polygon(c(stages$mid, stages$mid[95:1]), c(nic$median - nic$mad, nic$median[95:1] + nic$mad[95:1]),
        col = adjustcolor("slategray2", alpha.f = 0.5), border = NA)
lines(stages$mid, nic$median, col= "steelblue4", lwd= 3)

abline(h=0)
abline(v=0)

legend(105,60, legend = c("median SST", "MAD"), 
       fill =c("steelblue4", "slategray2"),
       border =c("steelblue4", "slategray2"), bty="n",
       cex=1)

## * worldmap  -------------------------------------------------------------
#
#map230 <- reconstruct("plates", age=230, model=mod)
#plot(map230, col="gray")
#coll_pl <- collSST[collSST$stg==56,]
#points(coll_pl$plng, coll_pl$plat, pch=20, col="darkgoldenrod2")
#
## * SST map  --------------------------------------------------------------
#
#mapplot(tos["230",])
#points(coll_pl$plng, coll_pl$plat, pch=20, col="darkgoldenrod2")
#
## temperature niches of individual genera ---------------------------------



###########################################################################
# comparing cretaceous in two PBDB versions -------------------------------

#old version

load(url(
  "https://github.com/divDyn/ddPhanero/raw/master/data/PaleoDB/2019-05-31_paleoDB.RData"
))



source("https://github.com/divDyn/assets/raw/master/examples/misc/preparation.R")
dat <- prepare(dat)
dat <- dat[dat$order =="Scleractinia",]


# assign stg (old) --------------------------------------------------------

stg <- rep(NA, nrow(dat))

for (i in c(1:nrow(stages))){
  n <- which(dat$max_ma < stages$bottom[i] & dat$min_ma > stages$top[i])
  stg[n] <- stages$stg[i]
}

dat <- cbind(dat, stg)
dat <- dat[!is.na(dat$stg),]

sort(unique(dat$stg))


# reconstruct coordinates (old) -------------------------------------------

dat_coll <- dat[dat$stg%in%unique(dat$stg),c("collection_no", "genus", "lng", "lat", "stg", "paleolat" ,"paleolng")]
dat_coll <- unique(dat_coll)

dat_coll$mapage <- names(tosord)[dat_coll$stg]

pal_coo <- reconstruct(dat_coll[, c("lng", "lat")], age=dat_coll[, "mapage"], enumerate=FALSE, 
                        verbose=FALSE, model = "PALEOMAP")

colnames(pal_coo) <- c("plng", "plat")
dat_coll <- cbind(dat_coll, pal_coo)

dat_coll <- completeFun(dat_coll, c("lat","lng","paleolat","paleolng","mapage","stg","plng", "plat"))


# extract temperature values from tos (old) -------------------------------

dat_coll$SST <- extract(tosord, dat_coll, by="mapage")
dat_coll <- completeFun(dat_coll, c("SST"))


# median temperature per stage (old) --------------------------------------

nic2 <- as.data.frame(matrix(nrow = length(stages$stg), ncol = 5))
colnames(nic2) <- c("stg", "median", "max", "min", "mad")
nic2$stg <- stages$stg

for (i in c(nic2$stg)){
  
  nic2$median[i] <- median(dat_coll$SST[dat_coll$stg == nic2$stg[i]])
  nic2$max[i] <- max(dat_coll$SST[dat_coll$stg == nic2$stg[i]])
  nic2$min[i] <- min(dat_coll$SST[dat_coll$stg == nic2$stg[i]])
  nic2$mad[i] <- mad(dat_coll$SST[dat_coll$stg == nic2$stg[i]])
  
}

nic2$max[nic2$max == "-Inf"] <- NA
nic2$min[nic2$min == "Inf"] <- NA


# latitudes per stage (old) -----------------------------------------------

lat2 <- as.data.frame(matrix(nrow = length(stages$stg), ncol = 7))
colnames(lat2) <- c("stg", "latN", "maxN", "minN", "latS", "maxS", "minS")
lat2$stg <- stages$stg

for (i in c(lat2$stg)){
  
  lat2$latN[i] <- median(dat_coll$plat[dat_coll$stg == lat2$stg[i] & dat_coll$plat > 0])
  lat2$maxN[i] <- max(dat_coll$plat[dat_coll$stg == lat2$stg[i] & dat_coll$plat > 0])
  lat2$minN[i] <- min(dat_coll$plat[dat_coll$stg == lat2$stg[i] & dat_coll$plat > 0])
  
  lat2$latS[i] <- median(dat_coll$plat[dat_coll$stg == lat2$stg[i] & dat_coll$plat < 0])
  lat2$maxS[i] <- max(dat_coll$plat[dat_coll$stg == lat2$stg[i] & dat_coll$plat < 0])
  lat2$minS[i] <- min(dat_coll$plat[dat_coll$stg == lat2$stg[i] & dat_coll$plat < 0])
  
}

lat2[lat2 == "Inf"] <- NA
lat2[lat2 == "-Inf"] <- NA

# plots -------------------------------------------------------------------

# * latitude through time -------------------------------------------------

windows(h=6,w=10)
tsplot(stages, boxes=c("sys"), shading="sys", boxes.col="systemCol", ylim=c(-80,80),
       ylab="latitude")

polygon(c(stages$mid, stages$mid[95:1]), c(lat2$minN, lat2$maxN[95:1]),
        col = adjustcolor("slategray2", alpha.f = 0.5), border = NA)
lines(stages$mid, lat2$latN, col= "steelblue4", lwd= 3)

polygon(c(stages$mid, stages$mid[95:1]), c(lat2$minS, lat2$maxS[95:1]),
        col = adjustcolor("slategray2", alpha.f = 0.5), border = NA)
lines(stages$mid, lat2$latS, col= "steelblue4", lwd= 3)

abline(h=0)
abline(v=0)

legend(105,80, legend = c("median latitude", "range"), 
       fill =c("steelblue4", "slategray2"),
       border =c("steelblue4", "slategray2"), bty="n",
       cex=1)


# * temperature through time ----------------------------------------------

windows(h=6,w=10)
tsplot(stages, boxes=c("sys"), shading="sys", boxes.col="systemCol", ylim=c(0,62),
       ylab="SST [°C]")

polygon(c(stages$mid, stages$mid[95:1]), c(nic2$median - nic2$mad, nic2$median[95:1] + nic2$mad[95:1]),
        col = adjustcolor("slategray2", alpha.f = 0.5), border = NA)
lines(stages$mid, nic2$median, col= "steelblue4", lwd= 3)

abline(h=0)
abline(v=0)

legend(105,60, legend = c("median SST", "MAD"), 
       fill =c("steelblue4", "slategray2"),
       border =c("steelblue4", "slategray2"), bty="n",
       cex=1)


# * worldmap  -------------------------------------------------------------

mapplot(pr["230",], rgb=TRUE)
coll_pl <- dat_coll[dat_coll$stg==56,]
points(coll_pl$plng, coll_pl$plat, pch=20, col="darkgoldenrod2")

# * SST map  --------------------------------------------------------------

mapplot(tos["230",], rgb=TRUE)
points(coll_pl$plng, coll_pl$plat, pch=20, col="darkgoldenrod2")




