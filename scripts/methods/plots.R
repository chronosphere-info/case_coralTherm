pal <- c(z="#cc2f04", az="#013a94")

# Uniform function for time series
Tp <- function(...){
	tsplot(stages, boxes="sys", boxes.col="systemCol", 
		shading="series", xlim=52:95, ...)
}


# Function to plot ranges as polygons
#' @param latRanges output of LatRanges()
Polyplot <- function(x,...){
	polygon(
		x=c(
			stages$mid[c(as.numeric(rownames(latRanges)), 
			rev(as.numeric(rownames(latRanges))))]), 
		y=c(latRanges[,1], rev(latRanges[,2])),
		...
	)
}
#' Function to plot ranges as polygons
#' functino to plot the niche of an individual taxa in a time slice
#' @param genusNiche OUtput of GenusNiche()
LinesNiche <- function(genusNiche, ...){
	for(i in 1:nrow(genusNiche)){
		lines(stages[,"mid"], genusNiche[i,],...)
	}
}

#Lame plot of the latitudinal ranges with an sst map
#' @param datZ The Z coral subset
#' @param datAZ The AZ corral subset
#' @param i time interval identifier
#' @param zcol Hexadecimal color for z corals
#' @param azcol Hexadecimal color for az corals
#' @param alpha Alpha of the rectangles 
PlotMapWithRanges <- function(datZ, datAZ, i, zcol, azcol, alpha=20, legend=TRUE){
	# select bin-specific datasets
	Z <- Coordinates(datZ[datZ$stg==i,])
	AZ <- Coordinates(datAZ[datAZ$stg==i,])
	
	# which reconstruction should be plotted
	date <- unique(datZ[datZ$stg==i,"mapage"])
	
	oldpar <- par(no.readonly = TRUE)
	
	par(mar=c(8,4,2,8))
	plot(0,0, 
		xlim=c(-180,180), 
		ylim=c(-90,90), 
		xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)

	# 
	axis(1, at=seq(-180, 180, 60), label=seq(-180, 180, 60))
	axis(2, at=seq(-90, 90, 45), label=seq(-90, 90, 45))
	rect(xleft=-180, xright=180,ybottom=-90, ytop=90, col=NA, border="black")

	plot(tos[as.character(date)], col=gradinv(255), legend=FALSE, add=TRUE)
	
	if (legend){
	plot(tos[as.character(date)], col=gradinv(255), horizontal=TRUE,legend.only=TRUE, 
			 add=TRUE,
			 legend.args=list(
			 		text=expression("Sea Surface Temperature ("*~degree*C*")"), 
			 		side=1, font=2, line=-2.5, cex=0.8))
	}
	
	# plot the latitudinal ranges
	azRange <- range(AZ$plat)
	rect(xleft=-180, xright=180, ytop=azRange[2], ybottom=azRange[1], border=azcol,
			 col=paste0(azcol, alpha))
	
	zRange <- range(Z$plat)
	rect(xleft=-180, xright=180, ytop=zRange[2], ybottom=zRange[1], border=zcol,
			 col=paste0(zcol, alpha))
	
	# plot the points
	points(AZ, col=azcol, bg="white", pch=21, cex=1.2, lwd=1.2)
	points(Z, col=zcol, bg="white", pch=21, cex=1.2, lwd=1.2)
	
	par(xpd=TRUE)
	
	lines(x=c(180, 200), y=rep(zRange[2], 2), lty="dashed", col=zcol)
	lines(x=c(180, 200), y=rep(zRange[1], 2), lty="dashed", col=zcol)
	text(x=200, y=mean(zRange[c(1,2)]), label="Zoothanthellate", 
		adj=0.5, srt=-90, font=2, cex=1, col=zcol)
	
	zL <- floor((diff(zRange)-50)/2)
	
	arrows(x0=200, y0=zRange[1], y1 = zRange[1]+zL, 
				 length = 0.1, angle = 30, code = 1, col = zcol, lwd=2)
	arrows(x0=200, y0=zRange[2], y1 = zRange[2]-zL, 
				 length = 0.1, angle = 30, code = 1, col = zcol, lwd=2)
	
	
	azL <- floor((diff(azRange)-50)/2)
	lines(x=c(180, 220), y=rep(azRange[2], 2), lty="dashed", col=azcol)
	lines(x=c(180, 220), y=rep(azRange[1], 2), lty="dashed", col=azcol)
	text(x=220, y=mean(azRange[c(1,2)]), label="Azoothanthellate", 
		adj=0.5, srt=-90, font=2, cex=1, col=azcol)
	arrows(x0=220, y0=azRange[1], y1 = azRange[1]+azL, 
				 length = 0.1, angle = 30, code = 1, col = azcol, lwd=2)
	arrows(x0=220, y0=azRange[2], y1 = azRange[2]-azL, 
				 length = 0.1, angle = 30, code = 1, col = azcol, lwd=2)
	
	#restore old plotting parameters
	par(oldpar)
}

CompareSST <- function(zSST, azSST, col, wilcox=TRUE){
	plot(c(1,2), y=c(0,40), xlim=c(0.5,2.5), ylim=c(-5,40), type="n", axes=FALSE,
			 ylab=expression("Sea Surface Temperature ("*~degree*C*")"), xlab="",
			 xaxs="i", yaxs="i")
	
	points(x=runif(length(unique(zCorals$SST)), 0.6, 1.4), y=unique(zCorals$SST),
				 pch=16, cex=0.8, col=paste0(col["z"], 10))
	points(x=runif(length(unique(azCorals$SST)), 1.6, 2.4), y=unique(azCorals$SST),
				 pch=16, cex=0.8, col=paste0(col["az"], 10))
	
	# median difference (Figure 2.)
	boxplot(
		list("z-corals"=zSST, "az-corals"=azSST),
		names=c("Z-corals", "AZ-corals"),
		lwd=1.2,lty=1,
		border=col[c("z", "az")], col=NA, pch=16, add=TRUE)
	
	# depending on statistic value
	if(wilcox){
		WT <- wilcox.test(zSST, azSST)

		if(WT$p.value<1e-04){
			text(0.6, 38, 
				 label=bquote(italic(W)== .(WT$statistic) ~ ", " ~ italic(p) < 10^-4), 
				 adj=0, cex=0.8)
		}else{
		text(0.6, 38, 
				 label=bquote(italic(W)== .(WT$statistic) ~ ", " ~ italic(p) == 
				 	.(formatC(WT$p.value, format = "e", digits = 2))), 
				 adj=0, cex=0.8)
		}
	}
}

#' @param name The name of the plot
#' @param path Where the plot should be put.
savepdf <- function(name, path="export/", ...){
	pdf(file.path(path, name), ...)
}
