# Function to extrapolate to tho the whole globe. This is necessary as the input topographies are wrong.
# TPS model built on the coarse resolution model
# used with the fine resolution one.

Extrapolate <- function(coarse, fine){
	require(fields)
	
	# resulting raster
	res <- fine

	message("Extrapolating SST values. This will take a long time.")
	# iterate for every time slice
	for (i in 1:length(coarse)){
		# select one raster
		focalCoarse <- coarse[i]
		focalFine <- fine[i]

		# extract the coordinates
		coordinates <- data.frame(xyFromCell(focalCoarse, 1:ncell(focalCoarse)))

		# and values
		vals <- getValues(focalCoarse)

		# remove missing values
	#	bMissing <- is.na(vals)

		# fit thin-plate-splines
		tps <- fields::Tps(coordinates, vals, lon.lat=TRUE)

		# do the actual interpolation 
		newFocal <- raster(focalFine)
		newFocal <- interpolate(newFocal, tps)

		# save
		res[i] <- newFocal

		cat(i, "\r")
		flush.console()
	}
	return(res)
}
