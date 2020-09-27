# loading function for functions
funky <- function(path="scripts/methods/"){
	all <- list.files(path)
	rFiles <- all[grep(".R", all)]
	for(i in 1:length(rFiles)) source(file.path(path, rFiles[i]))
}


# utiliy function to print a function in an RMarkDown document
#' @param f Function name to be displayed (characters)
#' @param path Where the function files are, folder
# THe only restriction is that the function and assignment have to be on the same line, and the function definition
# has to end with } - no one-line functions, and add a space at the end of the function name, before the <-!
printfunction <- function(f, path="scripts/methods/"){
	# name
	f <- paste0(f, " ")

	# read everything
	all <- list.files(path)
	rFiles <- all[grep(".R", all)]
	
	allLines <- NULL

	for(i in 1:length(rFiles)){
		newLines <- readLines(file.path(path, rFiles[i]))
		allLines <- c(allLines, newLines)
	}

	# find assignment statements
	nAssign <- grep("<-", allLines)

	# find function
	nFunction <- grep("function", allLines)

	# 
	nBoth <- nAssign[nAssign%in%nFunction]

	# the current target
	curFunction <- grep(f, allLines[nBoth])

	# starting Line
	startLine <- nBoth[curFunction] 

	if(curFunction==length(nBoth)){
		endLine <- length(allLines)
	}else{
		endLine <- nBoth[curFunction+1]-1
	}

	# range of a function
	funDomain <- allLines[startLine:endLine]

	# omit the garbage at the end
	going <- TRUE

	i <- length(funDomain)
	# while the end of the function is not found
	while(going){

		linego <- TRUE

		# start with the first element
		j <- 1

		# go through the line
		while(linego){
			# substring
			sst <- substr(start=1, stop=j, funDomain[i])
			# do a test for the comment
			comm <- grep("#", sst)

			# if this is the comment than the loop has to go on
			if(length(comm)>0){
				linego <- FALSE
			}

			# if the closing braces are found, stop the whole thing
			close <- grep("}", sst)

			if(length(close)>0){
				linego <- FALSE
				going <- FALSE
			}

			# if the line length is reached, without nothing, break the loop
			if(j>=nchar(funDomain[i])){
				linego <- FALSE
			}
			# increment j
			j <- j+1

		}
		# decrement, go to previous line
		i <- i-1
	}

	# the printARea
	printDomain <- funDomain[1:(i+1)]

	b<- c("```r", printDomain, "```")
	
	# then dispplay
	for(i in 1:length(b)) cat(b[i],"\n")
}

printUseall <- function(path="../misc/useall.R"){
	a <-readLines(path)
	a <- c("```r", a, "```")
	for(i in 1:length(a)) cat(a[i],"\n")
}
