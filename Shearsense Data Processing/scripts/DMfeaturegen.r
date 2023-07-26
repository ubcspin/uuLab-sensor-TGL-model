datadir <- "C:/Users/Laura/Documents/School/2017-2022 PhD/Projects/uuLab/datafolder/DM/data/allinone"
todir <- "C:/Users/Laura/Documents/School/2017-2022 PhD/Projects/uuLab/new data/data/datafile"
setwd(datadir)

###################################################################################
#	this set of fxns is file name processing									  #
###################################################################################
getCondition<-function(x) {
	condition <- "error"
	if (grepl("FLAT", x)) {
		condition <- "flat"
	}
	if (grepl("PILLOW", x)) {
		condition <- "pillow"
	}
	return(condition)
}

#get gesture performed
getGesture<-function(x) {
	gesture <- "error"

	if (grepl("constant", x)) {
		gesture <- "constant"
	} 
	if (grepl("drag", x)) {
		gesture <- "drag"
	} 
	if (grepl("fist", x)) {
		gesture <- "fist"
	} 
	if (grepl("karate", x)) {
		gesture <- "karate"
	} 
	if (grepl("knead", x)){
		gesture <- "knead"
	} 
	if (grepl("massage", x)) {
		gesture <- "massage"
	} 
	if (grepl("no", x)) {
		gesture <- "no"
	} 
	if (grepl("pat", x)) {
		gesture <- "pat"
	} 
	if (grepl("pinch", x)) {
		gesture <- "pinch"
	} 
	if (grepl("poke", x)) {
		gesture <- "poke"
	} 
	if (grepl("press", x)) {
		gesture <- "press"
	} 
	if (grepl("rub", x)){
		gesture <- "rub"
	} 
	if (grepl("scratch", x)) {
		gesture <- "scratch"
	} 	
	if (grepl("slide", x)) {
		gesture <- "slide"
	} 
	if (grepl("stroke", x)) {
		gesture <- "stroke"
	} 
	if (grepl("tap", x)) {
		gesture <- "tap"
	} 
	if (grepl("twist", x)) {
		gesture <- "twist"
	} 
	if (grepl("type", x)) {
		gesture <- "type"
	} 
	return(gesture)
}

getSubject<-function(x) {
	subNumLoc <- unlist(gregexpr('_', x))[1]
	
	#check if participant number is single digit (i.e. P1)
	
	subNum <- substr(x, 1, subNumLoc-1)

	return(subNum)
}

###################################################################################
#	this set of fxns processes the frame										  #
###################################################################################

getFramesum <- function(x) {

	framesum <- sum(x)
	
	return(framesum)
}

getX  <- function(x) {

	S <- sum( x[1],   x[7],   x[13],   x[19],   x[25],   x[31],
			2*x[2], 2*x[8], 2*x[14], 2*x[20], 2*x[26], 2*x[32],
			3*x[3], 3*x[9], 3*x[15], 3*x[21], 3*x[27], 3*x[33],
			4*x[4], 4*x[10],4*x[16], 4*x[22], 4*x[28], 4*x[34],
			5*x[5], 5*x[11],5*x[17], 5*x[23], 5*x[29], 5*x[25],
			6*x[6], 6*x[12],6*x[18], 6*x[24], 6*x[30], 6*x[36])
	A <- sum(x)
	
	if(A==0){
		
		xcoord <- 0
	
	} else {
	
		xcoord <- S/A
	
	}
	
	return(xcoord)

}

getY <- function(x) {

	S <- sum(x[1:6], 2*x[7:12], 3*x[13:18], 4*x[19:24], 5*x[25:30], 6*x[31:36])
	A <- sum(x)
	
	if(A==0){
		
		ycoord <- 0
	
	} else {
	
		ycoord <- S/A
	
	}
	
	return(ycoord)

}


###################################################################################
#	this set of fxns is statistical feature calcs								  #
###################################################################################

totalvar<-function(x) {
	tmptv<-0
	for (i in 1:(length(x)-1)) {	
		tmp<-abs(x[i] - x[i+1])
		tmptv<-tmptv+tmp
	}
	return(tmptv)
}

auc<-function(x) {
	return(sum(x))
}

findtuple<-function(x) {
	foundmax<-max(x)
	foundmin<-min(x)
	foundmean<-mean(x)
	foundmedian<-median(x)
	foundvariance<-var(x)
	foundtotvar<-totalvar(x)
	foundauc<-auc(x)
	tuple<-c(foundmax, foundmin, foundmean, foundmedian, foundvariance, foundtotvar, foundauc)
	return(as.numeric(tuple))
}

windowFeatures <- c("max", "min", "mean", "median", "var", "totalvar", "auc", 
	"Xmax", "Xmin", "Xmean", "Xmedian", "Xvar", "Xtotalvar", "Xauc", 
	"Ymax", "Ymin", "Ymean", "Ymedian", "Yvar", "Ytotalvar", "Yauc", 
	"condition", "subject", "gesture")
	
gestureFiles <- list.files()
featureData <- seq(along.with = windowFeatures)

for (j in 1:length(gestureFiles)) {

	print(paste("Processing ", gestureFiles[j], " which is: ", j, " of ", length(gestureFiles), sep=""))
	
	thisFile <- gestureFiles[j]
	
	#thisCondition <- getCondition(thisFile)
	thisCondition <- "flat"
	#thisSubject <- getSubject(thisFile)
	thisSubject <- "DM"
	thisGesture <- getGesture(thisFile)
	
	thisData <- read.csv(thisFile, header=FALSE)
	
	framesum <- 0
	thisX <- 0
	thisY <- 0
	
	for (i in 1:nrow(thisData)) {
		
		#print(paste("Calculating fs, x, y in ", i, " of 450", sep=""))
		framesum <- c(framesum, getFramesum(thisData[i,]))
		thisX <- c(thisX, getX(thisData[i,]))
		thisY <- c(thisY, getY(thisData[i,]))
		
	}
	
	framesums <- framesum[-1]
	Xs <- thisX[-1]
	Ys <- thisY[-1]
	
	datainst1 <- c(findtuple(framesums[1:80]), findtuple(Xs[1:80]), findtuple(Ys[1:80]), thisCondition, thisSubject, thisGesture)
	datainst2 <- c(findtuple(framesums[81:160]), findtuple(Xs[81:160]), findtuple(Ys[81:160]), thisCondition, thisSubject, thisGesture)
	datainst3 <- c(findtuple(framesums[161:240]), findtuple(Xs[161:240]), findtuple(Ys[161:240]), thisCondition, thisSubject, thisGesture)
	datainst4 <- c(findtuple(framesums[241:320]), findtuple(Xs[241:320]), findtuple(Ys[241:320]), thisCondition, thisSubject, thisGesture)
	
	featureData <- rbind(featureData, datainst1, datainst2, datainst3, datainst4)
	
}

setwd(todir)
featureData <- featureData[-1,]
colnames(featureData) <- windowFeatures
write.csv(featureData, file="DMgesturefeaturefile.csv", row.names=FALSE)

