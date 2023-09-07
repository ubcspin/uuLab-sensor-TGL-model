#datadirpress <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/pressure"
#datadirsx <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/shearx"
#datadirsy <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/sheary"
datadirpress <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/sept6/P1b_processed_csv_data/P1b/pressure"
datadirsx <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/sept6/P1b_processed_csv_data/P1b/shearX"
datadirsy <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/sept6/P1b_processed_csv_data/P1b/shearY"


todir <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/featurefiles"
setwd(datadirsy)


###################################################################################
#	this set of fxns is file name processing									  #
###################################################################################
getPFCondition<-function(x) {
	condition <- "error"
	if (grepl("FLAT", x)) {
		condition <- "flat"
	}
	if (grepl("PILLOW", x)) {
		condition <- "pillow"
	}
	return(condition)
}

getGloveCondition<-function(x) {
	condition <- "notgloved"
	
	if (grepl("glovefinger", x)) {
		condition <- "gloved"
	}
	if (grepl("nofinger", x)) {
		condition <- "notgloved"
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
	if (grepl("freeplay", x)) {
		gesture <- "freeplay"
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
	if (grepl("tickle", x)) {
		gesture <- "tickle"
	} 
	if (grepl("twist", x)) {
		gesture <- "twist"
	} 
	if (grepl("type", x)) {
		gesture <- "type"
	} 
	return(gesture)
}

getPSubject<-function(x) {
	subNumLoc <- unlist(gregexpr('_P', x))[1]
	
	#check if participant number is single digit (i.e. P1)
	
	subNum <- substr(x, subNumLoc+1, subNumLoc+5)
	subN <- as.numeric(gsub("\\D+","",subNum))
	subP <- paste("P", subN, sep="")

	return(subP)
}

getTestSubject <- function(x) {
	subject <- "error"
	
	if (grepl("LC", x)) {
		subject <- "LC"
	} 
	if (grepl("RG", x)) {
		subject <- "RG"
	} 
	if (grepl("WZ", x)) {
		subject <- "WZ"
	} 
	if (grepl("DM", x)) {
		subject <- "DM"
	} 
	 
	return(subject)
}

getCount <- function(x) {
	
	filehalf <- substr(x, nchar(x)-10, nchar(x))
	filecount <- as.numeric(gsub("\\D+","",filehalf))
		
	if(!is.na(filecount)) {
		return(filecount)
	} else {
		return(-1)
	}

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
	"subcondition", "subject", "filecount", "gesture")
	
gestureFiles <- list.files()
featureData <- seq(along.with = windowFeatures)
needsProcessing <- c("num", "filename", "filelength")


for (j in 1:length(gestureFiles)) {

	print(paste("Processing ", gestureFiles[j], " which is: ", j, " of ", length(gestureFiles), sep=""))
	
	thisFile <- gestureFiles[j]
	
	thisPFCondition <- getPFCondition(thisFile)
	thisPSubject <- "P1b"
	thisGesture <- getGesture(thisFile)
	thisCount <- getCount(thisFile)
	

	
	thisData <- read.csv(thisFile, header=FALSE)
	filerow <- c(thisFile, nrow(thisData))
	
	if(nrow(thisData) >= 320) {
	
		framesum <- 0
		thisX <- 0
		thisY <- 0
	
		for (i in 1:320) {
			
			#print(paste("Calculating fs, x, y in ", i, " of 450", sep=""))
			framesum <- c(framesum, getFramesum(thisData[i,]))
			thisX <- c(thisX, getX(thisData[i,]))
			thisY <- c(thisY, getY(thisData[i,]))
			
		}
		
		framesums <- framesum[-1]
		Xs <- thisX[-1]
		Ys <- thisY[-1]
		
		datainst1 <- c(findtuple(framesums[1:80]), findtuple(Xs[1:80]), findtuple(Ys[1:80]), thisPFCondition, thisPSubject, thisCount, thisGesture)
		datainst2 <- c(findtuple(framesums[81:160]), findtuple(Xs[81:160]), findtuple(Ys[81:160]), thisPFCondition, thisPSubject, thisCount, thisGesture)
		datainst3 <- c(findtuple(framesums[161:240]), findtuple(Xs[161:240]), findtuple(Ys[161:240]), thisPFCondition, thisPSubject, thisCount, thisGesture)
		datainst4 <- c(findtuple(framesums[241:320]), findtuple(Xs[241:320]), findtuple(Ys[241:320]), thisPFCondition, thisPSubject, thisCount, thisGesture)
		
		featureData <- rbind(featureData, datainst1, datainst2, datainst3, datainst4)
			
	} else {
	
		needsProcessing <- rbind(j, needsProcessing, filerow)

	}
}


colnames(needsProcessing) <- needsProcessing[1,]
needsProcessing <- needsProcessing[-1,]
print(needsProcessing)

setwd(todir)
featureData <- featureData[-1,]
colnames(featureData) <- windowFeatures
write.csv(featureData, file="gesturefeaturefileshearYP1b-sept6.csv", row.names=FALSE)

