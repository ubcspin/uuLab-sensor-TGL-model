datadirpress <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/pressure"
datadirsx <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/shearx"
datadirsy <- "C:/Users/Laura/Documents/GitHub/uuLab-sensor-TGL-model/processed_csv_data/sheary"
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

calcXYmag <- function(x, y) {

	sumX <- sum(x)
	sumY <- sum(y)
	
	XYmag <- sqrt(sumX^2 + sumY^2)
	
	return(XYmag)

}

calcXYang <- function(x, y) {

	sumX <- sum(x)
	sumY <- sum(y)
	
	XYang <- atan2(sumY, sumX)
	
	return(XYang)

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

windowFeatures <- c("maxR", "minR", "meanR", "medianR", "varR", "totalvarR", "aucR", 
	"maxTheta", "minTheta", "meanTheta", "medianTheta", "varTheta", "totalvarTheta", "aucTheta",
	"subconditionV", "subjectV", "filecountV", "gestureV")

setwd(datadirsx)
gestureFilesX <- list.files()
setwd(datadirsy)
gestureFilesY <- list.files()
featureData <- seq(along.with = windowFeatures)
needsProcessing <- c("num", "filename", "filelength")

for (j in 1:length(gestureFilesX)) {

	
	print(paste("Processing ", gestureFilesX[j], " which is: ", j, " of ", length(gestureFilesX), sep=""))
	setwd(datadirsx)
	thisXFile <- gestureFilesX[j]
	
	thisXPFCondition <- getPFCondition(thisXFile)
	thisXPSubject <- getPSubject(thisXFile)
	thisXGesture <- getGesture(thisXFile)
	thisXCount <- getCount(thisXFile)
	Xinfo <- c(thisXPFCondition, thisXPSubject, thisXGesture, thisXCount)
	
	setwd(datadirsy)
	thisYFile <- gestureFilesY[j]
	
	thisYPFCondition <- getPFCondition(thisYFile)
	thisYPSubject <- getPSubject(thisYFile)
	thisYGesture <- getGesture(thisYFile)
	thisYCount <- getCount(thisYFile)
	
	Yinfo <- c(thisYPFCondition, thisYPSubject, thisYGesture, thisYCount)
	
	
	
	if(identical(Xinfo, Yinfo)) {
		setwd(datadirsx)
		thisXData <- read.csv(thisXFile, header=FALSE)
		
		setwd(datadirsy)
		thisYData <- read.csv(thisYFile, header=FALSE)
		
		thisPFCondition <- thisXPFCondition
		thisPSubject <- thisXPSubject
		thisGesture <- thisXGesture
		thisCount <- thisXCount
		
		if((nrow(thisXData) == nrow(thisYData)) && (nrow(thisXData) >=320)) {
		
			thisRData <- seq(1,36)
			thisAData <- seq(1,36)
		
			for(r in 1:nrow(thisXData)) {
			
				thisRrow <- sqrt(sum(thisXData[r,]^2, thisYData[r,]^2))
				thisRData <- rbind(thisRData, thisRrow)
				thisArow <- atan2(thisYData[r,], thisXData[r,])
				thisAData <- rbind(thisAData, thisArow
			
			}
		
		} else {
		
			print(paste("Error: ", thisXFile, " has different dimensions to ", thisYFile, sep=""))
		
		}
	
	
	 # now I have an XYmag = r and an XYang = theta
		frameR <- 0
		thisRX <- 0
		thisRY <- 0
		
		frameA <- 0
		thisAX <- 0
		thisAY <- 0
		
		for (i in 1:320) {
			
			#print(paste("Calculating fs, x, y in ", i, " of 450", sep=""))
			frameR <- c(frameR, getFramesum(thisData[i,]))
			thisRX <- c(thisX, getX(thisRData[i,]))
			thisRY <- c(thisY, getY(thisRData[i,]))
			frameA <- c(frameA, mod(getFramesum(thisAData[i,]),360))
			thisAX <- c(thisX, getX(thisRData[i,]))
			thisAY <- c(thisY, getY(thisRData[i,]))
			
		}

		
			
		datainst1 <- c(findtuple(frameR[1:80]), findtuple(thisRX[1:80]), findtuple(thisRY[1:80]), findtuple(frameA[1:80]), findtuple(thisAX[1:80]), findtuple(thisAY[1:80]), thisPFCondition, thisPSubject, thisCount, thisGesture)
		datainst2 <- c(findtuple(frameR[81:160]), findtuple(thisRX[81:160]), findtuple(thisRY[81:160]), findtuple(frameA[81:160]), findtuple(thisAX[81:160]), findtuple(thisAY[81:160]), thisPFCondition, thisPSubject, thisCount, thisGesture)
		datainst3 <- c(findtuple(frameR[161:240]), findtuple(thisRX[161:240]), findtuple(thisRY[161:240]), findtuple(frameA[161:240]), findtuple(thisAX[161:240]), findtuple(thisAY[161:240]), thisPFCondition, thisPSubject, thisCount, thisGesture)
		datainst4 <- c(findtuple(frameR[241:320]), findtuple(thisRX[241:320]), findtuple(thisRY[241:320]), findtuple(frameA[241:320]), findtuple(thisAX[241:320]), findtuple(thisAY[241:320]), thisPFCondition, thisPSubject, thisCount, thisGesture)
	
		featureData <- rbind(featureData, datainst1, datainst2, datainst3, datainst4)
		
		 
		
	} else {
	
		needsProcessing <- rbind(j, needsProcessing, nrow(thisXData))

	}
	
}


colnames(needsProcessing) <- needsProcessing[1,]
needsProcessing <- needsProcessing[-1,]
print(needsProcessing)

setwd(todir)
featureData <- featureData[-1,]
colnames(featureData) <- windowFeatures
write.csv(featureData, file="vresFeatures.csv", row.names=FALSE)

write.csv(needsProcessing, file="vresneedsProcessing.csv", row.names=FALSE)

