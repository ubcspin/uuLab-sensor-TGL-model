datadir <- "C:/Users/Laura/Documents/School/2017-2022 PhD/Projects/uuLab/haarttest"
todir <- "C:/Users/Laura/Documents/School/2017-2022 PhD/Projects/uuLab/haartfile"
setwd(datadir)

###################################################################################
#	this set of fxns processes the frame										  #
###################################################################################

getFramesum <- function(x) {

	framesum <- sum(x)
	
	return(framesum)
}

getX  <- function(x) {

	S <- sum( x[1],   x[9],    x[17],   x[25],   x[33],   x[41],   x[49],   x[57],
			2*x[2], 2*x[10], 2*x[18], 2*x[26], 2*x[34], 2*x[42], 2*x[50], 2*x[58],
			3*x[3], 3*x[11], 3*x[19], 3*x[27], 3*x[35], 3*x[43], 3*x[51], 3*x[59],
			4*x[4], 4*x[12], 4*x[20], 4*x[28], 4*x[36], 4*x[44], 4*x[52], 4*x[60],
			5*x[5], 5*x[13], 5*x[21], 5*x[29], 5*x[37], 5*x[45], 5*x[53], 5*x[61],
			6*x[6], 6*x[14], 6*x[22], 6*x[30], 6*x[38], 6*x[46], 6*x[54], 6*x[62],
			7*x[7], 7*x[15], 7*x[23], 7*x[31], 7*x[39], 7*x[47], 7*x[55], 7*x[63],
			8*x[8], 8*x[16], 8*x[24], 8*x[32], 8*x[40], 8*x[48], 8*x[56], 8*x[64])
 	A <- sum(x)
	
	xcoord <- S/A
	
	return(xcoord)

}

getY <- function(x) {

	S <- sum(x[1:8], 2*x[9:16], 3*x[17:24], 4*x[25:32], 5*x[33:40], 6*x[41:48], 7*x[49:56], 8*x[57:64])
	A <- sum(x)
	
	ycoord <- S/A
	
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
	"subject", "substrate", "cover", "gesture")
	

featureData <- seq(along.with = windowFeatures)
thisFile <- read.csv("t129.csv", header=TRUE)

for (j in 1:nrow(thisFile)) {

	#print(paste("Processing ", gestureFiles[j], " which is: ", j, " of ", length(gestureFiles), sep=""))
	
	#thisFile <- gestureFiles[j]
	
	thisCondition <- getCondition(thisFile)
	thisSubject <- getSubject(thisFile)
	thisGesture <- getGesture(thisFile)
	
	thisData <- read.csv(thisFile, header=FALSE)
	
	framesum <- 0
	thisX <- 0
	thisY <- 0
	
	for (i in 1:450) {
		
		#print(paste("Calculating fs, x, y in ", i, " of 450", sep=""))
		framesum <- c(framesum, getFramesum(thisData[i,]))
		thisX <- c(thisX, getX(thisData[i,]))
		thisY <- c(thisY, getY(thisData[i,]))
		
	}
	
	framesums <- framesum[-1]
	Xs <- thisX[-1]
	Ys <- thisY[-1]
	
	datainst1 <- c(findtuple(framesums[1:100]), findtuple(Xs[1:100]), findtuple(Ys[1:100]), thisCondition, thisSubject, thisGesture)
	datainst2 <- c(findtuple(framesums[101:200]), findtuple(Xs[101:200]), findtuple(Ys[101:200]), thisCondition, thisSubject, thisGesture)
	datainst3 <- c(findtuple(framesums[201:300]), findtuple(Xs[201:300]), findtuple(Ys[201:300]), thisCondition, thisSubject, thisGesture)
	datainst4 <- c(findtuple(framesums[301:400]), findtuple(Xs[301:400]), findtuple(Ys[301:400]), thisCondition, thisSubject, thisGesture)
	
	featureData <- rbind(featureData, datainst1, datainst2, datainst3, datainst4)
	
}

setwd(todir)
featureData <- featureData[-1,]
colnames(featureData) <- windowFeatures
write.csv(featureData, file="gesturefeaturefile.csv", row.names=FALSE)

