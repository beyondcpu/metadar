setClass("Dataset", contains="ExpressionSet")

setMethod("initialize", signature=c("Dataset"),
  function(.Object, ...) {
    readDataset(.Object, ...)
  })

setGeneric("readDataset",  def=function(Object, metabolomicsDataFile,
phenoDataFile) standardGeneric("readDataset"))

setMethod("readDataset", signature=c("Dataset", "missing", "missing"),
  function(Object) {
    Object
})

setMethod("readDataset", signature=c("Dataset", "character", "character"),
	function(Object, metabolomicsDataFile="exprs.csv",
		phenoDataFile="phenoDataFramea.csv") {
	phenoDataFrame <- read.csv(phenoDataFile)
	rownames(phenoDataFrame) <- as.character(phenoDataFrame[,"SampleName"])
	metabolomicsDataFrame <- read.csv(metabolomicsDataFile,check.names=F)
	dat <- metabolomicsDataFrame[which(!is.na(metabolomicsDataFrame[,"ID"])),
		as.character(phenoDataFrame[,"SampleName"])]
	rownames(dat) <- metabolomicsDataFrame[,"ID"]
	phenoData <- new("AnnotatedDataFrame", data=phenoDataFrame)
	Object@assayData=assayDataNew(exprs=apply(dat,2,as.numeric))
	featureNames(assayData(Object)) <- rownames(dat)
	Object@phenoData=phenoData
	Object
})

setMethod("readDataset", signature=c("Dataset", "character", "missing"),
	function(Object, metabolomicsDataFile="exprs.csv") {
		### The input is expected in a SimpleClassificationDataset format
		### i.e. Along the rows are compounds,
		### along the columns are samples
		### The first row contains sample names
		### The second row contains the class labels
		### The first column contains Id
	#phenoDataFrame <- read.csv(phenoDataFile)
	#rownames(phenoDataFrame) <- as.character(phenoDataFrame[,"SampleName"])
	inputData <- read.csv(metabolomicsDataFile,check.names=F)
	dat <- inputData[-1, -1]
	rownames(dat) <- inputData[-1,"ID"]
	phenoDataFrame <- data.frame("SampleName"=colnames(inputData)[2:ncol(inputData)],
				     "Class"=unlist(inputData[1,2:ncol(inputData)]))
	rownames(phenoDataFrame) <- colnames(inputData)[2:ncol(inputData)]
	phenoData <- new("AnnotatedDataFrame", data=phenoDataFrame)
	Object@assayData=assayDataNew(exprs=apply(dat,2,as.numeric))
	featureNames(assayData(Object)) <- rownames(dat)
	Object@phenoData=phenoData
	Object
})

##setGeneric("setDataset",  def=function(Object, metabolomicsDataFrame, phenoDataFrame) standardGeneric("setDataset"))

setMethod("readDataset", signature=c("Dataset", "data.frame", "data.frame"),
	function(Object, metabolomicsDataFile,
		phenoDataFile) {
  rownames(phenoDataFile) <- as.character(phenoDataFile[,"SampleName"])
	dat <- metabolomicsDataFile[which(!is.na(metabolomicsDataFile[,"ID"])),
		as.character(phenoDataFile[,"SampleName"])]
	rownames(dat) <- metabolomicsDataFile[,"ID"]
	phenoData <- new("AnnotatedDataFrame", data=phenoDataFile)
	Object@assayData=assayDataNew(exprs=apply(dat,2,as.numeric))
	featureNames(assayData(Object)) <- rownames(dat)
	Object@phenoData=phenoData
	Object
})

setMethod("readDataset", signature=c("Dataset", "data.frame", "missing"),
  function(Object, metabolomicsDataFile) {
    dat <- metabolomicsDataFile[-1, -1]
    rownames(dat) <- metabolomicsDataFile[-1,"ID"]
    phenoDataFrame <- data.frame("SampleName"=colnames(metabolomicsDataFile)[2:ncol(metabolomicsDataFile)],
                                 "Class"=unlist(metabolomicsDataFile[1,2:ncol(metabolomicsDataFile)]))
    rownames(phenoDataFrame) <- colnames(metabolomicsDataFile)[2:ncol(metabolomicsDataFile)]
    phenoData <- new("AnnotatedDataFrame", data=phenoDataFrame)
    Object@assayData=assayDataNew(exprs=apply(dat,2,as.numeric))
    featureNames(assayData(Object)) <- rownames(dat)
    Object@phenoData=phenoData
    Object
})

setMethod("readDataset", signature=c("Dataset", "ExpressionSet", "missing"),
          function(Object, metabolomicsDataFile) {
            Object@assayData=assayData(metabolomicsDataFile)
            Object@phenoData=phenoData(metabolomicsDataFile)
            Object
          })


setGeneric("setExprs", def=function(Object, exprs) standardGeneric("setExprs"))

setMethod("setExprs", signature=c("Dataset", "matrix"),
	function(Object, exprs=matrix()) {
	if(!is.null(pData(Object)))
	{
		exprs(Object) <- exprs[, as.character(pData(Object)[,"SampleName"])]
	} else {
		stop("pData(Object) is null, use the method
			setDataset(Object, metabolomicsDataFrame, phenoDataFrame)
			or readDataset(Object, metabolomicsDataFile, phoneDataFile) instead")
	}
	Object
})

setGeneric("univariateCorrelation", def=function(Object, covariate)
standardGeneric("univariateCorrelation"))

setMethod("univariateCorrelation",  signature=c(Object="Dataset",
covariate="character"), valueClass="data.frame", definition=function(Object,
		covariate) {
		expr <- exprs(Object)
		output <- pData(Object)[,covariate]
		cors <- vector("numeric",nrow(expr))
		pvals <- vector("numeric",nrow(expr))
		for(i in seq(nrow(expr))) {
			ct <- cor.test(expr[i,], output, alternative="t", method="pearson")
			cors[i] <- ct$estimate
			pvals[i] <- ct$p.value
		}
		qvals <- p.adjust(pvals, method="BH")
		return(data.frame(row.names=featureNames(assayData(Object)),
			"Pearson correlation"=cors, "P value"=pvals, "BH95 FDR Q value"=qvals))
})

setMethod("univariateCorrelation",  signature=c(Object="Dataset",
covariate="numeric"), valueClass="data.frame", definition=function(Object,
		covariate) {
		expr <- exprs(Object)
		output <- covariate
		cors <- vector("numeric",nrow(expr))
		pvals <- vector("numeric",nrow(expr))
		for(i in seq(nrow(expr))) {
			ct <- cor.test(expr[i,], output, alternative="t", method="pearson")
			cors[i] <- ct$estimate
			pvals[i] <- ct$p.value
		}
		qvals <- p.adjust(pvals, method="BH")
		return(data.frame(row.names=featureNames(assayData(Object)),
			"Pearson correlation"=cors, "P value"=pvals, "BH95 FDR Q value"=qvals))
})

setGeneric("univariateAUC", def=function(Object, covariate) standardGeneric("univariateAUC"))

setMethod("univariateAUC",  signature=c(Object="Dataset", covariate="character"),
	valueClass="numeric", definition=function(Object,
		covariate) {
		return(rowpAUCs(Object, fac=pData(Object)[,covariate])@AUC)
})

setGeneric("univariateTTest", def=function(Object, covariate) standardGeneric("univariateTTest"))

setMethod("univariateTTest",  signature=c(Object="Dataset", covariate="character"),
valueClass="data.frame", definition=function(Object,
		covariate) {
	res <- rowttests(Object, fac=covariate)
	colnames(res)[1] <- "T-statistic"
	colnames(res)[2] <- paste(covariate, "Difference of means")
	qvals <- p.adjust(res[,"p.value"], method="BH")
	means <- t(apply(exprs(Object), 1, function(x) {
			 unlist(lapply(split(x, factor(pData(Object)[,covariate])), mean))
			}))
	colnames(means) <- paste("Mean",colnames(means))
	return(data.frame(means, res, "BH95 FDR Q value"=qvals))
})

setGeneric("rankNormalization", function(Object) standardGeneric("rankNormalization"))

setMethod("rankNormalization", signature="Dataset",
  function(Object) {
    exprs(Object) <- apply(exprs(Object), 2, rank)
    Object
  })

setGeneric("concatenate", function(Object1, Object2) standardGeneric("concatenate"))

setMethod("concatenate", signature=c("Dataset", "Dataset"),
  function(Object1, Object2) {
  #### Right now it is assumed that
    ## pData(Object1) is identical to pData(Object2)
    ## TODO: improve the method to the case where
    ## sampleNames(Object1) and sampleNames(Object2) are identical as sets
    ## (but not necessarily in the same order)... i.e. phenoData objects may
    ## be complementary.
    cobj <- Object1
    cobj <- setExprs(cobj, rbind(exprs(Object1), exprs(Object2)))
    cobj <- setExprs(cobj, t(scale(t(exprs(cobj)))))
    cobj
})

setGeneric("varSel", function(Object, covariate, method, ...)
standardGeneric("varSel"))

setMethod("varSel", signature=c("Dataset", "character",
"character"), function(Object, covariate, method="glmnet", family="binomial", ...) {
	if(method=="glmnet") {
		y <- pData(Object)[,covariate]
		names(y) <- pData(Object)[,"SampleName"]
		if(family=="binomial") {
		  y = factor(y)
		}
		return(vsglmnet2(y=y, x=t(exprs(Object)), family=family, ...))
	}
})

setGeneric("printDataset", function(Object, filename) standardGeneric("printDataset"))

setMethod("printDataset", signature=c("Dataset", "missing"),
	  function(Object) {
		  dat <- rbind(t(pData(Object)), exprs(Object))
		  write.csv(dat, file=paste("Dataset", Sys.time(), ".csv", sep=""))
	  })

setMethod("printDataset", signature=c("Dataset", "character"),
	  function(Object, filename) {
		  dat <- rbind(t(pData(Object)), exprs(Object))
		  write.csv(dat, file=filename)
	  })



#### the following is an implementation for the generic stats/na.omit
setMethod("na.omit", signature=c("Dataset"),
	  function(object) {
		  object <- object[-na.action(na.omit(exprs(object))),]
	  })

### This is for taking an intersection of samples between two data sets
### Combining data sets in terms of compounds is handled by Guineu as an alignment task
setMethod("intersect", signature=c("Dataset", "Dataset"),
	  function(x, y) {
		  sampleNames1 <- sampleNames(x)
		  sampleNames2 <- sampleNames(y)
		  print("\nThe following samples are in x but not in y:\n")
		  print(setdiff(sampleNames1, sampleNames2))
		  print("\nThe following samples are in y but not in x:\n")
		  print(setdiff(sampleNames2, sampleNames1))
		  commonSampleNames <- intersect(sampleNames1, sampleNames2)
		  if(length(commonSampleNames) == 0) {
			  stop("\nThere are no common samples between object1 and object 2\n")
		  }

		  x2 <- x[,commonSampleNames]
		  y2 <- y[,commonSampleNames]

		  pdf <- data.frame(pData(x2), pData(y2))
		  exprdf <- rbind(exprs(x2), exprs(y2))
		  Object <- new("Dataset")
		  Object@phenoData=new("AnnotatedDataFrame", data=pdf)
		  Object@featureData=new("AnnotatedDataFrame",
			 data=data.frame(row.names=c(paste("1", featureNames(x2), sep=""),
				  paste("2", featureNames(y2), sep=""))))
		  Object@assayData=assayDataNew(exprs=exprdf)
		  featureNames(Object) <- c(paste("1", featureNames(x2), sep=""),
				  paste("2", featureNames(y2), sep=""))
		  Object
	  })
