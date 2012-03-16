setClass("Dataset", contains="ExpressionSet")

setGeneric("readDataset",  def=function(Object, metabolomicsDataFile,
phenoDataFile) standardGeneric("readDataset"))

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

setGeneric("setDataset",  def=function(Object, metabolomicsDataFrame, phenoDataFrame) standardGeneric("setDataset"))

setMethod("setDataset", signature=c("Dataset", "data.frame", "data.frame"),
	function(Object, metabolomicsDataFrame=data.frame(),
		phenoDataFrame=data.frame()) {
	dat <- metabolomicsDataFrame[which(!is.na(metabolomicsDataFrame[,"ID"])),
		as.character(phenoDataFrame[,"SampleName"])]
	rownames(dat) <- metabolomicsDataFrame[,"ID"]
	phenoData <- new("AnnotatedDataFrame", data=phenoDataFrame)
	Object@assayData=assayDataNew(exprs=apply(dat,2,as.numeric))
	featureNames(assayData(Object)) <- rownames(dat)
	Object@phenoData=phenoData
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

setGeneric("varSel", function(Object, covariate, method, ...)
standardGeneric("varSel"))

setMethod("varSel", signature=c("Dataset", "character",
"character"), function(Object, covariate, method, family="binomial", ...) {
	if(method=="glmnet") {
		y <- pData(Object)[,covariate]
		names(y) <- pData(Object)[,"SampleName"]
    if(family=="binomial") {
      y = factor(y)
    }
		return(vsglmnet2(y=y, x=t(exprs(Object)), family=family, ...))
	}
})

setGeneric("zeroImputation", function(Object, method, covariate) standardGeneric("zeroImputation"))
### something is wrong
setMethod("zeroImputation", signature=c("Dataset", "character", "missing"),
	  function(Object, method="HalfMin") {
		  dat <- exprs(Object)
		  dat[is.na(dat)] <- 0
		  if(method=="HalfMin") {
			  for(i in seq(nrow(dat))) {
				  if(!all(dat[i,]!=0) && (sum(dat[i,]) !=0)) {
					  dat[i,which(dat[i,]==0)] <- min(dat[i,which(dat[i,]!=0)])/2
				  }
			  }
		  }

		  exprs(Object) <- dat
		  Object
})

setMethod("zeroImputation", signature=c("Dataset", "character", "character"),
	  function(Object, method="HalfMin", covariate) {
		  groups <- levels(factor(pData(Object)[,covariate]))
		  for(j in seq(length(groups))) {
			  datg <- get.array.subset(Object, covariate, groups[j])
			  dat <- exprs(datg)
			  dat[is.na(dat)] <- 0

			  if(method=="HalfMin") {

				  for(i in seq(nrow(dat))) {
					  if(!all(dat[i,]!=0) && (sum(dat[i,]) !=0)) {
						  dat[i,which(dat[i,]==0)] <- min(dat[i,which(dat[i,]!=0)])/2
					  }
				  }
			  }

			  exprs(datg) <- dat
			  if(j==1) {
				  ret.Obj <- datg
			  } else {
				  ret.Obj <- combine(ret.Obj, datg)
			  }
		  }
		  ret.Obj
	  })

setGeneric("printDataset", function(Object) standardGeneric("printDataset"))

setMethod("printDataset", signature=c("Dataset"),
	  function(Object) {
		  dat <- rbind(t(pData(Object)), exprs(Object))
		  write.csv(dat, file=paste("Dataset", Sys.time(), ".csv", sep=""))
	  })

setGeneric("oneWayAnova", function(Object, covariate) standardGeneric("oneWayAnova"))

setMethod("oneWayAnova", signature=c("Dataset", "character"),
	  function(Object, covariate) {
		  res <- vector("list", nrow(Object))
      pvals <- vector("numeric", nrow(Object))
		  for(i in seq(nrow(Object))) {
			  x <- exprs(Object)[i,]
			  fit <- aov(x~f, data=data.frame("x"=x, "f"=factor(pData(Object)[,covariate])))
			  tuk <- TukeyHSD(fit)
			  res.i <- c(summary(fit)[[1]]["f","F value"],
                   summary(fit)[[1]]["f","Pr(>F)"],
                   tuk$f[,c(1,4)])
			  names(res.i) <- c(paste("One way Anova (", covariate, ") F-value"),
                          paste("One way Anova (", covariate, ")P-value"),
                          paste(rownames(tuk$f), colnames(tuk$f)[1]),
                          paste(rownames(tuk$f), colnames(tuk$f)[4]))
			  res[[i]] <- res.i
        pvals[i] <- summary(fit)[[1]]["f","Pr(>F)"]
		  }
		  res <- t(data.frame(res, check.names=F))
		  rownames(res) <- featureNames(Object)
      qvals <- p.adjust(pvals, method="BH")
      res <- data.frame(res, qvals, check.names=F)
      colnames(res)[ncol(res)] <- paste("One way Anova (", covariate, ") FDR-BH95 Q value")
		  return(res)
	  })


setGeneric("twoWayAnova", function(Object, covariate1, covariate2) standardGeneric("twoWayAnova"))

setMethod("twoWayAnova", signature=c("Dataset", "character", "character"),
	  function(Object, covariate1, covariate2) {
		  res <- vector("list", nrow(Object))
		  res.diff <- vector("list", nrow(Object))
		  res.p <- vector("list", nrow(Object))
		  for(i in seq(nrow(Object))) {
			  x <- exprs(Object)[i,]
			  fit <- aov(x~f1*f2, data=data.frame("x"=x, "f1"=factor(pData(Object)[,covariate1]), "f2"=factor(pData(Object)[,covariate2])))
			  tuk <- TukeyHSD(fit)

			  res.i <- c(summary(fit)[[1]][1,"F value"],
				     summary(fit)[[1]][1,"Pr(>F)"],
				     summary(fit)[[1]][2,"F value"],
				     summary(fit)[[1]][2,"Pr(>F)"],
				     summary(fit)[[1]][3,"F value"],
				     summary(fit)[[1]][3,"Pr(>F)"],
				     tuk$f1[,c(1,4)],
				     tuk$f2[,c(1,4)],
				     tuk$"f1:f2"[,c(1,4)])
			  names(res.i) <- c(paste(covariate1, "F-value"), 
					    paste(covariate1, "P-value"),
					    paste(covariate2, "F-value"),
					    paste(covariate2, "P-value"),
					    paste(covariate1, ":", covariate2, "F-value"),
					    paste(covariate1, ":", covariate2, "P-value"),
					    paste(rownames(tuk$f1), colnames(tuk$f1)[1]),
					    paste(rownames(tuk$f1), colnames(tuk$f1)[4]),
					    paste(rownames(tuk$f2), colnames(tuk$f2)[1]),
					    paste(rownames(tuk$f2), colnames(tuk$f2)[4]),
					    paste(rownames(tuk$"f1:f2"), colnames(tuk$"f1:f2")[1]),
					    paste(rownames(tuk$"f1:f2"), colnames(tuk$"f1:f2")[4]))

			  res[[i]] <- res.i 

			  res.diff.i <- c(tuk$f1[,1], tuk$f2[,1], tuk$"f1:f2"[,1])
			  names(res.diff.i) <- c(paste(rownames(tuk$f1), colnames(tuk$f1)[1]),
						 paste(rownames(tuk$f2), colnames(tuk$f2)[1]),
						 paste(rownames(tuk$"f1:f2"), colnames(tuk$"f1:f2")[1]))
			  res.diff[[i]] <- res.diff.i

			  res.p.i <- c(tuk$f1[,4], tuk$f2[,4], tuk$"f1:f2"[,4])
			  names(res.p.i) <- c(paste(rownames(tuk$f1), colnames(tuk$f1)[4]),
					      paste(rownames(tuk$f2), colnames(tuk$f2)[4]),
					      paste(rownames(tuk$"f1:f2"), colnames(tuk$"f1:f2")[4]))
			  res.p[[i]] <- res.p.i
		  }
		  res <- t(data.frame(res))
		  rownames(res) <- featureNames(Object)
      
      ## differences of means
		  res.diff <- t(data.frame(res.diff))
		  rownames(res.diff) <- featureNames(Object)
      ## p values
		  res.p <- t(data.frame(res.p))
		  rownames(res.p) <- featureNames(Object)
      ## ratios of means
      #browser()
      
		  return(list("BigResultTable"=res, "DifferencesTable"=res.diff, "PvalsTable"=res.p))
	  })

setGeneric("boxPlot", function(Object, covariate1, covariate2) standardGeneric("boxPlot"))

setMethod("boxPlot", signature=c("Dataset", "character", "missing"),
	  function(Object, covariate1) {
		  for(i in seq(nrow(Object))) {
        fit <- aov(x~f, data=data.frame("x"=exprs(Object)[i,],
                                        "f"=factor(pData(Object)[,covariate1])
                                        ))
			  boxplot(exprs(Object)[i,]~factor(pData(Object)[,covariate1]),
				  main=paste("Name:", featureNames(Object)[i],
                     "\nAnova P-value:",
                     round(summary(fit)[[1]]["f","Pr(>F)"], 5)))
		  }
	  })

setMethod("boxPlot", signature=c("Dataset", "character", "character"),
	  function(Object, covariate1, covariate2) {
      previous <- par(mai=c(0.5,0.5,1,0.2))
		  for(i in seq(nrow(Object))) {
		    fit <- aov(x~f1*f2, data=data.frame("x"=exprs(Object)[i,],
                                            "f1"=factor(pData(Object)[,covariate1]),
                                            "f2"=factor(pData(Object)[,covariate2])
                                            )
                   )
        p1 <- round(summary(fit)[[1]][1,"Pr(>F)"],5)
        p2 <- round(summary(fit)[[1]][2,"Pr(>F)"], 5)
        p3 <- round(summary(fit)[[1]][3,"Pr(>F)"], 5)
        if((p1 < 0.05) | (p2 < 0.05) | (p3 < 0.05)) {
    		  boxplot(exprs(Object)[i,]~factor(paste(pData(Object)[,covariate1],
  								 pData(Object)[,covariate2])),
  				  main=paste("Name:", featureNames(Object)[i],
                       "\n", covariate1, "p-value", p1,
                       "\n", covariate2, "p-value", p2,
                       "\n", covariate1, covariate2, "interaction p-value", p3))
            
        }
		  }
      par(previous)
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
