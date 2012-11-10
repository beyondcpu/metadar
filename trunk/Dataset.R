setClass("Dataset", contains="ExpressionSet")

setMethod("initialize", signature=c("Dataset"),
          function(.Object, ...) {
            readDataset(.Object, ...)
          })

setGeneric("readDataset",  def=function(Object, metabolomicsDataFile,
                                        phenoDataFile, ...) standardGeneric("readDataset"))

setMethod("readDataset", signature=c("Dataset", "missing", "missing"),
          function(Object) {
            Object
          })

setMethod("readDataset", signature=c("Dataset", "character", "character"),
          function(Object, metabolomicsDataFile="exprs.csv",
                   phenoDataFile="phenoDataFramea.csv", ...) {
            phenoDataFrame <- read.csv(phenoDataFile, ...)
            rownames(phenoDataFrame) <- as.character(phenoDataFrame[,"SampleName"])
            metabolomicsDataFrame <- read.csv(metabolomicsDataFile,check.names=F,...)
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
          function(Object, metabolomicsDataFile="exprs.csv", ...) {
            ### The input is expected in a SimpleClassificationDataset format
            ### i.e. Along the rows are compounds,
            ### along the columns are samples
            ### The first row contains sample names
            ### The second row contains the class labels
            ### The first column contains Id
            #phenoDataFrame <- read.csv(phenoDataFile)
            #rownames(phenoDataFrame) <- as.character(phenoDataFrame[,"SampleName"])
            inputData <- read.csv(metabolomicsDataFile,check.names=F, ...)
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

setGeneric("univariateCorrelation", def=function(Object, covariate, method)
  standardGeneric("univariateCorrelation"))

setMethod("univariateCorrelation",  signature=c(Object="Dataset", covariate="character", method="missing"),
          valueClass="data.frame",
          definition=function(Object,covariate) {
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
                              "Correlation coefficient (Pearson)"=cors, "P value"=pvals, "BH95 FDR Q value"=qvals))
          })

setMethod("univariateCorrelation",  signature=c(Object="Dataset", covariate="numeric", method="missing"),
          valueClass="data.frame",
          definition=function(Object, covariate) {
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
                              "Correlation coefficient (Pearson)"=cors, "P value"=pvals, "BH95 FDR Q value"=qvals))
          })

setMethod("univariateCorrelation",  signature=c(Object="Dataset", covariate="character", method="character"),
          valueClass="data.frame",
          definition=function(Object,covariate, method) {
            expr <- exprs(Object)
            output <- pData(Object)[,covariate]
            cors <- vector("numeric",nrow(expr))
            pvals <- vector("numeric",nrow(expr))
            for(i in seq(nrow(expr))) {
              ct <- cor.test(expr[i,], output, alternative="t", method=method)
              cors[i] <- ct$estimate
              pvals[i] <- ct$p.value
            }
            qvals <- p.adjust(pvals, method="BH")
	    ret <- data.frame(row.names=featureNames(assayData(Object)), cors, pvals, qvals)
	    colnames(ret) <- c(paste("Correlation coefficient (", method, ")", sep=""), "P value", "BH95 FDR Q value")
            return(ret)
          })

setMethod("univariateCorrelation",  signature=c(Object="Dataset", covariate="numeric", method="character"),
          valueClass="data.frame",
          definition=function(Object, covariate, method) {
            expr <- exprs(Object)
            output <- covariate
            cors <- vector("numeric",nrow(expr))
            pvals <- vector("numeric",nrow(expr))
            for(i in seq(nrow(expr))) {
              ct <- cor.test(expr[i,], output, alternative="t", method=method)
              cors[i] <- ct$estimate
              pvals[i] <- ct$p.value
            }
            qvals <- p.adjust(pvals, method="BH")
	    ret <- data.frame(row.names=featureNames(assayData(Object)), cors, pvals, qvals)
	    colnames(ret) <- c(paste("Correlation coefficient (", method, ")", sep=""), "P value", "BH95 FDR Q value")
            return(ret)
          })

setGeneric("univariateAUC", def=function(Object, covariate) standardGeneric("univariateAUC"))

setMethod("univariateAUC",  signature=c(Object="Dataset", covariate="character"),
          valueClass="numeric", definition=function(Object, covariate) {
            return(rowpAUCs(Object, fac=pData(Object)[,covariate])@AUC)
          })

setGeneric("univariateTTest", def=function(Object, covariate, paired) standardGeneric("univariateTTest"))

setMethod("univariateTTest",  signature=c(Object="Dataset", covariate="character", paired="missing"),
          valueClass="data.frame", definition=function(Object, covariate) {
            res <- t(apply(exprs(Object), 1, function(x) {
              tt <- t.test(x ~ factor(pData(Object)[,covariate]), alternative="two.sided")
              c(tt$estimate, "T statistic"=tt$statistic, "P value"=tt$p.value)
            }))
            qvals <- p.adjust(res[,"P value"], method="BH")
            return(data.frame(res, "BH95 FDR Q value"=qvals))
          })

setMethod("univariateTTest",  signature=c(Object="Dataset", covariate="character", paired="logical"),
          valueClass="data.frame", definition=function(Object, covariate, paired) {
            res <- t(apply(exprs(Object), 1, function(x) {
              tt <- t.test(x ~ factor(pData(Object)[,covariate]), alternative="two.sided", paired=paired)
              c(tt$estimate, "T statistic"=tt$statistic, "P value"=tt$p.value)
            }))
            qvals <- p.adjust(res[,"P value"], method="BH")
            return(data.frame(res, "BH95 FDR Q value"=qvals))
          })

setGeneric("univariateWilcox", def=function(Object, covariate, paired) standardGeneric("univariateWilcox"))

setMethod("univariateWilcox",  signature=c(Object="Dataset", covariate="character", paired="missing"),
          valueClass="data.frame", definition=function(Object, covariate) {
            pvals <- apply(exprs(Object), 1, function(x) {
              wil <- wilcox.test(x ~ factor(pData(Object)[,covariate]), alternative="two.sided")
              wil$p.value
            })
            qvals <- p.adjust(pvals, method="BH")
            medians <- t(apply(exprs(Object), 1, function(x) {
              unlist(lapply(split(x, factor(pData(Object)[,covariate])), median))
            }))
            colnames(medians) <- paste("Median",colnames(medians))
            return(data.frame(medians, "p.value"=pvals, "BH95.FDR.Q.value"=qvals))
          })

setMethod("univariateWilcox",  signature=c(Object="Dataset", covariate="character", paired="logical"),
          valueClass="data.frame", definition=function(Object, covariate, paired) {
            pvals <- apply(exprs(Object), 1, function(x) {
              wil <- wilcox.test(x ~ factor(pData(Object)[,covariate]), alternative="two.sided", paired=paired)
              wil$p.value
            })
            qvals <- p.adjust(pvals, method="BH")
            medians <- t(apply(exprs(Object), 1, function(x) {
              unlist(lapply(split(x, factor(pData(Object)[,covariate])), median))
            }))
            colnames(medians) <- paste("Median",colnames(medians))
            return(data.frame(medians, "p.value"=pvals, "BH95.FDR.Q.value"=qvals))
          })

setGeneric("foldChange", def=function(Object, covariate, paired, is.logged, log.base) standardGeneric("foldChange"))

setMethod("foldChange", signature=c(Object="Dataset", covariate="character", paired="logical", is.logged="logical", log.base="numeric"),
          valueClass="data.frame", definition=function(Object, covariate, paired, is.logged, log.base=2) {
            ### at the moment, it is written to handle binary covariate
            ### improve it for (1) (TODO) multinomial covariate (one-way anova design)
            ### (2) (TODO) two covariates (two-way anova design)
            if(length(levels(factor(pData(Object)[,covariate]))) != 2) {
              stop("Right now this only works with binary covariate\n
                   Consider taking the subset of the data before calling fold change function")
            }
            fcs <- matrix(0, nrow=nrow(Object), ncol=4)
            colnames(fcs) <- c("FC", "FC 95CI.LL", "FC 95CI.UL", "FC 95% CI")
            
            if(is.logged) {
              if(paired) {
                for(i in seq(nrow(Object))) {
                  y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                  pd <- y[[1]] - y[[2]]
                  pdm <- mean(pd)
                  pdse <- sd(pd)/sqrt(length(pd))
                  fcs[i, 1] <- log.base^pdm
                  fcs[i, c(2,3)] <- log.base^(pdm + (c(-1,1)*qnorm(0.975)*pdse))
                }
              } else {
                for(i in seq(nrow(Object))) {
                  y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                  pdm <- mean(y[[1]]) - mean(y[[2]])
                  pd1se <- sd(y[[1]])/sqrt(length(y[[1]]))
                  pd2se <- sd(y[[2]])/sqrt(length(y[[2]]))
                  pdse <- 0.5*(pd1se + pd2se)
                  fcs[i, 1] <- log.base^pdm
                  fcs[i, c(2,3)] <- log.base^(pdm + (c(-1,1)*qnorm(0.975)*pdse))
                }
              }
            } else {
              warning("If your data is not normal, please consider log transformation")
              if(paired) {
                for(i in seq(nrow(Object))) {
                  y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                  pd <- y[[1]] / y[[2]]
                  pdm <- mean(pd)
                  pdse <- sd(pd)/sqrt(length(pd))
                  fcs[i, 1] <- pdm
                  fcs[i, c(2,3)] <- pdm + (c(-1,1)*qnorm(0.975)*pdse)
                }
              } else {
                for(i in seq(nrow(Object))) {
                  y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                  pdm <- mean(y[[1]]) / mean(y[[2]])
                  pd1se <- sd(y[[1]])/sqrt(length(y[[1]]))
                  pd2se <- sd(y[[2]])/sqrt(length(y[[2]]))
                  pdse <- 0.5*(pd1se + pd2se)
                  fcs[i, 1] <- pdm
                  fcs[i, c(2,3)] <- pdm + (c(-1,1)*qnorm(0.975)*pdse)
                }
              }
            }
            fcs[,4] <- paste("(", round(fcs[,2],2), ", ", round(fcs[,3],2),")", sep="")
            fcs
          })

setMethod("foldChange", signature=c(Object="Dataset", covariate="character",
                                    paired="logical", is.logged="missing", log.base="missing"),
          valueClass="data.frame", definition=function(Object, covariate, paired) {
            ### at the moment, it is written to handle binary covariate
            ### improve it for (1) (TODO) multinomial covariate (one-way anova design)
            ### (2) (TODO) two covariates (two-way anova design)
            if(length(levels(factor(pData(Object)[,covariate]))) != 2) {
              stop("Right now this only works with binary covariate\n
                   Consider taking the subset of the data before calling fold change function")
            }
            fcs <- matrix(0, nrow=nrow(Object), ncol=4)
            colnames(fcs) <- c("FC", "FC 95CI.LL", "FC 95CI.UL", "FC 95% CI")
            
            warning("If your data is not normal, please consider log transformation")
            if(paired) {
              for(i in seq(nrow(Object))) {
                y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                pd <- y[[1]] / y[[2]]
                pdm <- mean(pd)
                pdse <- sd(pd)/sqrt(length(pd))
                fcs[i, 1] <- pdm
                fcs[i, c(2,3)] <- pdm + (c(-1,1)*qnorm(0.975)*pdse)
              }
            } else {
              for(i in seq(nrow(Object))) {
                y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                pdm <- mean(y[[1]]) / mean(y[[2]])
                pd1se <- sd(y[[1]])/sqrt(length(y[[1]]))
                pd2se <- sd(y[[2]])/sqrt(length(y[[2]]))
                pdse <- 0.5*(pd1se + pd2se)
                fcs[i, 1] <- pdm
                fcs[i, c(2,3)] <- pdm + (c(-1,1)*qnorm(0.975)*pdse)
              }
            }
            fcs[,4] <- paste("(", round(fcs[,2],2), ", ", round(fcs[,3],2),")", sep="")
            fcs
          })

setMethod("foldChange", signature=c(Object="Dataset", covariate="character",
                                    paired="missing", is.logged="logical", log.base="numeric"),
          valueClass="data.frame", definition=function(Object, covariate, is.logged, log.base=2) {
            ### at the moment, it is written to handle binary covariate
            ### improve it for (1) (TODO) multinomial covariate (one-way anova design)
            ### (2) (TODO) two covariates (two-way anova design)
            if(length(levels(factor(pData(Object)[,covariate]))) != 2) {
              stop("Right now this only works with binary covariate\n
                   Consider taking the subset of the data before calling fold change function")
            }
            fcs <- matrix(0, nrow=nrow(Object), ncol=4)
            colnames(fcs) <- c("FC", "FC 95CI.LL", "FC 95CI.UL", "FC 95% CI")
            
            if(is.logged) {
              for(i in seq(nrow(Object))) {
                y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                pdm <- mean(y[[1]]) - mean(y[[2]])
                pd1se <- sd(y[[1]])/sqrt(length(y[[1]]))
                pd2se <- sd(y[[2]])/sqrt(length(y[[2]]))
                pdse <- 0.5*(pd1se + pd2se)
                fcs[i, 1] <- log.base^pdm
                fcs[i, c(2,3)] <- log.base^(pdm + (c(-1,1)*qnorm(0.975)*pdse))
              }
            } else {
              warning("If your data is not normal, please consider log transformation")
              for(i in seq(nrow(Object))) {
                y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
                pdm <- mean(y[[1]]) / mean(y[[2]])
                pd1se <- sd(y[[1]])/sqrt(length(y[[1]]))
                pd2se <- sd(y[[2]])/sqrt(length(y[[2]]))
                pdse <- 0.5*(pd1se + pd2se)
                fcs[i, 1] <- pdm
                fcs[i, c(2,3)] <- pdm + (c(-1,1)*qnorm(0.975)*pdse)
              }
            }
            fcs[,4] <- paste("(", round(fcs[,2],2), ", ", round(fcs[,3],2),")", sep="")
            fcs
          })

setMethod("foldChange", signature=c(Object="Dataset", covariate="character",
                                    paired="missing", is.logged="missing", log.base="missing"),
          valueClass="data.frame", definition=function(Object, covariate) {
            ### at the moment, it is written to handle binary covariate
            ### improve it for (1) (TODO) multinomial covariate (one-way anova design)
            ### (2) (TODO) two covariates (two-way anova design)
            if(length(levels(factor(pData(Object)[,covariate]))) != 2) {
              stop("Right now this only works with binary covariate\n
                   Consider taking the subset of the data before calling fold change function")
            }
            fcs <- matrix(0, nrow=nrow(Object), ncol=4)
            colnames(fcs) <- c("FC", "FC 95CI.LL", "FC 95CI.UL", "FC 95% CI")
            
            warning("If your data is not normal, please consider log transformation")
            for(i in seq(nrow(Object))) {
              y <- split(exprs(Object)[i,], factor(pData(Object)[,covariate]))
              pdm <- mean(y[[1]]) / mean(y[[2]])
              pd1se <- sd(y[[1]])/sqrt(length(y[[1]]))
              pd2se <- sd(y[[2]])/sqrt(length(y[[2]]))
              pdse <- 0.5*(pd1se + pd2se)
              fcs[i, 1] <- pdm
              fcs[i, c(2,3)] <- pdm + (c(-1,1)*qnorm(0.975)*pdse)
            }
            
            fcs[,4] <- paste("(", round(fcs[,2],2), ", ", round(fcs[,3],2),")", sep="")
            fcs
          })

setGeneric("univariateOddsRatio",
           def=function(Object, covariate, given) standardGeneric("univariateOddsRatio"))

setMethod("univariateOddsRatio", signature=c(Object="Dataset", covariate="character", given="missing"),
          valueClass="data.frame", definition=function(Object, covariate) {

            ors <- matrix(0, nrow=nrow(Object), ncol=6)
            rownames(ors) <- featureNames(Object)
            colnames(ors) <- c("OR", "OR 95CI.LL", "OR 95CI.UL", "OR 95% CI", "P value (H0: OR=1)", "FDR Q-value")
            
            for(j in seq(nrow(Object))) {
                glm1 <- glm(y ~ .,
                            data=data.frame("x" = exprs(Object)[j,],
                                            "y" = factor(pData(Object)[,covariate])),
                            family="binomial")
                
                ors[j, 1] <- signif(exp(summary(glm1)[["coefficients"]]["x", "Estimate"]),4)
                ors[j, 5] <- signif(summary(glm1)[["coefficients"]]["x", "Pr(>|z|)"],4)
                ors[j, c(2,3)] <- signif(exp(summary(glm1)[["coefficients"]]["x", "Estimate"] +
                  c(-1,1) * qnorm(0.975) * summary(glm1)[["coefficients"]]["x", "Std. Error"]),4)
              }
            ors[,4] <- paste("(", round(ors[,2],2), ", ", round(ors[,3],2),")", sep="")
            ors[,6] <- p.adjust(ors[,5], method="BH")
            ors
          })        

setMethod("univariateOddsRatio",
          signature=c(Object="Dataset", covariate="character", given="character"),
          valueClass="data.frame", definition=function(Object, covariate, given) {

            ors <- matrix(0, nrow=nrow(Object), ncol=6)
            rownames(ors) <- featureNames(Object)
            colnames(ors) <- c("OR", "OR 95CI.LL", "OR 95CI.UL", "OR 95% CI", "P value (H0: OR=1)", "FDR Q-value")
            
            given <- given[given %in% varLabels(phenoData(Object))]
            if(length(given) <= 0) {
              stop(paste("Argument `given` did not match any of the feature names of the dataset."))
            } else {
              given.ors <- matrix(0, nrow=nrow(Object), ncol=length(given)*2)
              rownames(given.ors) <- featureNames(Object)
              colnames(given.ors) <- paste(rep(given, each=2),
                                           rep(c("OR", "P val"), times=length(given)))
            }
            
            for(j in seq(nrow(Object))) {
              if(length(given) == 1) {
                dat <- data.frame(exprs(Object)[j,],
                                  as.numeric(pData(Object)[,given]),
                                  factor(pData(Object)[,covariate]))
                colnames(dat) <- c("x", given, "y")
              } else if(length(given) > 1) {
                dat <- data.frame(exprs(Object)[j,],
                                  apply(pData(Object)[,given], 2, as.numeric),
                                  factor(pData(Object)[,covariate]))
                colnames(dat) <- c("x", given, "y")
              }
              
              glm1 <- glm(y ~ ., data=dat, family="binomial")
              
              coefs <- summary(glm1)[["coefficients"]]
              rownames(coefs) <- gsub("`","",rownames(coefs))
              ors[j, 1] <- signif(exp(coefs["x", "Estimate"]),4)
              ors[j, 5] <- signif(coefs["x", "Pr(>|z|)"],4)
              ors[j, c(2,3)] <- signif(exp(coefs["x", "Estimate"] +
                c(-1,1) * qnorm(0.975) * coefs["x", "Std. Error"]),4)
              
              # c(rbind(x,y)) is a trick to interlace elements of x and y
              given.ors[j,] <- signif(c(rbind(exp(coefs[given, "Estimate"]),
                                       coefs[given, "Pr(>|z|)"])),4)
            }
            ors[,4] <- paste("(", round(ors[,2],2), ", ", round(ors[,3],2),")", sep="")
            ors[,6] <- p.adjust(ors[,5], method="BH")
            data.frame(ors, given.ors, check.names=F)
          })
            
setGeneric("rankNormalization", function(Object) standardGeneric("rankNormalization"))

setMethod("rankNormalization", signature="Dataset",
          function(Object) {
            exprs(Object) <- apply(exprs(Object), 2, rank)
            Object
          })

setMethod("log2", signature=c("Dataset"),
          function(x) {
            x2 <- x
            exprs(x2) <- log2(exprs(x2))
            x2
          })

setMethod("log10", signature=c("Dataset"),
          function(x) {
            x2 <- x
            exprs(x2) <- log10(exprs(x2))
            x2
          })

setMethod("log", signature=c("Dataset"),
          function(x, base=exp(1)) {
            x2 <- x
            exprs(x2) <- log(exprs(x2), base=base)
            x2
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
