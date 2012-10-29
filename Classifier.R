cla <- setRefClass("Classifier", fields=list(
					     x = "data.frame",
					     y = "factor",
					     x.test = "data.frame",
					     y.test = "factor",
					     predicted.class = "factor",
					     predicted.value = "numeric",
					     optimalCutoff = "numeric",
					     sens = "numeric",#based on youden index
					     spec = "numeric",#based on youden index
					     rr = "numeric",
					     or = "numeric",
					     rr.l = "numeric",
					     rr.u = "numeric",
					     or.l = "numeric",
					     or.u = "numeric",
					     auc = "numeric",
					     auc.l = "numeric",
					     auc.u = "numeric",
					     contingency = "table",
					     model = "ANY",
					     name = "character",
					     roc = "list" # list of class "roc" from pROC package
					     ))

### AUC, CI(AUC), p-value of difference between AUC
### sensitivity, specificity, relative risk, odds ratio (ci of all)
###

cla$methods(list(
		 initialize = function(...) {
			 createClassifier(.self,...)
		 },

		 setName = function(name) {
			 .self$name <- name
		 },

		 buildClassifier = function() {
			 ### this is the method which all the inherited classes
			 ### should implement
			 callNextMethod()
		 },

		 predict = function() {
			 callNextMethod()
		 },

		 getROC = function() {
			 .self$roc[[1]]
		 },

		 computeROC = function() {
			 .self$roc[[1]] <- roc(y, predicted.value)
		 },
     
     plotROC = function(...) {
       plot(getROC(), print.auc=T, print.thres=optimalCutoff, ...)
     },

		 computeAUC = function() {
			 CI <- ci(.self$getROC())
			 .self$auc.l <- CI[[1]]
			 .self$auc <- CI[[2]]
			 .self$auc.u <- CI[[3]]
		 },

		 computeoptimalCutoff = function() {
			 .self$optimalCutoff <- .self$getROC()$thresholds[which.max((.self$getROC()$sensitivities * .self$getROC()$specificities)/(.self$getROC()$sensitivities + .self$getROC()$specificities))]
		 },

		 computeSensitivity = function() {
			 ## Sensitivity at the Youden's Index
			 .self$sens <-  .self$getROC()$sensitivities[which.max((.self$getROC()$sensitivities * .self$getROC()$specificities)/(.self$getROC()$sensitivities + .self$getROC()$specificities))]
		 },

		 computeSpecificity = function() {
			 ## Specificity at the Youden's Index
			 .self$spec <- .self$getROC()$specificities[which.max((.self$getROC()$sensitivities * .self$getROC()$specificities)/(.self$getROC()$sensitivities + .self$getROC()$specificities))]
		 },
     
     printStatistics = function() {
       print(data.frame("AUC CIL"=auc.l, "AUC"=auc, "AUC CIU"=auc.u,
                        "Optimal cutoff"=optimalCutoff,
                        "Sensitivity"=sens, "Specificity"=spec))
     }
		 ))

setGeneric("createClassifier", def=function(object, x, y, x.test, y.test, selectedVariables)
	   standardGeneric("createClassifier"))

setMethod("createClassifier", signature=c("Classifier", "missing", "missing", "missing", "missing", "missing"),
	  function(object) {
		  object
	  })

# setMethod("createClassifier", signature=c("Classifier", "data.frame", "factor", "missing", "missing"),
# 	  function(object, x, y) {
# 		  object$x <- x
# 		  object$y <- y
# 		  object
# 	  })
# 
# setMethod("createClassifier", signature=c("Classifier", "data.frame", "factor", "data.frame", "factor"),
# 	  function(object, x, y, x.test, y.test) {
# 		  object$x <- x
# 		  object$y <- y
# 		  object$x.test <- x.test
# 		  object$y.test <- y.test
# 		  object
# 	  })
# 
# setMethod("createClassifier", signature=c("Classifier", "character", "missing", "missing", "missing"),
# 	  function(object, x) {
# 		  inputData <- read.csv(x,check.names=F)
# 		  dat <- inputData[-1, -1] ### if you have cluster column, excluse also that column: e.g. -c(1,2)
# 		  rownames(dat) <- inputData[-1,"Id"]
# 		  object$x.train <- t(apply(dat,2,as.numeric))
# 		  colnames(object$x.train) <- rownames(dat)
# 		  rownames(object$x.train) <- colnames(dat)
# 		  object$y.train <- factor(unlist(inputData[1,2:ncol(inputData)])) ### if you have cluster column, use 3:ncol(inputData) instead
# 		  object
# 	  })
# 
# setMethod("createClassifier", signature=c("Classifier", "character", "missing", "character", "missing"),
# 	  function(object, x, x.test) {
# 		  inputData1 <- read.csv(x,check.names=F)
# 		  dat1 <- inputData1[-1, -1] ### if you have cluster column, excluse also that column: e.g. -c(1,2)
# 		  rownames(dat1) <- inputData1[-1,"Id"]
# 		  object$x <- t(apply(dat1,2,as.numeric))
# 		  colnames(object$x) <- rownames(dat1)
# 		  rownames(object$x) <- colnames(dat1)
# 		  object$y <- factor(unlist(inputData1[1,2:ncol(inputData1)])) ### if you have cluster column, use 3:ncol(inputData) instead
# 
# 		  inputData2 <- read.csv(x.test,check.names=F)
# 		  dat2 <- inputData2[-1, -1] ### if you have cluster column, excluse also that column: e.g. -c(1,2)
# 		  rownames(dat2) <- inputData2[-1,"Id"]
# 		  object$x.test <- t(apply(dat2,2,as.numeric))
# 		  colnames(object$x.test) <- rownames(dat2)
# 		  rownames(object$x.test) <- colnames(dat2)
# 		  object$y.test <- factor(unlist(inputData2[1,2:ncol(inputData2)])) ### if you have cluster column, use 3:ncol(inputData) instead
# 
# 		  object
# 	  })

setMethod("createClassifier", signature=c("Classifier", "Dataset", "character", "missing", "missing", "character"),          
      function(object, x, y, selectedVariables) {
        object$x <- data.frame(exprs(x)[selectedVariables,], check.names=F)
        object$y <- factor(pData(x)[,y])
        names(object$y) <- sampleNames(x)
        object
      })

setMethod("createClassifier", signature=c("Classifier", "Dataset", "character", "Dataset", "character", "character"),          
      function(object, x, y, x.test, y.test, selectedVariables) {
        object$x <- data.frame(exprs(x)[selectedVariables,], check.names=F)
        object$y <- factor(pData(x)[,y])
        names(object$y) <- sampleNames(x)
        object$x.test <- data.frame(exprs(x.test)[selectedVariables,], check.names=F)
        object$y.test <- factor(pData(x.test)[,y.test])
        names(object$y.test) <- sampleNames(x.test)
        object
      })
