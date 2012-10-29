lrm <- setRefClass("ModelValidationCV", fields=list(
		x = "data.frame",
		y = "factor",
		x.test = "data.frame",
		y.test = "factor",
		cv = "numeric",
		pred.test = "prediction",
		cv.auc = "numeric",
		cv.or = "numeric",
		cv.acc = "numeric",
		cv.cutoff = "numeric",
		cv.sens = "numeric",
		cv.spec = "numeric",
		auc.quantiles = "numeric",
		or.quantiles = "numeric",
		acc.quantiles = "numeric",
		cutoff.quantiles = "numeric",
		sens.quantiles = "numeric",
		spec.quantiles = "numeric",
		auc.mean = "numeric",
		or.mean = "numeric",
		acc.mean = "numeric",
		cutoff.mean = "numeric",
		sens.mean = "numeric",
		spec.mean = "numeric"))
lrm$methods(list(
	initialize = function(...) {
		createMe(.self, ...)
	},

	setTrainData = function(x, y) {
		.self$x <- x
		.self$y <- y
	},

	setTestData = function(x.test, y.test) {
		.self$x.test <- x.test
		.self$y.test <- y.test
	},

	setCV = function(cv) {
		.self$cv <- cv
	},

	buildCV = function() { # abstract
	},
	
	buildCV2 = function() { # abstract
		# when there is an independent test data set
		# build the model using whole training
		# and show the cross-validation performance
		# of this model on test data

		# Hint: Under the setting x = x.test, and y = y.test
		# you get the confidence limits of the full training model
		# on subsets!!
	},

	computeCVcutoff = function() { # private
    perf.test <- performance(.self$pred.test, measure="sens", x.measure="spec")
		
		.self$cv.cutoff <- rep(0,.self$cv)
		for(i in seq(.self$cv)) {
			.self$cv.cutoff[i] <- perf.test@alpha.values[[i]][which.max((perf.test@x.values[[i]]*perf.test@y.values[[i]])/(perf.test@x.values[[i]]+perf.test@y.values[[i]]))]
		}
		.self$cv.cutoff <- .self$cv.cutoff[.self$cv.cutoff!=Inf]
		.self$cv.cutoff <- .self$cv.cutoff[!is.na(.self$cv.cutoff)]
		.self$cutoff.mean <- mean(.self$cv.cutoff)
		.self$cutoff.quantiles <- quantile(.self$cv.cutoff, c(0.05, 0.25, 0.5, 0.75, 0.95))
	},

	computeCVacc = function() { # private
		acc <- performance(.self$pred.test, measure="acc")
		.self$cv.acc <- rep(0,.self$cv)
		for(i in seq(.self$cv)) {
			.self$cv.acc[i] <- acc@y.values[[i]][which.min(abs(acc@x.values[[i]]-.self$cutoff.mean))]
		}
		.self$cv.acc <- .self$cv.acc[.self$cv.acc!=Inf]
		.self$cv.acc <- .self$cv.acc[!is.na(.self$cv.acc)]
		.self$acc.mean <- mean(.self$cv.acc)
		.self$acc.quantiles <- quantile(.self$cv.acc, c(0.05, 0.25, 0.5, 0.75, 0.95))
	},

	computeCVauc = function() { #private
		auc <- performance(.self$pred.test, measure="auc")
		.self$cv.auc <- rep(0, .self$cv)
		for(i in seq(.self$cv)) {
			.self$cv.auc[i] <- auc@y.values[[i]][1]
		}
		.self$cv.auc <- .self$cv.auc[.self$cv.auc!=Inf]
		.self$cv.auc <- .self$cv.auc[!is.na(.self$cv.auc)]
		.self$auc.mean <- mean(.self$cv.auc)
		.self$auc.quantiles <- quantile(.self$cv.auc, c(0.05, 0.25, 0.5, 0.75, 0.95))
	},

	computeCVor = function() { # private
		or <- performance(.self$pred.test, measure="odds")
		.self$cv.or <- rep(0,.self$cv)
		for(i in seq(.self$cv)) {
			.self$cv.or[i] <- or@y.values[[i]][which.min(abs(or@x.values[[i]]-.self$cutoff.mean))]
		}
		.self$cv.or <- .self$cv.or[.self$cv.or!=Inf]
		.self$cv.or <- .self$cv.or[!is.na(.self$cv.or)]
		.self$or.mean <- mean(.self$cv.or)
		.self$or.quantiles <- quantile(.self$cv.or, c(0.05, 0.25, 0.5, 0.75, 0.95))
	},

	computeCVsens = function() { # private
		sens <- performance(.self$pred.test, measure="sens")
		.self$cv.sens <- rep(0,.self$cv)
		for(i in seq(.self$cv)) {
			.self$cv.sens[i] <- sens@y.values[[i]][which.min(abs(sens@x.values[[i]]-.self$cutoff.mean))]
		}
		.self$cv.sens <- .self$cv.sens[.self$cv.sens!=Inf]
		.self$cv.sens <- .self$cv.sens[!is.na(.self$cv.sens)]
		.self$sens.mean <- mean(.self$cv.sens)
		.self$sens.quantiles <- quantile(.self$cv.sens, c(0.05, 0.25, 0.5, 0.75, 0.95))
	},

	computeCVspec = function() { # private
		spec <- performance(.self$pred.test, measure="spec")
		.self$cv.spec <- rep(0,.self$cv)
		for(i in seq(.self$cv)) {
			.self$cv.spec[i] <- spec@y.values[[i]][which.min(abs(spec@x.values[[i]]-.self$cutoff.mean))]
		}
		.self$cv.spec <- .self$cv.spec[.self$cv.spec!=Inf]
		.self$cv.spec <- .self$cv.spec[!is.na(.self$cv.spec)]
		.self$spec.mean <- mean(.self$cv.spec)
		.self$spec.quantiles <- quantile(.self$cv.spec, c(0.05, 0.25, 0.5, 0.75, 0.95))
	},

	plotROC = function(title, leg.pos.x=0.3, leg.pos.y=0.25, legend.cex=1.5, showlegend=TRUE) {
		perf.test <- performance(.self$pred.test, measure="tpr", x.measure="fpr")
		plot(perf.test, avg="vertical", spread.estimate="boxplot", main=title)
		abline(a=0, b=1)
		if(showlegend) {
			legend("bottomright", legend=c(paste("AUC = ", round(.self$auc.mean, 3), " (", round(.self$auc.quantiles[1],3), ", ", round(.self$auc.quantiles[5],3), ")", sep=""),
					  paste("ACC = ", round(.self$acc.mean,3), " (", round(.self$acc.quantiles[1],3), ", ", round(.self$acc.quantiles[5],3), ")", sep=""),
					  paste("OR = ", round(.self$or.mean,3), " (", round(.self$or.quantiles[1],3), ", ", round(.self$or.quantiles[5],3), ")", sep="")), box.lwd=0, cex=legend.cex)
		}
	},

	printStats = function(file.name) {
		op <- data.frame("CutOff"=cutoff.quantiles, "AUC"=auc.quantiles, "Odds Ratios"=or.quantiles, "Accuracy"=acc.quantiles, "Sensitivity"=sens.quantiles, "Specificity"=spec.quantiles)
		op <- rbind(op, "mean"=c(cutoff.mean, auc.mean, or.mean, acc.mean, sens.mean, spec.mean))
		write.csv(op, file=file.name)
	}
))

setGeneric("createMe", def=function(object, x, y, x.test, y.test, cv, selectedVariables)
    standardGeneric("createMe"))
    
setMethod("createMe", signature=c("ModelValidationCV", "missing", "missing", "missing", "missing", "missing", "missing"),
    function(object) {
      object
    })
  
setMethod("createMe", signature=c("ModelValidationCV", "data.frame", "factor", "missing", "missing", "numeric", "character"),
    function(object, x, y, cv, selectedVariables) {
      object$x <- data.frame(x[,selectedVariables],check.names=F)
      object$y <- y
      object$cv <- cv
	  if(length(selectedVariables) == 1) {
		object$x <- data.frame(t(object$x), check.names=F)
		rownames(object$x) <- selectedVariables
	  }
      object
    })

setMethod("createMe", signature=c("ModelValidationCV", "data.frame", "factor", "data.frame", "factor", "numeric", "character"),
    function(object, x, y, x.test, y.test, cv, selectedVariables) {
      object$x <- data.frame(x[,selectedVariables],check.names=F)
      object$y <- y
      object$x.test <- data.frame(x.test[,selectedVariables],check.names=F)
      object$y.test <- y.test
      object$cv <- cv
	  if(length(selectedVariables) == 1) {
		object$x <- data.frame(t(object$x),check.names=F)
		rownames(object$x) <- selectedVariables
		object$x.test <- data.frame(t(object$x.test), check.names=F)
		rownames(object$x.test) <- selectedVariables
	  }
      object
    })

setMethod("createMe", signature=c("ModelValidationCV", "Dataset", "character", "missing", "missing", "numeric", "character"),
    function(object, x, y, cv, selectedVariables) {
      object$x <- data.frame(exprs(x)[selectedVariables,], check.names=F)
      object$y <- factor(pData(x)[,y])
      names(object$y) <- sampleNames(x)
      object$cv <- cv
	  if(length(selectedVariables) == 1) {
		object$x <- data.frame(t(object$x), check.names=F)
		rownames(object$x) <- selectedVariables
	  }
      object
    })

setMethod("createMe", signature=c("ModelValidationCV", "Dataset", "character", "Dataset", "character", "numeric"),
    function(object, x, y, x.test, y.test, cv, selectedVariables) {
      object$x <- data.frame(exprs(x)[selectedVariables,], check.names=F)
      object$y <- factor(pData(x)[,y])
      names(object$y) <- sampleNames(x)
      object$cv <- cv
      object$x.test <- data.frame(exprs(x.test)[selectedVariables,], check.names=F)
      object$y.test <- factor(pData(x.test)[,y.test])
      names(object$y.test) <- sampleNames(x.test)
	  if(length(selectedVariables) == 1) {
		object$x <- data.frame(t(object$x), check.names=F)
		rownames(object$x) <- selectedVariables
		object$x.test <- data.frame(t(object$x.test), check.names=F)
		rownames(object$x.test) <- selectedVariables
	  }
      object
    })
