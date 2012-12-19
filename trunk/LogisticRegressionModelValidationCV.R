lrm <- setRefClass("LogisticRegressionModelValidationCV",
	contains="ModelValidationCV")
lrm$methods(list(
	initialize = function(...) {
		#createMe(.self, ...)
    callSuper(...)
	},
  
  full.training.model = function(y, x, ...)
  {
    #browser()
    if(is.null(dim(x))) {
  		if(!identical(names(y), names(x)))
  			stop("Error names(x) must be equal to names(y)")
  	} else if(!identical(names(y),rownames(x))) stop("Error: rownames(x) must be equal to names(y)")
  
  	yx <- data.frame("y"=factor(y), x)
  	if(!is.null(dim(x)))
  		colnames(yx)[2:ncol(yx)] <- colnames(x)
  	yx.glm <- glm(y~., family="binomial", data=yx, ...)
  	return(yx.glm)
  },

	buildCV = function() { # public
		orig <- matrix(0, nrow=round(ncol(.self$x)/3), ncol=.self$cv)
		pred <- matrix(0, nrow=round(ncol(.self$x)/3), ncol=.self$cv)
		for(i in seq(.self$cv)) {
			tst <- sample(ncol(.self$x), round(ncol(.self$x)/3))
			logM <- full.training.model(x=t(.self$x[,-tst]), y=.self$y[-tst], maxit=50)
			pred[,i] <- predict(logM, newdata=data.frame(t(.self$x[,tst]), check.names=F),type="response")
			orig[,i] <- .self$y[tst]
		}
		.self$pred.test <- prediction(pred, orig)
    
		.self$computeCVcutoff()
		.self$computeCVacc()
		.self$computeCVauc()
		.self$computeCVor()
		.self$computeCVsens()
		.self$computeCVspec()
	},

	buildCV2 = function() { # public
		logM <- full.training.model(x=t(.self$x), y=.self$y, maxit=50)
		orig <- matrix(0, nrow=round(ncol(.self$x.test)/3), ncol=.self$cv)
		pred <- matrix(0, nrow=round(ncol(.self$x.test)/3), ncol=.self$cv)
		for(i in seq(.self$cv)) {
			tst <- sample(ncol(.self$x.test), round(ncol(.self$x.test)/3))
			pred[,i] <- predict(logM, newdata=data.frame(t(.self$x.test[,tst]), check.names=F),type="response")
			orig[,i] <- .self$y.test[tst]
		}
		.self$pred.test <- prediction(pred, orig)

		.self$computeCVcutoff()
		.self$computeCVacc()
		.self$computeCVauc()
		.self$computeCVor()
		.self$computeCVsens()
		.self$computeCVspec()
	}
))