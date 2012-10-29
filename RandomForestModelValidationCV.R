lrm <- setRefClass("RandomForestModelValidationCV",
	contains="ModelValidationCV")

lrm$methods(list(
	initialize = function(...) {
		callSuper(...)
	},

	buildCV = function() { # public
		orig <- matrix(0, nrow=round(ncol(.self$x)/3), ncol=.self$cv)
		pred <- matrix(0, nrow=round(ncol(.self$x)/3), ncol=.self$cv)
		for(i in seq(.self$cv)) {
			tst <- sample(ncol(.self$x), round(ncol(.self$x)/3))
			logM <- randomForest(x=t(x[,-tst]), y=y[-tst])
			pred[,i] <- predict(logM, newdata=data.frame(t(x[,tst]), check.names=F),type="prob")[,2]
			orig[,i] <- y[tst]
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
		logM <- randomForest(x=t(x), y=y)
		orig <- matrix(0, nrow=round(ncol(.self$x.test)/3), ncol=.self$cv)
		pred <- matrix(0, nrow=round(ncol(.self$x.test)/3), ncol=.self$cv)
		for(i in seq(.self$cv)) {
			tst <- sample(ncol(.self$x.test), round(ncol(.self$x.test)/3))
			pred[,i] <- predict(logM, newdata=data.frame(t(x.test[,tst]), check.names=F),type="prob")[,2]
			orig[,i] <- y.test[tst]
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

#setGeneric("createMe", def=function(object, x, y, x.test, y.test, cv)
#    standardGeneric("createMe"))
    
#setMethod("createMe", signature=c("RandomForestModelValidationCV", "missing", "missing", "missing", "missing", "missing"),
#    function(object) {
#      object
#    })
  
#setMethod("createMe", signature=c("RandomForestModelValidationCV", "data.frame", "factor", "missing", "missing", "numeric"),
#    function(object, x, y, cv) {
#      object$x <- x
#      object$y <- y
#      object$cv <- cv
#      object
#    })

#setMethod("createMe", signature=c("RandomForestModelValidationCV", "data.frame", "factor", "data.frame", "factor", "numeric"),
#    function(object, x, y, x.test, y.test, cv) {
#      object$x <- x
#      object$y <- y
#      object$x.test <- x.test
#      object$y.test <- y.test
#      object$cv <- cv
#      object
#    })
