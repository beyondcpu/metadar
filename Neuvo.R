neu <- setRefClass("Neuvo", fields=list(
		   x.train = "matrix",
		   y.train = "factor",
		   M = "numeric", # number of models
			K = "numeric", # number of variables
			P = "matrix", # mutual value of model and variable
			population = "list",
			mAuc = "numeric", # ranking for models
			kAuc = "numeric")) # univariate ranking for variables

neu$methods(list(
		initialize = function(...) {
			createNeuvo(.self, ...)
		},

		initializeModels = function() {
			varIds <- matrix(sample(rownames(.self$x.train), size=M*2, replace=T), ncol=2)
		},

		addVariables = function() {
		},

		removeVariables = function() {
		},

		addModels = function() {
		},

		removeModels = function() {
		}




		))


setGeneric("createNeuvo", def=function(object, x, y)
    standardGeneric("createClassifier"))
    
setMethod("createNeuvo", signature=c("Neuvo", "missing", "missing"),
    function(object) {
      object
    })
  
setMethod("createNeuvo", signature=c("Neuvo", "data.frame", "factor"),
    function(object, x, y) {
      object$x.train <- x
      object$y.train <- y
      object
    })

setMethod("createNeuvo", signature=c("Neuvo", "character", "missing"),
	  function(object, x) {
		  inputData <- read.csv(x,check.names=F)
		  dat <- inputData[-1, -c(1,2)]
		  rownames(dat) <- inputData[-1,"Id"]
		  .self$x.train <- apply(dat,2,as.numeric)
		  rownames(.self$x.train) <- rownames(dat)
		  .self$y.train <- factor(unlist(inputData[1,3:ncol(inputData)]))
	  })
