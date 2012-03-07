vsglmnet <- setRefClass("LogisticNetVariableSelection", fileds=list(
	alpha = "numeric",
	lambda = "vector",
	mostFrequentModel = "vector",
	allSelectedModels = "list"),
	contains="VariableSelection")

vsglmnet$methods(list(
	initialize = function(...) {
		createMe(.self, ...)
	},

	optimizeParameters = function() {
		alphas <- seq(0.1,1,by=0.1)
		aucs <- rep(0, length(alphas))
		lambdas <- rep(0, length(alphas))
		cv.model <- vector("list", length(alphas))
		for(i in seq(length(alphas))) {
			cv.model[[i]] <- cv.glmnet(.self$x.train, .self$y.train, family="binomial", alpha=alphas[i], nfolds=10, type.measure="auc")
			aucs[i] <- cv.model[[i]]$cvm[which(cv.model[[i]]$lambda == cv.model[[i]]$lambda.min)]
			lambdas[i] <- cv.model[[i]]$lambda.min
		}
		.self$alpha <- alphas[which.max(aucs)]
		.self$lambda <- cv.model[[which.max(aucs)]]$lambda
		plot(cv.model[[which.max(aucs)]])
	},

	searchVariables = function(cv=1000, cv.threshold=0.6, clus=NULL) {
		## the arguments have to be betteter integrated into class definition
		## For example, the data (x.train and y.train etc) should be provided as "Dataset" objects
		## and "clus" should come inside the "Dataset" objects (using... AnnotatedDataFrame)

		cv.model <- vector("list", length=cv)
		vars <- list()
		for(j in seq(cv)) {
			test.idx <- sample(nrow(x))[1:round( 1/3 * nrow(x) )]
			if(!is.null(clus)) {
				rsamp <- unlist(tapply(colnames(x),clus,function(zz) {sample(zz,size=k)}))
			} else {
				rsamp <- colnames(x)
			}

			cv.model[[j]] <- glmnet(.self$x.train[-test.idx, rsamp],y[-test.idx], family="binomial", alpha=.self$alpha, lambda=.self$lambda)
			pre <- predict(cv.model[[j]], newx=x[test.idx,rsamp], s="lambda.min")
			auc.j[j] <- roc(y[test.idx], pre[,1])$auc[[1]]
			if(auc.j[j] >= cv.threshold) {
				tmp <- coef(cv.model[[j]], s="lambda.min")[-1,1]
				vars[[length(vars)+1]] <- sort(names(tmp[tmp!=0]))
			}
		}
		modelCounts <- lapply(lapply(vars, function(x){lapply(vars,function(y){identical(x,y)})}), function(z){length(which(unlist(z)))})
		.self$allSelectedModels <- list("vars"=vars, "counts"=modelCounts)
		.self$mostFrequentModel <- vars[[which.max(unlist(modelCounts))]]
	},
	
	printSelectedVarsTable = function(fileName) {
		newTable <- vector()
		for(i in seq(length(.self$allSelectedModels[["vars"]]))) {
			newTable[i] <- paste(.self$allSelectedModels[["vars"]][[i]], collapse=", ")
		}
		write.csv(count(newTable), file=fileName)
	}

	))

setGeneric("createMe", def=function(object, x.train, y.train))
