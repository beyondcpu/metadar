cc <- setRefClass("ClassifierComparison", fields=list(
		classifiers="list", ### list of Classifier objects
		name="character"))

cc$methods(list(
		 initialize = function(classifiers) {
			 .self$classifiers <- classifiers
		 },

		 setName = function(name) {
			 .self$name <- name
		 },

		 plotroclist = function() {
			 legends <- vector("character")
			 for(i in seq(length(classifiers))) {
				 plot(classifiers[[i]]$roc, add=ifelse(i==1,F,T), lwd=3, print.thres=classifiers[[i]]$youdenIndex,
						print.thres.pch=pch, print.thres.col=as.character(i), print.thres.cex=2.2, col=as.character(i))
				 legends[i] <- classifiers[[i]]$name
			 }

			 legend(legpos[1], legpos[2], legends, col=colors, lwd=3)
			 title("ROC curve")
		 }
))