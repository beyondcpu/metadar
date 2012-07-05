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
