
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

