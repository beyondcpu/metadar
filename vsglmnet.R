vsglmnet <- function(y, x, clus=NULL, k=1, cv=10, nvars=4, alphas=seq(0.1,1,by=0.1), lambdas=10^seq(from=5, to=-10, by=-0.5), family="gaussian")
{
	require("glmnet")
	require("plyr")
	if(!is.matrix(x)) stop("Error: x must be a matrix!")
	if(family=="binomial") {
		if(!is.factor(y)) stop("Error: y must be a factor!")
	}
	if(!identical(names(y),rownames(x))) stop("Error: rownames(x) must be equal to names(y)")

	multivarsByAlpha <- vector("list", length=length(alphas))
	univarsByAlpha <- vector("list", length=length(alphas))
	emptyModelsByAlpha <- rep(0, times=length(alphas))
  mseByAlpha <- vector("numeric", length=length(alphas))

	for(i in seq(length(alphas)))
	{
		vars.j <- rep("", length=cv)
		allVars <- c()
    mse.j <- vector("numeric", length=cv)
		
		for(j in seq(cv)) {
			test.idx <- sample(nrow(x))[1:round( 1/5 * nrow(x) )]

			if(!is.null(clus)) {
				rsamp <- unlist(tapply(colnames(x),clus,function(zz) {sample(zz,size=k)}))
			} else {
				rsamp <- colnames(x)
			}

			if(!is.na(nvars)) {
				cv.model <- cv.glmnet(x[-test.idx,rsamp], y[-test.idx], family=family, alpha=alphas[i], dfmax=nvars, lambda=lambdas)
			} else {
				cv.model <- cv.glmnet(x[-test.idx,rsamp], y[-test.idx], family=family, alpha=alphas[i], lambda=lambdas)
			}
      
      pre <- predict(cv.model, newx=x[test.idx,rsamp], s="lambda.min")
  		mse.j[j] <- mean(y[test.idx] - pre[,1])^2

			tmp <- coef(cv.model, s="lambda.min")[-1,1]
			vars.j[j] <- paste(sort(names(tmp[tmp!=0])), collapse="x")
			allVars <- c(allVars, names(tmp[tmp!=0]))
		}

		multivarsByAlpha[[i]] <- count(vars.j)
    mseByAlpha[i] <- mean(mse.j)
		univarsByAlpha[[i]] <- count(allVars)
		emptyModelsByAlpha[i] <- ifelse(length(count(vars.j)[which(count(vars.j)[,"x"]==""),"freq"])!=0,
			count(vars.j)[which(count(vars.j)[,"x"]==""),"freq"], 0)
		#emptyModelsByAlpha[i] <- count(vars.j)[which(count(vars.j)[,"x"]==""),"freq"]
	}	
	return(list("alphas"=alphas, "mseByAlpha"=mseByAlpha, "emptyModelsByAlpha"=emptyModelsByAlpha, "multivarsByAlpha"=multivarsByAlpha, "univarsByAlpha"=univarsByAlpha))
}

vsglmnet2 <- function(y, x, clus=NULL, k=1, cv=10, nvars=4, alphas=seq(0.1,1,by=0.1), lambdas=10^seq(from=5, to=-10, by=-0.5), family="gaussian")
{
  require("glmnet")
	require("plyr")
	if(!is.matrix(x)) stop("Error: x must be a matrix!")
	if(family=="binomial") {
		if(!is.factor(y)) stop("Error: y must be a factor!")
	}
	if(!identical(names(y),rownames(x))) stop("Error: rownames(x) must be equal to names(y)")

	varsByAlpha <- vector("list", length=length(alphas))
  mseByAlpha <- vector("numeric", length=length(alphas))
  cv.model <- vector("list", length=length(alphas))

	for(i in seq(length(alphas)))
	{
			if(!is.na(nvars)) {
				cv.model[[i]] <- cv.glmnet(x, y, family=family, alpha=alphas[i], dfmax=nvars, lambda=lambdas)
			} else {
				cv.model[[i]] <- cv.glmnet(x, y, family=family, alpha=alphas[i], lambda=lambdas)
			}
      
      mseByAlpha[i] <- cv.model[[i]]$cvm[which(cv.model[[i]]$lambda==cv.model[[i]]$lambda.min)]

			tmp <- coef(cv.model[[i]], s="lambda.min")[-1,1]
			varsByAlpha[[i]] <- names(tmp[tmp!=0])
	}
	return(list("alphas"=alphas, "mseByAlpha"=mseByAlpha, "varsByAlpha"=varsByAlpha, "cv.modelByAlpha"=cv.model))
}