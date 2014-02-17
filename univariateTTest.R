setGeneric("univariateTTest", signature=c("Object", "covariate"),
           def=function(Object, covariate, paired=FALSE, is.logged=FALSE, log.base=2, ...)
             standardGeneric("univariateTTest"))

setMethod("univariateTTest",  signature=c(Object="Dataset", covariate="character"),
          valueClass="data.frame",
          definition=function(Object, covariate, paired=FALSE, is.logged=FALSE, log.base=2, ...) {
            res <- t(apply(exprs(Object), 1, function(x) {
              tt <- t.test(x ~ factor(getSampleMetaData(Object,covariate)),
                           alternative="two.sided", paired=paired, ...)
              c("t.statistic"=as.numeric(tt$statistic), "p.value"=tt$p.value)
            }))
            qvals <- p.adjust(res[,"p.value"], method="BH")
            means <- meanSem(Object, covariate, is.logged, log.base)
            return(data.frame(means, res, "q.value"=qvals))
          })