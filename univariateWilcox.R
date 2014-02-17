setGeneric("univariateWilcox", signature=c("Object", "covariate"),
           def=function(Object, covariate, paired=FALSE, ...) standardGeneric("univariateWilcox"))

setMethod("univariateWilcox",  signature=c(Object="Dataset", covariate="character"),
          valueClass="data.frame", definition=function(Object, covariate, paired=FALSE, ...) {
            pvals <- apply(exprs(Object), 1, function(x) {
              wil <- wilcox.test(x ~ factor(pData(Object)[,covariate]),
                                 alternative="two.sided", paired=paired, ...)
              wil$p.value
            })
            qvals <- p.adjust(pvals, method="BH")
            medians <- medianCI(Object, covariate)
            return(data.frame(medians, "p.value"=pvals, "q.value"=qvals))
          })