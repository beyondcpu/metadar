setGeneric("pca", function(Object, annotation, color, scale, title) standardGeneric("pca"))

setMethod("pca", signature=c("Dataset", "character", "character", "logical", "missing"),
    function(Object, annotation, color, scale) {
      expr <- exprs(Object)
      if(scale) {
        expr <- scale(expr)
      }
      pr <- prcomp(expr, center=FALSE, scale=FALSE)
      prop.var <- round((pr$sdev^2 / sum(pr$sdev^2)) * 100, 2)
      plot(pr$rotation[,c(1,2)], xlab=paste("PC1:", prop.var[1], "% variance"),
           ylab=paste("PC2:", prop.var[2], "% variance"),
           type="n", main="Principal component analysis")
      annotation.colors <- as.factor(pData(Object)[,color])
      levels(annotation.colors) <- 1:length(levels(annotation.colors))
      text(pr$rotation[,c(1,2)], as.character(pData(Object)[,annotation]),
           col=as.character(annotation.colors))
      pr
    })

setMethod("pca", signature=c("Dataset", "character", "character", "logical", "character"),
    function(Object, annotation, color, scale, title) {
      expr <- exprs(Object)
      if(scale) {
        expr <- scale(expr)
      }
      pr <- prcomp(expr, center=FALSE, scale=FALSE)
      prop.var <- round((pr$sdev^2 / sum(pr$sdev^2)) * 100, 2)
      plot(pr$rotation[,c(1,2)], xlab=paste("PC1:", prop.var[1], "% variance"),
           ylab=paste("PC2:", prop.var[2], "% variance"),
           type="n", main=title)
      annotation.colors <- as.factor(pData(Object)[,color])
      levels(annotation.colors) <- 1:length(levels(annotation.colors))
      text(pr$rotation[,c(1,2)], as.character(pData(Object)[,annotation]),
           col=as.character(annotation.colors))
      pr
    })

setMethod("pca", signature=c("Dataset", "character", "character", "missing", "missing"),
    function(Object, annotation, color) {
      ### assume scaling if it is not provided
      scale = TRUE
      expr <- exprs(Object)
      if(scale) {
        expr <- scale(expr)
      }
      pr <- prcomp(expr, center=FALSE, scale=FALSE)
      prop.var <- round((pr$sdev^2 / sum(pr$sdev^2)) * 100, 2)
      plot(pr$rotation[,c(1,2)], xlab=paste("PC1:", prop.var[1], "% variance"),
           ylab=paste("PC2:", prop.var[2], "% variance"),
           type="n", main="Principal component analysis")
      annotation.colors <- as.factor(pData(Object)[,color])
      levels(annotation.colors) <- 1:length(levels(annotation.colors))
      text(pr$rotation[,c(1,2)], as.character(pData(Object)[,annotation]),
           col=as.character(annotation.colors))
      pr
    })

setMethod("pca", signature=c("Dataset", "character", "character", "missing", "character"),
    function(Object, annotation, color, title) {
      ### assume scaling if it is not provided
      scale = TRUE
      expr <- exprs(Object)
      if(scale) {
        expr <- scale(expr)
      }
      pr <- prcomp(expr, center=FALSE, scale=FALSE)
      prop.var <- round((pr$sdev^2 / sum(pr$sdev^2)) * 100, 2)
      plot(pr$rotation[,c(1,2)], xlab=paste("PC1:", prop.var[1], "% variance"),
           ylab=paste("PC2:", prop.var[2], "% variance"),
           type="n", main=title)
      annotation.colors <- as.factor(pData(Object)[,color])
      levels(annotation.colors) <- 1:length(levels(annotation.colors))
      text(pr$rotation[,c(1,2)], as.character(pData(Object)[,annotation]),
           col=as.character(annotation.colors))
      pr
    })
