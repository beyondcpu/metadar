my.boxplot2 = function(my.data.frame, boxes, colors=NULL, 
xlabel="Groups", ylabel="Concentration", title="Box and whisker plot of 
concentrations in each sample", device="X11", OutputFileName=NULL) {
  switch(device,
    "pdf"=pdf(OutputFileName),
    "png"=png(OutputFileName),
    X11()
  )
    if(length(levels(boxes)) == ncol(my.data.frame)) {
      if(!is.null(colors)) {
        colors2 <- factor(colors)
        levels(colors2) <- 1:length(levels(colors))
        boxplot(data.frame(apply(my.data.frame, 2, as.numeric)), 
col=colors2, xlab=xlabel, ylab=ylabel, main=title)
        legend("topright", legend=levels(factor(colors)), 
col=levels(colors2), lwd=4)
      } else {
        boxplot(data.frame(apply(my.data.frame, 2, as.numeric)), 
xlab=xlabel, ylab=ylabel, main=title)
      }
    } else {
       newdata <- split(t(apply(my.data.frame, 2, as.numeric)), boxes)
       boxplot(newdata, xlab=xlabel, ylab=ylabel, main=title)
    }
  switch(device,
    "pdf"=dev.off(),
    "png"=dev.off()
  )
}