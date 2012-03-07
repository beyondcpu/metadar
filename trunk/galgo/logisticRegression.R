logisticregression <- function(chr, parent, tr, te, result) {
	d <- parent$data
	yx.glm <- glm(y~., family=binomial, data=data.frame("y"=d$classes[tr], d$data[tr, chr]))
	pred <- predict.glm(yx.glm, newdata=data.frame(d$data[te, chr]), type="response")
	k <- ifelse(pred>=0.5, 2, 1)
	if(result) sum(k == d$classes[te])/length(te)
	else k
}
