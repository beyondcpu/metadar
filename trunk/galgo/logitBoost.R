logitboost <- function (chr, parent, tr, te, result) {
	d <- parent$data
	s <- logitboost(d$data[tr, chr], d$iclasses[tr]-1, d$data[te, chr])
	k <- ifelse(s[,ncol(s)] >= 0.5,2,1)
	if (result) sum(k == d$classes[te])/length(te)
	else k
}

# Example of how to use it
# bb.lb <- configBB.VarSel(..., classification.method="user",
# classification.userFitnessFunc=logitboost)
