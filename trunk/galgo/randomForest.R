randomforest <- function(chr, parent, tr, te, result) {
	d <- parent$data
	xrf <- randomForest(x=d$data[tr,chr], y=d$classes[tr],
			    xtest=d$data[te,chr], ytest=d$classes[te])
	#if all(te==tr) resubstitution was specified, which is faster
	#considering that RF performs an internal cross-validation (out-of-bag)
	if (result) {
		if (all(te==tr)) sum(xrf$predicted==d$classes[te])/length(te)
		else sum(xrf$test$predicted==d$classes[te])/length(te)
	} else {
		if (all(te==tr)) xrf$predicted==d$classes[te]
		else xrf$test$predicted
	}
}

# Example to call it.
# bb.lb <- configBB.VarSel(..., classification.method="user",
# classification.userFitnessFunc=randomforest,
# classification.train.error="resubstitution")
