vs <- setRefClass("VariableSelection", fields=list(
	x.train = "data.frame",
	y.train = "factor",
	x.test = "data.frame"
	y.test = "factor"))

vs$methods()
