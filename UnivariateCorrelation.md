# Introduction #

The function `univariateCorrelation` allows us to compute correlation coefficient of each feature in the `Dataset` object with a target phenotype variable of interest.

# Function interface #

`univariateCorrelation(Object, covariate, ...)`
  * `Object`: an object of class `Dataset`.
  * `covariate`: a character indicating the name of a column of sample metadata or a numeric vector containing the target phenotype variable of interest.
  * `...`: additional arguments for cor.test function. Please check the help document of cor.test function. All additional arguments except `alternative` can be passed (this function always uses `alternative="two.sided"`).

# Available signatures #
  1. `univariateCorrelation(Object="Dataset", covariate="character")`: `covariate` is a character string providing the name of a column of sample metadata that typically has a real valued data.
  1. `univariateCorrelation(Object="Dataset", covariate="numeric")`: `covariate` is a numeric vector containing the target phenotype against which the features should be correlated.