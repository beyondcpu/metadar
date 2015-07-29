# Introduction #

The function `medianCI` allows to calculate median value and 95% CI of median for every feature in a `Dataset` object for two or more groups of samples.

# Function interface #

`medianCI(Object, covariate)`
  * `Object`: an object of class `Dataset`. The mean values and standard errors of mean are calculated for each feature of this data set. This is a required argument.
  * `covariate`: a string specifying the name of the sample metadata column that has the group variable. The specified column should have a factor with two or more levels. This is a required argument.