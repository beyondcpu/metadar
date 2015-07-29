# Introduction #

The function `meanSem` allows to calculate mean value and standard error of mean for each feature in a `Dataset` object for two or more groups of samples.

# Function interface #

`meanSem(Object, covariate, is.logged=FALSE, log.base=2)`
  * `Object`: an object of class `Dataset`. The mean values and standard errors of mean are calculated for each feature of this data set. This is a required argument.
  * `covariate`: a string specifying the name of the sample metadata column that has the group variable. The specified column should have a factor with two or more levels. This is a required argument.
  * is.logged: a logical value specifying if the `Object` has the measurements in log-scale. When missing, the default value of this argument is FALSE, meaning that data are not in log-scale. If the data are  in log-scale, the mean values are provided in the absolute scale (i.e. transformed back from log using the log.base). Thus, when `is.logged = TRUE`, effectively, geometric means of absolute values are calculated.
  * log.base: if the data was in log-scale, the base of the logarithm. The default value is 2.

# Note #

When the data is in log-scale (and the argument `is.logged=TRUE` is passed accordingly), to prevent any misinterpretation of the results, `meanSem` function first inverts the data from log-scale and then computes the `mean` and `SEM`. When data is in log-scale, please consider using the function [meanCI](MeanCI.md), because the meanCI function computes the mean and confidence intervals based directly on the log-scale data.