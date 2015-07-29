# Version 0.48 #
  * Two additional arguments _statistic_ and _p.value_ are added to tgAnalysis method so that a custom statistic and p-value could be passed. The example use case that led to this is adjusting for BMI and AGE, in which case the statistic is odds ratio and it was computed from univariateOddsRatio.
  * Added "validation" methods to univariateTTest and univariateWilcox functions.
  * Added a new method called `hilo` that transforms the data into two (high or low) or three levels (high, medium or low) based on mean or median.

# Version 0.46 #
  * `q.value`s are added to `twoWayAnova` result.
  * `oneWayAnova` return object column headers updated.
  * Now when calling `allpairsTTest`, it is possible to pass additional arguments to `t.test` function (please check the help documentation of `t.test` function). The names of items in the list returned by `allpairsTTest` updated as "t.statistic" and "p.value" for consistency.
  * When is.logged=TRUE, meanSem() function antilogs the data before computing mean and SEM.
  * Added a function meanCI(). This is recommended over meanSem() when is.logged=TRUE

# Version 0.45 #

  * The function medianQuartile() is now renamed to medianCI() and as the name suggests, it now calculates 95% confidence interval of the median instead of first and third quartiles.
  * univariateTTest() now returns mean values and standard errors of mean besides the p-value and q-value.
  * univariateTTest() now accepts additional optional arguments (is.logged and log.base) to get the mean values in the right scale.
  * univariateWilcox() now returns median values and 95% CI of median besides the p-value and q-value.
  * In the result of univariateTTest() and univariateWilcox() the title of the p-value column is now "p.value" and that of q-value is "q.value".

# Version 0.44 #
  * In univariateOddsRatio() and allpairslogOdds() functions, the confidence interval calculation is now replaced by confint() function from MASS package.
  * selectVariables() and selectSamples() functions are removed. Please use _dataset[subset,]_ (or _dataset[,subset]_) to retrieve a _subset_ of samples (or features) from a _Dataset_ object _dataset_.
  * A new method "aroundmean" added to zero imputation.
  * PCA method has been removed. It is replaced by a PCA reference class.

# Version 0.43 #
  * A new method "aroundhalfmin" added to the zero imputation. It is the default zero imputation method now. This method adds a little noise around half of the minimum of the non-zero values.
  * Scaling of the PCA has been modified.