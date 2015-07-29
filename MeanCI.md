# Introduction #

The function meanCI allows to calculate mean value and 95% confidence interval of mean (CI) for every feature in a Dataset object for two or more groups of samples.


# Function interface #

meanCI(Object, covariate, is.logged=FALSE, log.base=2)
  * `Object`: an object of class `Dataset`. The mean values and CI are calculated for every feature of this data set. This is a required argument.
  * `covariate`: a `string` specifying the name of the sample metadata column that has the group variable. The specified column should have a factor with two or more levels. This is a required argument.
  * `is.logged`: a logical value specifying if the `Object` has the measurements in log-scale. When missing, the default value of this argument is FALSE, meaning that data are not in log-scale. If the data are  in log-scale, the mean values are provided in the absolute scale (i.e. transformed back from log using the log.base). Thus, when `is.logged = TRUE`, effectively, geometric means of absolute values are calculated.
  * `log.base`: if the data was in log-scale, the base of the logarithm. The default value is 2.