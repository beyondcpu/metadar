# Introduction #

Some data analyses do not work well if the intensity value data, which is real valued, has too many zeros or NA's. The method _zeroImputation_ allows imputation of zeros. Also the NA's in the data set are replaced by zeros and hence are imputed like other zeros.

# Function interface #

zeroImputation(Object, method, covariate)
  * `Object`: an object of class _Dataset_ on which the zero imputation will be performed.
  * `method`: one of "aroundhalfmin", "halfmin", "aroundmean". The default value is "aroundhalfmin". The method "aroundhalfmin" replaces zeros by half of the minimum non-zero value plus some noise. The "halfmin" method replaces zeros by half of the minimum non-zero value. The "aroundmean" method replaces zeros by the mean value plus noise. When `covariate` is not provided zero imputation of a peak is done based on non-zero values of the peak from all samples in the data set. When `covariate` is provided, sample groups defined by `covariate` are used independently i.e. a zero value of a peak in a sample is replaced by non-zero values of the peak from the rest of the samples in the same group.
  * `covariate`: covariate conditioned upon which imputation should be performed. When provided, it should be the name of a column in the sample metadata whose values represent a group variable (i.e. a factor). If this argument is provided, the imputation is performed conditioned upon the group defined by the covariate i.e. non-zero values are imputed in each sample group is imputed independently of other sample groups.

# Available signatures #
  1. `zeroImputation("Dataset", "missing", "missing")`: the function can be called with one argument, `Object`, on which the zero imputation has to be performed. The default method, "aroundhalfmin" will be used.
  1. `zeroImputation("Dataset", "character", "missing")`: the function can be called with two arguments, `Object` and `method`. The specified method will be used for zero imputation of the data set object.
  1. `zeroImputation("Dataset", "missing", "character")`: the function can be called with two arguments, `Object` and `covariate`. The default zero imputation method will be applied using the sample groups defined by the `covariate`.
  1. `zeroImputation("Dataset", "character", "character")`: the function can be called with all three arguments. The imputation will be performed on the data set object using the specified method for each sample group defined by the `covariate`

Note that when this function is called with two arguments, it is necessary to name the arguments for correct method dispatch to work.
# Examples #
The following example invocations are written in the same order as in the [Available signatures](ZeroImputation#Available_signatures.md) section. Also, it is assumed in these examples that a _Dataset_ object called dataset is created and that its sample metadata has a column titled "Class".
  1. Example with one argument (the default method, "aroundhalfmin" will be used)
```
zi <- zeroImputation(dataset)
```
  1. Example with dataset and method arguments
```
zi <- zeroImputation(dataset, method="aroundmean")
```
  1. Example with dataset and covariate arguments (the default method, "aroundhalfmin" will be used)
```
zi <- zeroImputation(dataset, covariate="Class")
```
  1. Example with dataset, method and covariate arguments
```
zi <- zeroImputation(dataset, method="halfmin", covariate="Class")
```