# Introduction #

The function `foldChange` computes the fold change for each feature in a `Dataset` object with respect to a phenotype grouping variable.


# Function interface #

foldChange(Object, covariate, paired=FALSE, is.logged=FALSE, log.base=FALSE)
  * `Object`: an object of class `Dataset`
  * `covariate`: a character string specifying the title of sample metadata column that contains the groups. The specified column should define two groups.
  * `paired`: a logical object. When TRUE, the samples are considered to be paired, and it is assumed that the samples in each group are ordered by pairing.
  * `is.logged`: a logical value indicating whether or not the `Dataset` object contains log-values.
  * `log.base`: numeric specifying the base of the logarithm in case `is.logged=TRUE`.

# Available signatures #

  1. `foldChange(Object="Dataset", covariate="character", paired=FALSE, is.logged=FALSE, log.base=2)`: i.e. `Object` and `covariate` are required. The rest of the arguments could be missing. When missing, the arguments `paired`, `is.logged` and `log.base` assume the default values specified in the signature.

# Results #

This function returns a `data.frame` object that contains the following columns

  * `FC`: Fold change value.
  * `FC 95CI.LL`: Lower limit of the 95% confidence interval of the fold change.
  * `FC 95CI.UL`: Upper limit of the 95% confidence interval of the fold change.
  * `FC 95% CI`: The confidence interval presented in the format "(LL, UL)". In this column the limits are rounded to two digits.

# Examples #

```
fc <- foldChange(Object=dataset, covariate="Class")
```
```
fc <- foldChange(Object=dataset, covariate="Class", paired=TRUE)
```
```
fc <- foldChange(Object=dataset, covariate="Class", is.logged=TRUE) # log.base=2 is assumed
```