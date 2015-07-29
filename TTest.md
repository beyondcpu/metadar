# Introduction #

The function `univariateTTest` allows performing [Student's t-test](http://en.wikipedia.org/wiki/Student%27s_t-test) for each feature in a `Dataset`object for the comparison of means between a pair of sample groups.

# Function interface #

univariateTTest(Object, covariate, paired=FALSE, is.logged=FALSE, log.base=2, ...)
  * `Object`: an object of class `Dataset`. A t-test will be performed on each feature of this data set. This is a required argument.
  * `covariate`: a string specifying the name of the sample metadata column that has the group variable. Note that the specified column should have a factor with exactly two levels. This is a required argument.
  * `paired`: a logical value specifying whether test should be performed by assuming that there is a pairing of samples between groups defined by `covariate`. This can be missing and the default value is FALSE.
  * `is.logged`: a logical value specifying if the `Object` has the measurements in log-scale. The default value is FALSE, meaning that they are not in log-scale.
  * `log.base`: if the data was in log-scale, the base of the logarithm. The default value is 2.
  * `...`: optional additional arguments to be passed to `t.test` function of the `stats` package in R.

# Available signatures #

`univariateTTest(Object="Dataset", covariate="character", paired=FALSE, is.logged=FALSE, log.base=2, ...)`: i.e. `Object` and `covariate` are required arguments and they should be of classes `Dataset` and `character` respectively. The the rest of the arguments could be missing. When missing, the arguments `paired`, `is.logged` and `log.base` assume the default values specified in the signature and in practice do not have any influence. Additional arguments can be passed to `t.test` function (please check the help document of the `t.test` function).

# Results #

The result of the function `univariateTTest` is a data frame with a row for each feature in the input `Dataset` object. It contains the following columns.

  * `Mean` columns: two columns providing the mean values of both the `covariate` groups. These column names start with "Mean" but depend on the names of the `covariate` groups. When `is.logged = TRUE` the mean values are scaled back to absolute values using the `log.base`. Thus, when `is.logged = TRUE`, effectively, geometric means are computed instead arithmetic means.
  * `SEM` columns: two columns providing the standard error of mean for both the `covariate` groups. These column names start with "SEM" but depend on the names of the `covariate` groups. When `is.logged = TRUE` the SEM values are scaled back to absolute values using the `log.base`.
  * `t.statistic`: a column titled "t.statistic" providing the t-statistic.
  * `p.value`: a column titled "p.value" providing the p-value (i.e. the probability of the observing the "t.statistic" value if the null hypothesis that _no difference in means exists_ were true)
  * `q.value`: an adjusted p-value considering the multiple hypotheses testing. They are computed using `p.adjust` function of `stats` package using Benjamini-Hochberg method.

# Notes #
  * The column specified by the `covariate` must define exactly two groups (i.e. its entries should have exactly two distinct values). If this column divides the samples into more than two groups, please consider subsetting the `Dataset` object using e.g. `get.array.subset` function of `simpleaffy` package. As an example, if you have a `Dataset` object called `Object.orig` with a sample metadata column `Phenotype` with more than two levels (say, "WT", "KO1", "KO2", and "KO4") and you want to compare "KO1" group with "WT" group of samples, then do the following:
```
Object <- get.array.subset(Object.orig, "Phenotype", c("WT", "KO1"))
ttestresult <- univariateTTest(Object, "Phenotype")
```

  * For a paired t-test (i.e. when the samples in the two groups have a one-to-one correspondence), the `Dataset` object is assumed to have the samples of both the groups in the same order. As an example, let us assume that a `Dataset` object `dataset` has measurements from _n_ subjects before and after the treatment with a drug and the objective is to compare the mean value of each feature of `dataset` with respect to this "Time" parameter (i.e. between `before` and `after` the treatment). In order to apply a paired t-test in such case, care must be taken so that the samples corresponding to `before` and `after` in the `dataset` are in the correct matching order. Correct order can be ensured, for example, by sorting the samples by subject name/id in each "Time" group. The paired test can then be done as follows
```
ttestresult <- univariateTTest(dataset, covariate="Time", paired=TRUE)
```

  * Calculation of mean and SEM is done by another function [meanSem](MeanSem.md)