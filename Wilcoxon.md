# Introduction #

The function `univariateWilcox` allows performing Wilcoxon's [signed rank test](http://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test) (when samples are paired) or [rank sum test](http://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U) (when samples are not paired) for each feature in a `Dataset` object for a non-parametric comparison between a pair of sample groups.

# Function interface #

`univariateWilcox(Object, covariate, paired=FALSE, ...)`
  * `Object`: an object of class `Dataset`. A Wilcoxon rank sum test or signed rank test will be performed on each feature of this data set. `Object` is a required argument.
  * `covariate`: a string specifying the name of the sample metadata column that has the group variable. Note that the specified column should have a factor with exactly two levels. `covariate` is a required argument.
  * `paired`: a logical value specifying whether test should be performed by assuming that there is a pairing of samples between groups defined by `covariate`. This can be missing and the default value is FALSE.
  * `...`: optional additional arguments to be passed to `wilcox.test` function of the `stats` package in R.

# Available signatures #

`univariateTTest(Object="Dataset", covariate="character", paired=FALSE, ...)`: i.e. `Object` and `covariate` are required arguments and they should be of classes `Dataset` and `character` respectively. The argument `paired` could be missing. When missing, the default value of `paired = FALSE` is assumed. Additional arguments can be passed to `wilcox.test` function (please check the help document of the `wilcox.test` function).

# Results #

The result of the function univariateWilcox is a data frame with a row for each feature in the input `Dataset` object. It contains the following columns.

  * `Median` columns: two columns providing the median values of both the `covariate` groups. These column names start with "Median" but depend on the names of the `covariate` groups.

  * 95% confidence intervals of medians: two columns providing the lower and upper bound of 95% CI for median for each `covariate` group. The names of the columns containing the lower bound of the 95% CI start with "95% CIL", but depend on the names of the `covariate` groups.  The names of the columns containing the upper bound of the 95% CI start with "95% CIU", but depend on the names of the `covariate` groups.
  * `p.value`: a column titled "p.value" providing the p-value (i.e. the probability of the observing the test statistic value if the null hypothesis that _no difference between the groups exist_ were true)
  * `q.value`: an adjusted p-value considering the multiple hypotheses testing. They are computed using `p.adjust` function of `stats` package using Benjamini-Hochberg method.

# Notes #
  * The column specified by the `covariate` must define exactly two groups (i.e. its entries should have exactly two distinct values). If this column divides the samples into more than two groups, please consider subsetting the `Dataset` object using e.g. `get.array.subset` function of `simpleaffy` package. As an example, if you have a `Dataset` object called `Object.orig` with a sample metadata column `Phenotype` with more than two levels (say, "WT", "KO1", "KO2", and "KO4") and you want to compare "KO1" group with "WT" group of samples, then do the following:
```
Object <- get.array.subset(Object.orig, "Phenotype", c("WT", "KO1"))
wilcresult <- univariateWilcox(Object, "Phenotype")
```

  * For a paired test (i.e. when the samples in the two groups have a one-to-one correspondence), the `Dataset` object is assumed to have the samples of both the groups in the same order. As an example, let us assume that a `Dataset` object `dataset` has measurements from _n_ subjects before and after the treatment with a drug and the objective is to compare each feature of `dataset` with respect to this "Time" parameter (i.e. difference between `before` and `after` the treatment). In order to apply a paired test in such case, care must be taken so that the samples corresponding to `before` and `after` in the `dataset` are in the correct matching order. Correct order can be ensured, for example, by sorting the samples by subject name/id in each "Time" group. The paired test can then be done as follows
```
wilcresult <- univariateWilcoxon(dataset, covariate="Time", paired=TRUE)
```

  * Calculation of median and 95% CI of median is done by another function [medianCI](MedianCI.md).