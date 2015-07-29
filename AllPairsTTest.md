# Introduction #

Function `allpairsTTest` allows calculation of a t-test between every variable in one dataset and every variable in a second dataset when the dataset has real-valued variables and the second dataset has binary variables.


# Function interface #

`allpairsTTest(RealValuedDataset, BinaryDataset, ...)`
  * `RealValuedDataset`: an object of class `Dataset` in which the features are real-valued e.g. concentration or abundance levels of molecules.
  * `BinaryDataset`: an object of class `Dataset` in which the feaures are binary e.g. presence or absence of a bacterial DNA.
  * `...`: optional additional arguments to pass on to `t.test` function

# Results #

This function returns a list with two items in it
  * `t.statistic`: a numeric matrix with dimensions `nrow(RealValuedDataset) x nrow(BinaryDataset)`. The _(i, j)_<sup>th</sup> element of this matrix contains the t-statistic value from the t-test on _i_<sup>th</sup> variable of `RealValuedDataset` with respect to the _j_<sup>th</sup> variable of `BinaryDataset` as the covariate.
  * `p.value`: a numeric matrix with dimensions `nrow(RealValuedDataset) x nrow(BinaryDataset)`. The _(i, j)_<sup>th</sup> element of this matrix contains the p-value from the t-test on _i_<sup>th</sup> variable of `RealValuedDataset` with respect to the _j_<sup>th</sup> variable of `BinaryDataset` as the covariate.

A useful way to view the result is using a [heatmap](http://code.google.com/p/ihm) as in the following example
```
res <- allpairsTTest(met, bact)
ihm(res[["t.statistic"]], res[["p.value"]])
```