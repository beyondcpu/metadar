# Introduction #

Function _allpairsCorrelation_ allows calculation of a correlation coefficient between every pair of variables of one dataset or between every variable in one dataset and every variable in a second dataset.


# Function interface #

allpairsCorrelation(Object1, Object2, method, ...)
  * `Object1`: A `Dataset` object or a numeric matrix containing variables along rows and samples along columns
  * `Object2`: A `Dataset` object or a numeric matrix containing variables along rows and samples along columns
  * `method`: A character string with value as "pearson" or "spearman". Check the method argument of cor.test for more details
  * `...`: Additional arguments for _cor.test_ function of R _stats_ package

# Available signatures #
  1. `allpairsCorrelation("Dataset", "missing", "character")` i.e. the function can be called with `Object1` (a `Dataset` object) and `method` arguments and `Object2` can be missing. In this case correlation between every pair of features in `Object1` is computed using the specified `method` ("pearson" or "spearman").
  1. `allpairsCorrelation("Dataset", "Dataset", "character")` i.e. the function can be called with `Object1` (a `Dataset` object), `Object2` (a `Dataset` object) and `method` arguments. In this case correlation between every feature in `Object1` and every feature in `Object2` is computed using the specified `method` ("pearson" or "spearman").
  1. `allpairsCorrelation("matrix", "missing", "character")` i.e. the function can be called with `Object1` (a numeric matrix with features in rows and samples in columns) and `method` arguments and `Object2` can be missing. In this case correlation between every pair of features in `Object1` is computed using the specified `method` ("pearson" or "spearman").
  1. `allpairsCorrelation("matrix", "missing", "character")` i.e. the function can be called with `Object1` (a numeric matrix with features in rows and samples in columns), `Object2` (a numeric matrix with features in rows and samples in columns) and `method` arguments. In this case correlation between every feature in `Object1` and every feature in `Object2` is computed using the specified `method` ("pearson" or "spearman").

# Examples #
The following example invocations of the `allpairsCorrelation` method are written in the same order as in the **Available signatures** section. It is assumed that `Dataset` objects `dataset1` and `dataset2` and numeric matrices `matr1` and `matr2` are defined.
  1. Pearson's correlation between every pair of features in a `Dataset` object
```
corr <- allpairsCorrelation(dataset1, method="pearson")
```
  1. For every feature `f` in `dataset1` and every feature `g` in `dataset2`, compute Spearman's correlation `rho(f, g)`. In the following invocation one additional argument to _cor.test_ function is issued
```
corr <- allpairsCorrelation(dataset1, dataset2, method="spearman", exact=FALSE)
```
  1. Spearman's rank correlation between every pair of rows in a numerical matrix
```
corr <- allpairsCorrelation(matr1, method="spearman")
```
  1. Pearson's product moment correlation between every row in `matr1` and every row in `matr2`.
```
corr <- allpairsCorrelation(matr1, matr2, method="pearson")
```

# Results #

The resulting object `corr` in above examples is a list of two matrices. The first element of this list, namely, `corr[["r"]]` contains the correlation coefficients and the second element, `corr[["p"]]` contains the respective p-values associated with the null hypothesis that there is no correlation (i.e. correlation coefficient = 0).

# Note #

One very useful way to look at the resulting object `corr` is using a heatmap. Using [ihm](http://code.google.com/p/ihm), it can be constructed as follows
```
ihm(corr[["r"]], corr[["p"]], clusterRows=TRUE, clusterColumns=TRUE, device="pdf", file="corr_heatmap.pdf")
```