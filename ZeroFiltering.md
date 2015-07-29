# Introduction #

Function _zeroFiltering_ allows filtering of zero intensity peaks. One could filter the peaks of the data set by requiring a minimum number (or percentage) of samples in which the peak has a non-zero intensity value.


# Function interface #

zeroFiltering(Object, minNfound, pctNfound, covariate)

  * `Object`: an object of _Dataset_ class
  * `minNfound`: an `integer` indicating minimum number of samples in which you expect peak be found (i.e. minimum number of non-zero values required) to keep the peak in the filtered dataset. The peaks which don't pass this threshold will be filtered out (=removed). If the argument `covariate` is provided, then this can be an `integer` vector of length equal to the number of groups defined by the `covariate`.
  * `pctNfound`: an `integer` specifying minimum percentage of samples in which you expect peak be found (i.e. minimum percentage of non-zero values required) to keep the peak in the filtered dataset. The peaks which don't pass this threshold will be filtered out (=removed).  If the argument `covariate` is provided, then this can be an `integer` vector of length equal to the number of groups defined by the `covariate`.
  * `covariate`: covariate based on which filtering should be performed. When provided, it should be the name of a column in the sample metadata whose values represent a group variable (i.e. a factor). If this argument is provided, the filtering is performed conditioned upon the group defined by the covariate i.e. only those peaks which satisfy the `minNfound`/`pctNfound` cut-off in at least one of the groups is kept and the rest removed.

# Available signatures #

  1. `zeroFiltering("Dataset", "numeric", "missing", "missing")`. i.e. the function can be called only using Object and minNfound as the arguments and other arguments can be missing. Object is an object of Dataset class and minNfound is the minimum number of samples in which you expect to see a peak (i.e. minimum number of non-zero values expected for a feature f) to keep the feature in the in Object. The features of Object that don't meet this criterion will be removed from Object.
  1. `zeroFiltering("Dataset", "missing", "numeric", "missing")`. i.e. the function can be called only using Object and pctNfound as the arguments and other arguments can be missing. Object is an object of Dataset class and minNfound is the minimum percentage of samples in which you expect to see a peak (i.e. minimum percentage of non-zero values expected for a feature f) to keep the feature in the in Object. The features of Object that don't meet this criterion will be removed from Object.
  1. `zeroFiltering("Dataset", "numeric", "missing", "character")`. i.e. the function can be called using _Object_, _minNfound_ and _covariate_. The _covariate_ is the name of a sample metadata column whose contents have to be a factor (i.e. define a sample group). The filtering criterion (i.e. _minNfound_) is checked so that at least one sample group passes the criterion. If a peak does not satisfy the criterion in none of the sample groups, then it will be removed.
  1. `zeroFiltering("Dataset", "missing", "numeric", "character")`. i.e. the function can be called using _Object_, _pctNfound_ and _covariate_. The _covariate_ is the name of a sample metadata column whose contents have to be a factor (i.e. define a sample group). The filtering criterion (i.e. _pctNfound_) is checked so that at least one sample group passes the criterion. If a peak does not satisfy the criterion in none of the sample groups, then it will be removed.

# Result #

This function returns a _Dataset_ object that contains a subset of the features from the input _Dataset_ object i.e. only those features that pass the zero filtering criterion.

# Note #

The filtering cut-off is provided as either _minNfound_ or _pctNfound_, but not both.

# CAUTION #

As always with data analysis please take a good look at the data and apply this method only after making sure that it is applicable. That is, try to figure out why there are zeros in the data or if there are too many more zeros than you would expect before applying this method. Applying this method would of course leave you with features that have as many non-zero values as you require, but if some unexpected error has introduced zeros in your data, it may even be possible that the whole data is wrong.

As a specific example, I use this method often for GCxGC-Tof metabolomics data. In some cases too many zeros in GCxGC data maybe due to errors during the pre-processing (e.g. alignment of peaks across study samples).