# Introduction #

The function `boxPlot` allows you to create box-and-whisker plots for all features in a given `Dataset` object.

# Function interface #

boxPlot(Object, covariate1, covariate2, file="boxplots.pdf")
  * `Object`: an object of class `Dataset`.
  * `covariate1`: a character string giving the column name of sample metadata that divides the samples into two or more groups.
  * `covariate2`: a character string giving the column name of sample metadata that divides the samples into further groups (as a second factor on top of `covariate1`).
  * `file`: an optional file name argument into which the box plots will be written. If not supplied, the default value "boxplots.pdf" will be assumed. File name maybe provided as a fully qualified path. If the file name provided is not a full path, then it will be written into the working directory.

# Available signatures #

  1. `boxPlot(Object="Dataset", covariate1="character", covariate2="missing", file="boxplots.pdf")` i.e. `covariate2` can be missing, `Object` and `covariate1` are supplied, and `file` is optional with default value when missing.
  1. `boxPlot(Object="Dataset", covariate1="character", covariate2="character", file="boxplots.pdf")` i.e. `Object`, `covariate1`, and `covariate2` are supplied, and `file` is optional with default value when missing.

# Examples #

Let `dataset` be an object of class `Dataset` with sample metadata columns `"Drug"` and `"Time"`. Then the following function calls can be used
  1. to look at the differences in the abundances of compounds with respect to `"Drug"` (the resulting box plots will be created in the file boxplots.pdf to be found in the current working directory as the `file` argument is omitted from this call)
```
boxPlot(dataset, "Drug")
```
  1. to look at the differences in the abundances of compounds with respect to drug and time
```
boxPlot(dataset, "Drug", "Time", file="/home/xxx/boxplots.pdf")
```