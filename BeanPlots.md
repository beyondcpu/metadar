# Introduction #

The function beanPlot allows you to create bean plots for all features in a given Dataset object.


# Function interface #

beanPlot(Object, covariate1, covariate2, Index, file="beanplots.pdf", ...)
  * `Object`: an object of class `Dataset`.
  * `covariate1`: a character string giving the column name of sample metadata that divides the samples into two or more groups.
  * `covariate2`: a character string giving the column name of sample metadata that divides the samples into further groups (as a second factor on top of `covariate1`).
  * `Index`: an integer or character string (giving the index or ID respectively) of the variable for which beanPlot has to be plotted. When this argument is not provided, bean plot of each variable is plotted to the PDF file (one variable per page).
  * `file`: an optional file name argument into which the bean plots will be written. If not supplied, the default value "beanplots.pdf" will be assumed. File name maybe provided as a fully qualified path. If the file name provided is not a full path, then it will be written into the working directory.
  * `...`: additional arguments for `beanplot` function of `beanplot` package. The arguments `col`, and `main` are however not available.

# Available signatures #

  1. `beanPlot(Object="Dataset", covariate1="character", covariate2="missing", Index="missing", file="beanplots.pdf", ...)` i.e. `covariate2` and `Index` can be missing, `Object` and `covariate1` are supplied, and `file` is optional with default value when missing.
  1. `beanPlot(Object="Dataset", covariate1="character", covariate2="missing", Index="integer", file="beanplots.pdf", ...)` i.e. `covariate2` can be missing, `Object`, `covariate1`, and `Index` (integer) are supplied, and `file` is optional with default value when missing.
  1. `beanPlot(Object="Dataset", covariate1="character", covariate2="missing", Index="character", file="beanplots.pdf", ...)` i.e. `covariate2` can be missing, `Object`, `covariate1`, and `Index` (character) are supplied, and `file` is optional with default value when missing.
  1. `beanPlot(Object="Dataset", covariate1="character", covariate2="character", Index="missing", file="beanplots.pdf", ...)` i.e. `Object`, `covariate1`, and `covariate2` are supplied, `Index` is missing, and `file` is optional with default value when missing.
  1. `beanPlot(Object="Dataset", covariate1="character", covariate2="character", Index="integer", file="beanplots.pdf", ...)` i.e. `Object`, `covariate1`, and `covariate2` are supplied, `Index` is integer, and `file` is optional with default value when missing.
  1. `beanPlot(Object="Dataset", covariate1="character", covariate2="character", Index="character", file="beanplots.pdf", ...)` i.e. `Object`, `covariate1`, and `covariate2` are supplied, `Index` is a character string, and `file` is optional with default value when missing.

# Examples #

Let `dataset` be an object of class `Dataset` with sample metadata columns `"Drug"` and `"Time"`. Then the following function calls can be used
  1. to look at the differences in the abundances of compounds with respect to `"Drug"` (the resulting bean plots will be created in the file beanplots.pdf to be found in the current working directory as the `file` argument is omitted from this call)
```
beanPlot(dataset, covariate1="Drug")
```
  1. to look at the differences in the abundances of compounds with respect to drug and time
```
beanPlot(dataset, covariate1="Drug", covariate2="Time", file="/home/xxx/beanplots.pdf")
```