# Introduction #

The function _twoWayAnova_ allows us to perform two-way analysis of variance for each feature in a `Dataset` object using two categorical treatment variables. The result would indicate the main effects of both treatment variables and interaction between the treatment variables.

# Function interface #

`twoWayAnova(Object, covariate1, covariate2)`
  * `Object`: An object of the class `Dataset`.
  * `covariate1`: the name of the column in sample metadata giving the first independent variable
  * `covariate2`: the name of the column in sample metadata giving the first independent variable

# Available signatures #

  1. `twoWayAnova("Dataset", "character", "character")` i.e. all three arguments are provided in a function call.

# Example #

Let `dataset` be an object of `Dataset` class and let "C1" and "C2" be the names of sample metadata columns. The function call goes as follows
```
aovresult <- twoWayAnova(dataset, "C1", "C2")
```

# Result #

The result of the `twoWayAnova` (`aovresult` in the example above) is a list of three data frame object each of which contain in its each row results for each variable in the `Dataset` object. The names of the data frame items of the list are
  * `BigResultTable`: contains all results i.e. all items in the other two data frames explained below. In the example above, this can be accessed with the command `aovresult[["BigResultTable"]]`.
  * `RatiosTable`: mean values for all groups and fold changes corresponding to the main effects and corresponding to every pair of groups. In the example above, this can be accessed with the command `aovresult[["RatiosTable"]]`.
  * `PvalsTable`:  p-values corresponding to each main effect, interaction effect and the post-hoc comparison of pairs of groups. In the example above, this can be accessed with the command `aovresult[["PvalsTable"]]`.

## Presenting the results ##

A heatmap of the significant results in `aovresult` (example above) can be drawn using [ihm](http://code.google.com/p/ihm) as follows:
```
log2fc <- log2(aovresult[["RatiosTable"]][, grep("ratio", colnames(aovresult[["RatiosTable"]]))])
pvals <- aovresult[["PvalsTable"]]
take <- apply(pvals, 1, function(x) any(x < 0.05))

ihm(log2fc[take,], pvals[take,], clusterColumns=TRUE, clusterRows=TRUE,
   device="pdf", file="Heatmap_sig.pdf", cellnotesizeCorrection=2)
```