# Introduction #

The function _oneWayAnova_ allows performing one-way analysis of variance using F-test for each feature in a `Dataset` object.

# Function interface #

`oneWayAnova(Object, covariate)`
  * `Object` - an object of class `Dataset`
  * covariate - a string specifying the name of the sample metadata column that has the group variable.

# Available signatures #
  * `oneWayAnova("Dataset", "character")` i.e. both arguments `Object` and `covariate` are provided in the function call

# Examples #

If `dataset` is an object of class `Dataset` and `Class` is a column name of the `covariate` of interest then the function call to perform one-way ANOVA goes as follows
```
aovresult <- oneWayAnova(dataset, "Class")
```

# Result #

The `oneWayAnova` function returns a data.frame object that contains the test result for each variable in the `Dataset` object in a row. The results include anova F-test result and the result of a post-hoc test called Tukey's Honestly Significant Differences (Tukey HSD). In more detail, the included results are:

  * the value of the F-statistic
  * the corresponding anova p-value (i.e. the probability of observing the F-statistic if the null hypothesis that there is no difference in means were true)
  * Benjamini-Hochberg q-value after adjusting the p-value for multiple hypotheses
  * mean value of the feature for each group defined by the `covariate`
  * ratio of mean values between every pair of `covariate` groups (i.e. fold changes)
  * adjusted p-value derived from Tukey HSD test indicating the significance of difference between means of every pair of groups.

## Presenting the results ##

It is often useful to write the results to a file to study them. The `aovresult` from the example above can be written to a file as follows

```
write.csv(aovresult, file="aovresult.csv")
```

Also, it maybe useful to graphically display the anova result. A heatmap showing significant results in `aovresult` can be drawn using [ihm](http://code.google.com/p/ihm) as follows:
```
log2foldchanges <- log2(aovresult[, grep("ratio", colnames(aovresult))])
pvalues <- aovresult[, grep("p adj", colnames(aovresult))]
take <- apply(pvalues, 1, function(x) any(x < 0.05))

ihm(log2foldchanges[take,], pvalues[take,], clusterColumns=TRUE, clusterRows=TRUE,
   device="pdf", file="Heatmap_sig.pdf", cellnotesizeCorrection=2)
```