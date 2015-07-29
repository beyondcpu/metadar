# Introduction #

The function `univariateAUC` computes area under the ROC curve (AUC) for each featured in the `Dataset` object as a measure of discrimination accuracy with respect to a binary group variable in the sample metadata.


# Function interface #

`univariateAUC(Object, covariate)`
  * `Object`: an object of `Dataset` class.
  * `covariate`: name of a sample metadata column that divides the samples into two groups.

# Results #

A vector of AUC values of all features in the `Dataset` object.

# Examples #
```
univariateAUC(dataset, "Class")
```