# Introduction #

The function `colhclust` allows calculation of hierarchical clustering. The result returned by this function can be displayed as a colored dendrogram where colors are defined by a group variable.

# Function interface #

`colhclust(Object, labels, color)`
  * `Object`: an object of class `Dataset`.
  * `labels`: a character string specifying the column name of sample metadata that contains the labels to annotate the leafs of the tree.
  * `color`: a character string specifying the column name of sample metadata that contains a grouping variable based on which leafs are colored.

# Available signatures #

  1. `colhclust("Dataset", "character", "character")` i.e. all arguments are supplied. Entries from the sample metadata column specified by `labels` will be displayed for leaf labels. The number of distinct colors equal to the number of levels in the sample metadata column specified by `color` are used for the display of leaf labels.
  1. `colhclust("Dataset", "missing", "character")` i.e. the argument `labels` can be missing while `Object` and `color` are present. In this case, the sample names are used as labels.

# Examples #
If `dataset` is an object of `Dataset` class with columns `"SampleLabels"` and `"DiseaseGroup"` among the columns of sample metadata, the following code examples can be used
  1. to draw a hierarchical clustering dendrogram with custom labels to leaf nodes and coloring based on the `"DiseaseGroup"`
```
hc <- colhclust(dataset, "SampleLabels", "DiseaseGroup")
plot(hc)
```
  1. to draw a hierarchical clustering dendrogram with default labels to leaf nodes, i.e. `sampleNames(dataset)`, and coloring based on the `"DiseaseGroup"`
```
hc <- colhclust(dataset, color="DiseaseGroup")
plot(hc)
```