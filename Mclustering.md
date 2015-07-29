# Introduction #

Class _Mclust2_ of _metadar_ package provides an interface for performing model based clustering ([1](Mclustering#References.md)) on a _Dataset_ object.


# Usage #

Let _dataset_ be an object of class _Dataset_ that you have created ([2](Mclustering#References.md)). There are two steps to applying mclust to _dataset_. The first step is to create an Mclust2 object.
```
mcl <- new("Mclust2", dataset)
```
The second step is to perform clustering. You can cluster either variables or samples.
```
mcl <- mcl$mclust2("variables", normalization=TRUE, G=5:15)
```
<p>In the above call the function argument <code>normalization</code> allows you to specify whether or not to normalize the entities that are being clustered so that their means are set to zero and variances to 1. In the above call we are clustering <code>variables</code> (i.e. features of our dataset). Since we have set normalization to be TRUE, the function will first normalize the variables to have zero mean and unit variance.</p>
<p>The second argument G is used to specify the number of clusters to try. In the example above, we are trying 5 clusters, 6 clusters, 7 clusters and so on until 15 clusters. The mclust method will then choose the best number of clusters from among these tested number of clusters based on the maximum likelihood. Often, you receive a warning saying that the optimum number of clusters occurred at <code>min</code> or <code>max</code> cluster numbers tried. If the optimum was <code>min</code>, we should try smaller numbers of clusters to check if there is a better model (e.g. by G=2:6 in the example above). If the optimum was at <code>max</code>, we should try larger number of clusters to check if there is a better model (e.g. by G=13:20 in the example above).</p>
Similarly as for variables, clustering samples can be performed by for example
```
mcl <- mcl$mclust2("samples", normalization=TRUE, G=2:10)
```

# Results #

Several methods of _Mclust2_ class allow you to examine the results of clustering.

## Number of clusters ##

You can get the number of clusters by
```
mcl$getNumberOfClusters()
```

## Members of clusters ##

You can get the number and list of elements in each cluster by
```
mcl$getClusterInfo()
```
You can get the elements of a specific cluster by
```
mcl$getClusterMembers(clusters=1)
```
The `getClusterMembers` function has two arguments, namely, variableNames and clusters. Both are optional. If no argument is issued, then it returns elements of all clusters as a list, with the default variable names. If the argument clusters is provided, it should be the number(s) of cluster(s) whose elements you want listed. The variableNames argument can be used to supply variable names to be shown. This argument has been made available for the following reasons. If the clustering of variables (i.e. features in the dataset) was performed, then _Mclust2_ class takes the featureNames(dataset) as the variable names. They may often be some ID such as just a number, which is not as informative as a name could be. In such case, variableNames argument allows you to provide a list of informative names (of the same length as there are variables). If the clustering was performed on samples of the dataset, then sampleNames(dataset) become the default variableNames. But often, showing the diagnostic class (or the like) of the sample maybe more informative than its name.

## Heatmap visualization of clusters ##

You can draw the heatmap visualization of clusters by using the method
```
mcl$drawClusterHeatmaps()
```

Note that the above command writes the heatmaps to PDF files rather than showing them in X11() window. It creates the following three heatmaps:
  1. The heatmap of variables in their original order (i.e. in the order they appear in the data set with no reordering). The name of the file to which this heatmap written to is `ihm_orig.pdf` by default.
  1. The heatmap of variables with hierarchical clustering. The name of the file to which this heatmap is written to is `ihm_hclust.pdf` by default.
  1. The heatmap of variables with the model based clusters marked in it. The name of the file to which this heatmap is written to is `ihm_mclust.pdf` by default.

The `drawClusterHeatmaps` function accepts an argument `filenames` to specify alternative file names if preferred. It has to be a character vector specifying three pdf file names corresponding to the three heatmaps described above. The default value is `filenames=c("ihm_orig.pdf", "ihm_hclust.pdf", "ihm_mclust.pdf")`.

The function `drawClusterHeatmaps` uses [http://code.google.com/p/ihm](ihm.md) to draw each heatmap. It is possible to pass additional arguments for the customization of heatmap via `drawClusterHeatmaps` to `ihm` function.

## Cluster means and variances ##
You can get the cluster centroids (means) using the following command
```
mcl$getClusterMeans()
```
Cluster variances can be obtained by
```
mcl$getClusterVariances()
```
# References #
  1. [mclust package](http://www.stat.washington.edu/mclust/)
  1. [Instructions for creating Dataset objects](https://code.google.com/p/metadar/w/edit/ReadingTheData)