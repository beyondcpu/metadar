# Introduction #

Class _PCA_ of metadar package provides an interface for performing principal component analysis on a _Dataset_ object. It is implemented as a Reference class.

# Usage #

Let _dataset_ be an object of class _Dataset_ that you have [created](ReadingTheData.md). In order to perform PCA, the most minimal command that is required is the following:
```
pc <- new("PCA", dataset)
```
This minimal call would in turn call _prcomp_ function of _stats_ package with its default function arguments to compute the PCA on the _dataset_. The most typically recommended set of arguments to include in the PCA call, however, are _center_ and _scale_ arguments of _prcomp_ method. With these arguments included the creation of PCA class would go as follows:
```
pc <- new("PCA", dataset, center=TRUE, scale=TRUE)
```
Please note that the argument _center_ is TRUE by default. So, dropping the _center_ argument in the above call also gives an identical result. But it is explicitly included in the above call just for clarity. Thus the following call is equivalent to the above:
```
pc <- new("PCA", dataset, scale=TRUE)
```
Please check the help document of _prcomp_ method for more details about its arguments. If you want to create a loadings plot, note that you should not set the argument retx to FALSE. The default retx=TRUE would enable us to build loadings plot from the PCA object.
# Results #
Several methods of _PCA_ class allow you to examine the results of the PCA. In what follows, it is assumed that _pc_ is the name of PCA object created by the user.

## Screeplot ##
The screeplot, the barplot which displays the variance explained by each principal component can be obtained by
```
pc$screeplot()
```
Note that additional arguments applicable to _plot_ function can be passed on to the _screeplot_ method (except the argument _main_).

## Variance explained by each component ##
Variance explained by each principal component can be queried by
```
pc$prop.var
```
Cumulative sum of variances explained by components can be obtained by using _cumsum_ R function
```
cumsum(pc$prop.var)
```

## Scores plot ##
The scores plot can be created by _scoresplot_ function. The resulting two dimensional plot shows the projection of samples of the _dataset_ onto two  specified principal components. This function accepts the following arguments:
  * `annotation`: This is an optional argument. When present, it has to be the name of a column in the sample metadata (i.e. pData(dataset)) whose contents you want to display next to the points in the scores plot e.g. if `annotation="SampleName"`, the sample names are displayed in the plot.
  * `color`: This is a mandatory argument. It has to be the name of a column in the sample metadata (i.e. pData(dataset)) whose contents you want to use to colour the points. For example, if there is a column called `DiagnosticGroup` in the sample metadata, then `color="DiagnosticGroup"` will result in the scoresplot with as many colors as there are diagnostic groups presented in this column.
  * `pcs`: This is a mandatory argument. It has to be an integer vector of length 2 and should specify which components you want to plot. e.g. to plot PC1 and PC2, use pcs=c(1,2); to plot PC2 and PC3, use pcs=c(2,3).
  * `labelpos`: This is an optional argument. When provided, it specifies the position of the label next to the points in the plot. It should be one of 1, 2, 3, or 4 which correspond to the text display below, to the left of, above or to the right of each point. The default value is 3. Therefore, by default the labels are shown above the points. Its value is passed on as _pos_ argument for the R function _text_.
  * `...`: additional arguments for R function _plot_ can be passed. Please note that the _plot_ arguments _xlab, ylab, main, pch, col, xlim, and ylim_ can not be passed because they are already specified inside the scoresplot function definition.

### Examples ###
A minimal syntax example for generating scoresplot is
```
pc$scoresplot(color="Class", pcs=1:2)
```
An example with inline annotation of points in the scoresplot is
```
pc$scoresplot(annotation="Class", color="Class", pcs=1:2)
```
An example with a custom position of the labels
```
pc$scoresplot(annotation="Class", color="Class", pcs=1:2, labelpos=2)
```
An example where an additional argument is passed to the _plot_ function to increase the size of points
```
pc$scoresplot(annotation="Class", color="Class", pcs=1:2, labelpos=2, cex=2)
```

## Loadings plot ##

The loadings plot can be created using _loadingsplot_ function. The resulting two dimensional plot shows the projection of features of the _dataset_ onto two specified principal components. This function accepts the following arguments:
  * `annotation`: This is an optional argument. If it is not provided, the ID's (i.e. featureNames(dataset)) will used. When present, it has to be either "ID" or the name of a column in the variable metadata whose contents you want to display next to the points in the scores plot e.g. if the variable metadata has a column called "Name", then annotation="Name" will result in the display of names presented in that column next to the points in the plot.
  * `color`: This is an optional argument. It has to be the name of a column in the variable metadata whose contents you want to use to colour the points. For example, if there is a column called `CompoundClass` in the variable metadata, then `color="CompoundClass"` will result in the loadings plot with as many colors as there are compound classes presented in this column.
  * `pcs`: This is a mandatory argument. It has to be an integer vector of length 2 and should specify which components you want to plot. e.g. to plot PC1 and PC2, use pcs=c(1,2); to plot PC2 and PC3, use pcs=c(2,3).
  * `labelpos`: This is an optional argument. When provided, it specifies the position of the label next to the points in the plot. It should be one of 1, 2, 3, or 4 which correspond to the text display below, to the left of, above or to the right of each point. The default value is 3. Therefore, by default the labels are shown above the points. Its value is passed on as _pos_ argument for the R function _text_.
  * `...`: additional arguments for R function _plot_ can be passed. Please note that the _plot_ arguments _xlab, ylab, main, pch, col, xlim, and ylim_ can not be passed because they are already specified inside the scoresplot function definition.

### Examples ###
A minimal syntax example for generating loadings plot is
```
pc$loadingsplot(pcs=1:2)
```
An example with a non-default inline annotation of points in the loadings plot is
```
pc$loadingsplot(annotation="Name", pcs=1:2)
```
An example with a custom position of the labels and compound classes coloured
```
pc$loadingsplot(color="CompoundClass", pcs=1:2, labelpos=2)
```
An example where an additional argument is passed to the _plot_ function to increase the size of points
```
pc$loadingsplot(annotation="Name", color="CompoundClass", pcs=1:2, labelpos=2, cex=2)
```

## Biplot ##
The biplot can be created using _biplot_ function. The minimal call for the function can be made as
```
pc$biplot()
```
Additional arguments for biplot.prcomp method can be passed.

## Scores ##

_scores_ function can be used to retrieve the scores with respect to the selected PC's (i.e. the co-ordinates of the samples in the rotated space spanned by the selected PC's). This function accepts one argument `pcs` which specifies the components for which the scores are required. The default value of `pcs` is c(1,2) meaning that the scores of PC1 and PC2 are returned by the call with no arguments as in
```
pc$scores()
```
The following call will retrieve the scores of first three principal components
```
pc$scores(pcs=1:3)
```

## Loadings ##
_loadings_ function can be used to retrieve the loadings with respect to the selected PC's (i.e. the co-ordinates of the features in the rotated space spanned by the selected PC's). This function accepts one argument `pcs` which specifies the components for which the loadings are required. The default value of `pcs` is c(1,2) meaning that the loadings of PC1 and PC2 are returned by the call with no arguments as in
```
pc$loadings()
```
The following call will retrieve the loadings of first three principal components
```
pc$loadings(pcs=1:3)
```