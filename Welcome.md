# Introduction #

Welcome! This package consists of many convenience functions to perform a variety of statistical analysis of metabolomics data. The methods are not limited to metabolomics data alone and should be applicable to any numerical data matrix, but the most important data that I have access to is metabolomics. The main purpose of writing this package is to make my own life easy in my daily routine of data analysis. [Here](Installation.md) are the instructions for installing this package. This package wouldn't exist without R community. I am obviously [indebted](Thanks.md) to the developers of R, Bioconductor and developers of the many R packages!

Capabilities of this package are listed here.

## Data import and processing ##
  * [Input of metabolomics data sets](ReadingTheData.md) (well, any numerical data set will work) and their meta data in Bioconductor `ExpressionSet` format.
  * [Removing](NAOmit.md) the peaks with missing measurements (i.e. removing NA's)
  * [Filtering](ZeroFiltering.md) zero intensity peaks
  * [Zero imputation](ZeroImputation.md)
  * [Transformation](LogTransform.md) of data into log scale

## Univariate statistics & statistical hypothesis tests ##
  * [One-way anova and post-hoc test](OneWayAnova.md) (Tukey Honestly Significant Differences).
  * [Two-way anova and post-hoc test](TwoWayAnova.md) (Tukey Honestly Significant Differences).
  * [Student's T-test](TTest.md) and [Wilcoxon rank sum or signed rank test](Wilcoxon.md) for each variable in the data set.
  * [Mean values and standard errors of mean](MeanSem.md) for each variable in two or more sample groups.
  * [Mean values and 95% CI of mean](MeanCI.md) for each variable in two or more sample groups.
  * [Median values and 95% CI of median](MedianCI.md) for each variable in two or more sample groups.
  * [Fold changes](FoldChanges.md) for each variable in the data set.
  * [Area under the ROC curve](UnivariateAUC.md) for each variable in the data set.
  * [Correlation coefficient](UnivariateCorrelation.md) of each variable with a selected variable of interest.
  * [All pairs correlation](AllPairsCorrelation.md): correlation between every pair of variables in a data set & correlation between variables in two data sets (i.e. every variable in data set 1 versus every variable in data set 2).
  * [All pairs t-test](AllPairsTTest.md): Students t-test for every variable in a data set with respect to every variable in another data set containing binary variables.
## Graphics ##
  * [Box plots](BoxPlots.md) and [bean plots](BeanPlots.md)
  * [Hierarchical clustering dendrogram](ColoredHClust.md)
## Classification & regression models ##
  * A classification model using logistic regression
  * A classification model using random forest
  * Evaluation of prediction performance of a logistic regression using cross validation
  * Evaluation of prediction performance of a random forest model using CV
## Multivariate analysis ##
  * [Principal component analysis](PCAClass.md)
  * [Model based clustering](Mclustering.md) using **mclust**