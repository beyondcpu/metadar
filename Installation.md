# Prerequisites #
  * A computer with R installed (http://www.r-project.org)
  * R packages from CRAN: randomForest, ROCR, gplots, beanplot, subselect, glmnet, mclust, pROC, plyr, MASS. Run the following command in R to install them:
```
install.packages(c("randomForest", "ROCR", "gplots", "beanplot", "subselect",
   "glmnet", "mclust", "pROC", "plyr", "MASS"), dep=TRUE)
```
  * R packages from Bioconductor: Biobase and simpleaffy. Run the following commands in R to install them:
```
source("http://www.bioconductor.org/biocLite.R")
biocLite(c("Biobase", "simpleaffy"), dep=TRUE)
```
  * R package [ihm](http://code.google.com/p/ihm), whose installation instructions are [here](http://code.google.com/p/ihm/wiki/Installation).

# Downloading the package #

The links for downloading the latest version of the package are on [the home page](https://code.google.com/p/metadar). There are binary versions available for Linux and Windows. Choose the latest version appropriate for your platform. I built the Linux binary on my computer. The windows build was then created using the excellent site, http://win-builder.r-project.org/. Many thanks to Uwe Liggis (and others, if any) for maintaining it. I think the Linux version is suitable for all non-Windows operating systems (please test and correct me if I am wrong).

# Installation #

  * Download the package binary to your computer
  * If you are on Linux: open a command terminal, change to the directory where the package binary was downloaded, and run the following command to install:
```
R CMD INSTALL metadar_<version>.tar.gz
```
  * If you are on Windows: use the R gui menu "install package for local zip" and point to the downloaded zip file. Or open a command prompt window and issue the following command at the command prompt:
```
R CMD INSTALL metadar_<version>.zip
```


# Notes #

  * I have tested the installation as well as functioning of the package in Linux (Ubuntu, Debian and Fedora). Please test the windows build if you use Windows. If you use some other OS (unix-like), please try the Linux version and let me know if there are problems. I expect the Linux version to work on Mac OS X.
  * On Windows 7 several people that I know had issues with installation of packages due the write permissions. When you want to install packages, please try to run R as administrator and then try the installation. This has worked on the computers I checked.