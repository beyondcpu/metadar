#### Here is an example of some simple data analysis of
#### one data set which had 24 samples from
#### two phenotypes measured under two time points
#### we are interested in the difference between
#### the case versus control phenotype at each time point

library("Biobase")
source("Dataset.R")
dat <- new("Dataset")
dat <- readDataset(dat, "Lipidomics_normalized.csv",
                   "Lipidomics_pdata2.csv")
### take a subset of the data (in this case we want to analyze each time point separately)
library("simpleaffy")
dat.3d <- get.array.subset(dat, "Time", "3d")
dat.24h <- get.array.subset(dat, "Time", "24h")

### Perform t-test
tt.3d <- univariateTTest(dat.3d, "Phenotype")
tt.24h <- univariateTTest(dat.24h, "Phenotype")
