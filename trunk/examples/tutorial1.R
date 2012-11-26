#### Here is an example of some simple data analysis of
#### one data set which had 24 samples from
#### two phenotypes measured under two time points
#### we are interested in the difference between
#### the case versus control phenotype at each time point

library("Biobase")
source("Dataset.R")
dat <- new("Dataset", "Lipidomics_normalized.csv",
                   "Lipidomics_pdata2.csv")

dataset <- new("Dataset", dat1, parameters1)
featureNames(dataset)

### Filter the data by removing compounds that have too many zero's.
source("zeroFiltering.R")
dat <- zeroFiltering(dat, minNfound=5)
# OR
dat <- zeroFiltering(dat, minNfound=5, covariate="Phenotype")
# OR
dat <- zeroFiltering(dat, pctNfound=70)
# OR
dat <- zeroFiltering(dat, pctNfound=70, covariate="Phenotype")

### zero imputation
source("zeroImputation.R")
dat <- zeroImputation(dat)
# OR
dat <- zeroImputation(dat, covariate="Phenotype")

### log transform the data
dat <- log2(dat)
# OR
dat <- log10(dat)
# OR
dat <- log(dat, base=2)

### take a subset of the data (in this case we want to analyze each time point separately)
library("simpleaffy")
dat.3d <- get.array.subset(dat, "Time", "3d")
dat.24h <- get.array.subset(dat, "Time", "24h")


### Perform t-test on 3d data
tt.3d <- univariateTTest(dat.3d, "Phenotype")

### Paired t-test on 3d data
tt.3d <- univariateTTest(dat.3d, "Phenotype", paired=TRUE)

### Fold change on 3d data
fc.3d <- foldChange(dat.3d, "Phenotype", is.logged=FALSE)
# OR
fc.3d <- foldChange(dat.3d, "Phenotype", is.logged=TRUE, log.base=2)

### Fold change when samples are paired!
fc.3d <- foldChange(dat.3d, "Phenotype", paired=TRUE)
# OR
fc.3d <- foldChange(dat.3d, "Phenotype", paired=TRUE, is.logged=TRUE, log.base=2)

### Compute correlation coefficient of each variable versus one meta data variable
corrs <- univariateCorrelation(dat, covariate="Fins")
# OR
corrs <- univariateCorrelation(dat, covariate=fins) ## where fins is the numerical variable
# OR
corrs <- univariateCorrelation(dat, covariate="Fins", method="pearson")
# OR
corrs <- univariateCorrelation(dat, covariate=fins, method="pearson")

### odds ratio of each variable with respect to a binary outcome
aucs <- univariateOddsRatio(dat, "Phenotype")

### The area under the ROC curve (AUC) of each variable
aucs <- univariateAUC(dat, "Phenotype")

### Principal components analysis (PCA)
source("PCA.R")
pca(dat, annotation="Phenotype", color="Phenotype", scale=TRUE, title="pca")
### Hierarchical clustering

plot(colhclust(dat, labels="SampleName", color="Phenotype"))
### one way anova
aov1 <- oneWayAnova(dat, covariate="Class")

### two way anova
aov2 <- twoWayAnova(dat, covariate1="Class", covariate2="Time")
library("ihm")
ihm(aov2[["RatiosTable"]], aov2[["PvalsTable"]])