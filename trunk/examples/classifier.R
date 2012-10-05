library("Biobase")
library("pROC")
library("MASS")
library("subselect")
library("glmnet")
source("~/Dropbox/Work/METADAR/Dataset.R")
source("~/Dropbox/Work/METADAR/Classifier.R")
source("~/Dropbox/Work/METADAR/LogisticRegression.R")
source("~/Dropbox/Work/METADAR/RandomForest.R")

dat <- new("Dataset", "~/Dropbox/Work/varp/Sample_DiscTraining_alvs.csv")

lr <- new("LogisticRegression", dat, "Class", dat, "Class", selectedVariables=c("6", "7"))
lr$subselect(method="lasso")
lr$trainingStatistics()
?