library("Biobase")
source("~/Dropbox/Work/METADAR/Dataset.R")
library("ROCR")
source("~/Dropbox/Work/METADAR/ModelValidationCV.R")
source("~/Dropbox/Work/METADAR/LogisticRegressionModelValidationCV.R")
source("~/Dropbox/Work/METADAR/RandomForestModelValidationCV.R")

### If you have only training data
dat <- new("Dataset", "~/Dropbox/Work/varp/Sample_DiscTraining_alvs.csv")
lrmv <- new("LogisticRegressionModelValidationCV", dat, "Class", cv=100, selectedVariables=c("6", "7", "8"))
lrmv$buildCV()
lrmv$plotROC("example")

library("randomForest")
rfmv <- new("RandomForestModelValidationCV",
            dat, "Class",
            dat, "Class", cv=100,
            selectedVariables=featureNames(dat))
rfmv$buildCV() ### needs only training data
rfmv$buildCV2() ### needs training and test
rfmv$plotROC("example")
