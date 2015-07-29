# Introduction #

The function `getSampleMetaData` allows you to access sample metadata.

# Function interface #

`getSampleMetaData(Object, metaDataColumns, selectedSamples)`
  * `Object`: an object of class `Dataset`.
  * `metaDataColumns`: a character vector specifying the metadata columns to retrieve.
  * `selectedSamples`: a character vector specifying the names of samples for which the metadata would be retrieved.

# Available signatures #

  1. `getSampleMetaData(Object="Dataset", metaDataColumns="missing", selectedSamples="missing")` i.e. only `Object` is supplied. When invoked like this, this function returns the whole of the sample metadata as a data.frame object.
  1. `getSampleMetaData(Object="Dataset", metaDataColumns="character", selectedSamples="missing")` i.e. `Object` and `metaDataColumns` are supplied. When invoked like this, if the length of `metaDataColumns` is more than one, the result is a data.frame object. If its length is one (i.e. if only one metadata variable is retrieved), the result is always returned as a character vector. Therefore, if the content of the query variable is known to be different from string, the result should be type-casted as required (i.e. using as.numeric or as.factor).
  1. `getSampleMetaData(Object="Dataset", metaDataColumns="character", selectedSamples="character")` i.e. all three arguments are supplied. When invoked like this, the selected variables indicated by `metaDataColumns` for the selected samples indicated by `selectedSamples` are returned. If the length of `metaDataColumns` is more than one, the result is a data.frame object. If its length is one (i.e. if only one metadata variable is retrieved), the result is always returned as a character vector. Therefore, if the content of the query variable is known to be different from string, the result should be type-casted as required (i.e. using as.numeric or as.factor).
  1. `getSampleMetaData(Object="Dataset", metaDataColumns="missing", selectedSamples="character")` i.e. `Object`and `selectedSamples` are supplied. When invoked like this, all the metadata of the selected samples indicated by `selectedSamples` are returned as a data.frame object.