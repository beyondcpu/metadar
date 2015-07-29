# Introduction #

The function `na.omit` removes any variables from a `Dataset` with one or more missing measurements (i.e. NA's).


# Function interface #

`na.omit(object)`
  * `object`: an object of class `Dataset` (or other R objects that `na.omit` function of R `stats` package accepts).

# Result #
If the input argument was an object of class `Dataset` the returned object is also a `Dataset` object where features that had one or more NA's are removed from it. The expression values as well as variable metadata are removed.