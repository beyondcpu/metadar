# Introduction #

The functions `log`, `log2`, and `log10` can be used to transform a `Dataset` object into a log-scale.

# Function interface #
`log(x, base=exp(1))`

`log2(x)`

`log10(x)`
  * `x`: an object of class `Dataset` (or numeric or a complex vector as in `log` function of the R `base` package)
  * `base`: a positive or complex number: the base with respect to which logarithms are computed. Defaults to e=exp(1).

# Result #
The return value is the object of the same class as the input argument i.e. if `x` was a `Dataset` object, then the result of the log transformation function is also a `Dataset` object.