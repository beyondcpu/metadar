## ERROR in univariateTTest(): data are essentially constant ##

### ERROR message ###

`Error in if (stderr < 10 * .Machine$double.eps * max(abs(mx), abs(my))) stop("data are essentially constant"): missing value where TRUE/FALSE needed` when running `univariateTTest()`

### FIX ###
This error occurred when a feature in a `Dataset` object has Inf or -Inf as values and I tried to perform t-test on it. In my case it was because I tried to transform the data into log-scale and the original values had zeros and the fix was to simply get rid of zeros and then log transform