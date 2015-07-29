# Introduction #

The Dataset class is the main data type of this package. Many functions of the package are operations on a Dataset object. A Dataset object can be created by calling the "new" function.

# Function interface #

new(Object, metabolomicsDataSource, sampleMetaDataSource, variableMetaDataSource, ...)

# Available signatures #

  1. `new("Dataset", "missing", "missing", "missing")`: all three input arguments can be missing
  1. `new("Dataset", "character", "missing", "missing")`: metabolomicsDataSource can be a character string giving the path to a CSV file that has the data. The path can be provided as a full path or relative to the current working directory. If the file is in the working directory, the name of the file alone is sufficient. The sampleMetaDataSource and variableMetaDataSource can be missing. Please check the [file and data formats](FileFormats.md) page for instructions on expected formats of input data.
  1. `new("Dataset", "data.frame", "missing", "missing")`: metabolomicsDataSource can be a data.frame. The sampleMetaDataSource and variableMetaDataSource can be missing. Please check the [file and data formats](FileFormats.md) page for instructions on expected formats of input data.
  1. `new("Dataset", "ExpressionSet", "missing", "missing")`: metabolomicsDataSource can be an `ExpressionSet` object, and sampleMetaDataSource and variableMetaDataSource missing.
  1. `new("Dataset", "character", "character", "missing")`: both metabolomicsDataSource and sampleMetaDataSource can be character strings, giving the path to two CSV files (data file and sample meta data file respectively). The variableMetaDataSource can be missing. Please check the [file and data formats](FileFormats.md) page for instructions on expected formats of input data.
  1. `new("Dataset", "data.frame", "data.frame", "missing")`: both metabolomicsDataSource and sampleMetaDataSource can be data.frame objects containing data and meta data respectively and variableMetaDataSource missing. Please check the [file and data formats](FileFormats.md) page for instructions on expected formats of input data.
  1. `new("Dataset", "character", "character", "character")`: metabolomicsDataSource, sampleMetaDataSource, and variableMetaDataSource can be character strings, giving the path to three CSV files (data file, sample meta data file, and variable meta data file respectively). Please check the [file and data formats](FileFormats.md) page for instructions on expected formats of input data.
  1. `new("Dataset", "data.frame", "data.frame", "data.frame")`: metabolomicsDataSource, sampleMetaDataSource, and variableMetaDataSource can be data.frame objects containing data and sample meta data, and variable meta data respectively. Please check the [file and data formats](FileFormats.md) page for instructions on expected formats of input data.

# Examples of usage #
  1. To create an empty Dataset. Perhaps not practically useful
```
dat <- new("Dataset")
```
  1. Creation of a Dataset from a single input file
```
dat <- new("Dataset", "/home/xyz/data.csv")
```
  1. Creation of a Dataset from a single data.frame object
```
dat <- new("Dataset", data.object)
```
  1. Creation of a `Dataset` object using an `ExpressionSet` object
```
dat <- new("Dataset", ExpressionSet.Object)
```
  1. Creation of a Dataset object using two input files (data and sample meta data files)
```
dat <- new("Dataset", "/home/xyz/data.csv", "/home/xyz/smetadata.csv")
```
  1. Creation of a Dataset object using two data.frame objects (data and sample meta data objects)
```
dat <- new("Dataset", data.object, smetadata.object
```
  1. Creation of a Dataset object using three input files (data, sample meta data, and variable meta data files)
```
dat <- new("Dataset", "/home/xyz/data.csv", "/home/xyz/smetadata.csv", "/home/xyz/vmetadata.csv")
```
  1. Creation of a Dataset object using three data.frame objects (data, sample meta data, and variable meta data objects)
```
dat <- new("Dataset", data.object, smetadata.object, vmetadata.object)
```

# Notes #
By default, the CSV files are assumed to have comma (,) as the delimiter and period (.) as the decimal symbol. If the CSV file has semicolon (;) as the field separator (delimiter) and comma (,) as the decimal symbol, please use something like:

```
dat <- new("Dataset", "/home/xyz/data.csv", sep=";", dec=",")
```

This is useful for the CSV files in the European format.