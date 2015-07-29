# Introduction #

This document introduces how the input data should be provided to [create a Dataset object](ReadingTheData.md). In this document, the arguments _metabolomicsDataSource_, _sampleMetaDataSource_, and _variableMetaDataSource_ are referred to as _data_, _sample metadata_ and _variable metadata_ respectively. A Dataset object can be created from a [single input](FileFormats#Content_of_the_data_when_no_meta_data_is_present.md), two inputs (one for [data](FileFormats#Content_of_the_data_when_meta_data_is_present.md) and the other for [sample meta data](FileFormats#Content_of_the_sample_metadata.md)), or three inputs (one for [data](FileFormats#Content_of_the_data_when_meta_data_is_present.md), other for [sample meta data](FileFormats#Content_of_the_sample_metadata.md), and still another for [variable meta data](FileFormats#Content_of_the_variable_metadata.md). The sources providing the data or meta data can be either all files or all data.frame objects. If the data is provided via files, then comma separated values (CSV) file format should be used. When the data is provided as a CSV file, the first row of the CSV file contains the names (i.e. titles) of the columns. In contrast, when the data is provided as a data.frame object, it has them as its column names. Other than this, the rules of the data content and titles are identical for CSV files and data.frame objects.

## Example CSV files (OR the long story cut short) ##

The quickest way to learn the data file format is to take a look at the example files. So, before giving long explanation about the data files, I point you to some example files.

  * If you are going to create a _Dataset_ object using one single input file, it should look like [this](http://code.google.com/p/metadar/source/browse/trunk/examples/egData_standalone.csv). Here is [another example](http://code.google.com/p/metadar/source/browse/trunk/examples/egData_standalone2.csv) of a standalone data file which contains a continuous sample metadata variable.
  * If you are going to create a _Dataset_ object using two input files, then here are example [data file](http://code.google.com/p/metadar/source/browse/trunk/examples/egData1.csv) and [sample metadata file](http://code.google.com/p/metadar/source/browse/trunk/examples/egSampleMetaData.csv).
  * When creating a _Dataset_ object using two input files, it is possible to include _variable metadata_ in the _data_ file. Here is an example [data file](http://code.google.com/p/metadar/source/browse/trunk/examples/egData2.csv) that contains some _variable metadata_. Note that all the columns that do not match sample names provided in the _sample metadata_ file will be considered as _variable metadata_ columns.
  * Finally, if you are creating the _Dataset_ object using three input files, then the files should look like [this data file](http://code.google.com/p/metadar/source/browse/trunk/examples/egData1.csv), [this sample metadata file](http://code.google.com/p/metadar/source/browse/trunk/examples/egSampleMetaData.csv) and this [variable metadata file](http://code.google.com/p/metadar/source/browse/trunk/examples/egVarMetaData.csv).

Note that all entries in these example files are fictional! I just created some random entries to demonstrate how the data should look like.

Having given the examples for a quick start, I will move on to giving the details more formally below.

## CSV file delimiter and decimal symbol ##

The input data file should be a delimited text file. By default the delimiter is assumed to be a comma (,). But the delimiter can be a semicolon (;) or a tab (\t) etc. When the delimiter is not the default comma (,), then an argument `sep` must be used when [creating the Dataset](ReadingTheData.md). Another related aspect is the decimal symbol. By default, the decimal symbol is assumed to be a period (.). If it is different (e.g. a comma), then an argument `dec` must be used when [creating the Dataset](ReadingTheData.md). Please check the help document of the R function `read.table` for more details.

# The content of the data source #

The data source must contain a column titled `ID`. The `ID` column is not allowed to have duplicate entries. Each row corresponds to one feature (typically one metabolite). The `ID` entry in each row is the unique ID for that feature. It can be any string (i.e. it is composed of [alphanumeric](http://en.wikipedia.org/wiki/Alphanumeric) characters). The remaining columns of the data source usually correspond to biological samples. The entry that corresponds to a feature _i_ and a sample _j_ represents the measured level of the feature in that sample, and it must be a number. The data source must always contain `ID` column and a non-empty set of samples, and the sample columns should have the sample names as their titles. The sample names should not have duplicates. Note, however, that the data input will be successful even if the sample names have duplicates and R simply appends .1, .2 etc at the end of any duplicated names to make them unique. But it is recommended that care is taken so that sample names do not have any duplicates. Especially, when the _sample metadata_ is provided in a separate input, the sample names must all be distinct for a successful input of _sample metadata_.

When the _Dataset_ object is created from two inputs the _data_ source may have additional columns (i.e. columns other than `ID` and samples) providing variable (i.e. feature) meta data. No additional columns are allowed when the _Dataset_ is created from a single input.

## Content of the data when no meta data is present ##

When the _Dataset_ is created from a single input file, the file must contain in its second row, one single meta data variable. (NOTE: if the _Dataset_ is created from a single input data.frame object, this corresponds to the first row in the data.frame). If the data is from a case control study, this metadata variable is typically the class variable that designates each sample as a case or control. If the data contains samples from several groups (e.g. healthy controls and several sub types of a disease), then this second row typically indicates the group of the samples (i.e. designates each column to be a control, or disease type1, disease type2 and so on). This single meta data variable can also be a real number (e.g. fasting glucose level).

This data file is not allowed to have any other columns than `ID` and the sample columns. Also, the `ID` column must be the first column in the data file.

Note that when the [Dataset object is created](ReadingTheData.md) from the single input, the single sample metadata variable provided in the file goes into the sample metadata field of the Dataset object, and the variable will always be called "Class" irrespective of whether it actually is a class variable (i.e. categorical variable) or a real number.

## Content of the data when meta data is present ##

When the _Dataset_ is created from two input sources (i.e. one corresponding to the data and the other corresponding to the sample metadata), then all the rows of the data source (except the column headers) must correspond to the features of the data set. The `ID` column can be anywhere (even though most commonly it is in the first column). If there are any columns other than the `ID` and the samples that are described in the accompanying _sample metadata_ file, they will be considered as _variable metadata_ columns unless _variable metadata_ is provided from a separate source. If _variable metadata_ is provided via a separate input source, these additional columns will be ignored.

## Content of the sample metadata ##

The purpose of the _sample metadata_ source is to provide description of each biological sample in the data set. It must contain a column titled `SampleName`, and the entries of this column must be the sample names of the data set (i.e. they should match the sample names of the _data_ source). If the `SampleName` column has any duplicate entries, the data input will fail. Typically the _sample metadata_ provides clinical variables including information such as disease group. Note that the _sample metadata_ is not allowed to include any samples that are not in the accompanying _data_. If it does, the data input will fail with an `index out of bounds error`. But, if the _data_ source has samples that are not in the _sample metadata_ source, they will be simply ignored (or interpreted as _variable metadata_).

## Content of the variable metadata ##

When given in a separate input source, the _variable metadata_ source should consist of an `ID` identical to that of _data_ and any number of other columns providing metadata of variables (i.e. features).

# Notes #

A Dataset object can hold any data, not necessarily only metabolomics or lipidomics data. The only condition is that the data values are numbers. For example, a factor variable such as the gender can be a feature in a Dataset as long as it is represented in numerical codes. The gender for instance can be coded as 0 for female and 1 for male (or vice versa).

Everything that is said about data files in the preceding sections applies also to the input data frames except the issues with delimiters, decimal separators and the column headers. That is, (i) the section corresponding to delimiters and decimal separators only concerns the file input and not the data.frame input, and (ii) when a data source is a file, the first row of the file contains the column headers and when a data source is a data.frame, the column headers are in the column names of the data.frame object.