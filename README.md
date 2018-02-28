# GCMSResultsCompiler

The goal of GCMSResultsCompiler is to quickly turn an excel file output from Agilent MassHunter and recompile the results to tables, showing area percent by carbon count, by carbon and hydrogen count, and by unique molecular count. 

## Installation
Install this package from Github:
```r
devtools::install_github("pbulsink/GCMSResultsCompiler")
```

## Examples

This is the typical usage:

``` r
library(GCMSResultsCompiler)
compile_reports("source_file_dir", "results_files_dir")
```

More complex inputs and outputs will eventually be able to be designated.

## Build Status
[![Travis-CI Master Build Status](https://travis-ci.org/pbulsink/GCMSResultsCompiler.svg?branch=master)](https://travis-ci.org/pbulsink/GCMSResultsCompiler)

[![Travis-CI Dev Build Status](https://travis-ci.org/pbulsink/GCMSResultsCompiler.svg?branch=dev)](https://travis-ci.org/pbulsink/GCMSResultsCompiler)

[![Coverage Status](https://img.shields.io/codecov/c/github/pbulsink/GCMSResultsCompiler/master.svg)](https://codecov.io/github/pbulsink/GCMSResultsCompiler?branch=master)
