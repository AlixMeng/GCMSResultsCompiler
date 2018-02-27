# GCMSResultsCompiler

The goal of GCMSResultsCompiler is to quickly turn an excel file output from Agilent MassHunter and recompile the results to tables, showing area percent by carbon count, by carbon and hydrogen count, and by unique molecular count. 

## Example

This is the typical usage:

``` r
library(GCMSResultsCompiler)
compile_results("source_file_dir", "results_files_dir")
```

More complex inputs and outputs will eventually be able to be designated.
