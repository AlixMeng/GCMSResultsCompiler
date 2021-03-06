% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReportCompiler.R
\name{single_report}
\alias{single_report}
\title{Generate a Report from a Single File}
\usage{
single_report(results_file, results_files_dir, report_template, element_list,
  write_pdf, write_csv, verbose = 1, ...)
}
\arguments{
\item{results_file}{The file from which to extract data}

\item{results_files_dir}{The directory to save output data}

\item{report_template}{The template for the report}

\item{element_list}{The list of elements to check for}

\item{write_pdf}{Binary: write a pdf file of the tables}

\item{write_csv}{Binary: write the tables into a csv file (one each)}

\item{verbose}{Level of information output: 0 - none, 1 - normal, 2 - high.}

\item{...}{Additional parameters to pass}
}
\description{
Most commonly called from compile_reports, when using a directory of report files to process.
}
