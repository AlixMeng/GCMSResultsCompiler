#' Compile Results & Generate a Report
#'
#' @param source_file_dir The folder containing the source MassHunter .xlsx files
#' @param results_files_dir The folder in which to put results. Defaults to \code{source_file_dir}/results/
#' @param report_template The report template. A default template is included in the package under /rmd/genericreport.rmd
#' @param heteroatoms The list of atoms and heteroatoms to check for in the 'all atoms' table. Must include C, H. Defaults to all atoms
#' @param write_pdf Whether to generate a pdf report
#' @param write_csv Whether to generate csv tables.
#' @inheritDotParams single_report
#'
#' @export
compile_reports <- function(source_file_dir, results_files_dir = NULL, report_template = NULL, heteroatoms = GCMSResultsCompiler::elements, write_pdf = TRUE,
    write_csv = TRUE, ...) {

  #Checks before stopping
  stopifnot((write_pdf || write_csv), "Must generate either PDF or CSV files.")

  #Find files
  filelist <- list.files(source_file_dir, pattern = "*.xls*")
  stopifnot(length(filelist>0), paste("No .xlsx files found in source_file_dir:", source_file_dir))

  if(is.null(results_files_dir)){
    results_files_dir<-file.path(source_file_dir, 'results')
  }

  if(!dir.exists(results_files_dir)){
    message("Creating results folder ", results_files_dir)
    dir.create(results_files_dir)
  }

  element_list<-check_heteroatoms(heteroatoms)

  for (results_file in filelist){
    tryCatch(
      single_report(results_file, results_files_dir, report_template, element_list, write_pdf, write_csv, ...),
      error = function(e) message("Error in generating report file from ", results_file, "./nError message: ", e),
      warning = function(w) message("Warning in generating report file from", results_file, "./nWarning message: ", w)
      )
  }
}


#' Generate a Report from a Single File
#'
#' Most commonly called from compile_reports, when using a directory of report files to process.
#'
#' @param results_file The file from which to extract data
#' @param results_files_dir The directory to save output data
#' @param report_template The template for the report
#' @param element_list The list of elements to check for
#' @param write_pdf Binary: write a pdf file of the tables
#' @param write_csv Binary: write the tables into a csv file (one each)
#' @param verbose Level of information output: 0 - none, 1 - normal, 2 - high.
#' @param ... Additional parameters to pass
#'
#' @export
#'
single_report<-function(results_file, results_files_dir, report_template, element_list, write_pdf, write_csv, verbose=1, ...){
  #Set verbosity
  if(verbose == 2){
    knitverbose <- TRUE
  } else {
    knitverbose <- FALSE
  }


}
