#' Compile Results & Generate a Report
#'
#' @param source_file_dir The folder containing the source MassHunter .xlsx files
#' @param results_files_dir The folder in which to put results. Defaults to \code{source_file_dir}/results/
#' @param report_template The report template. A default template is included in the package under /rmd/genericreport.rmd
#' @param heteroatoms The list of atoms and heteroatoms to check for in the 'all atoms' table. Must include C, H. Defaults to all atoms
#' @param write_pdf Whether to generate a pdf report
#' @param write_csv Whether to generate csv tables
#' @param verbose Verbosity level: 0 = silent, 1 = normal, 2 = verbose
#' @inheritDotParams single_report
#'
#' @export
compile_reports <- function(source_file_dir, results_files_dir = NULL, report_template = NULL, heteroatoms = GCMSResultsCompiler::elements, write_pdf = TRUE,
    write_csv = TRUE, verbose = 1, ...) {

  #Checks before stopping
  if (!write_pdf && !write_csv){
    stop("Must generate either PDF or CSV files.")
  }

  #Find files
  filelist <- list.files(source_file_dir, pattern = "*.xls*")
  if(length(filelist)==0){
    stop(paste("No .xls(x) files found in source_file_dir:", source_file_dir))
  }
  if (verbose == 2){
    print(paste0("Found ", length(filelist), " files to process."))
  }

  if(is.null(results_files_dir)){
    results_files_dir<-file.path(source_file_dir, 'results')
  }

  if(!dir.exists(results_files_dir)){
    print(paste0("Creating results folder ", results_files_dir))
    dir.create(results_files_dir)
  }

  element_list<-check_heteroatoms(heteroatoms)

  for (results_file in filelist){
    source_file<-file.path(source_file_dir, results_file)
    if(verbose == 2){
      print(paste0("Trying to process ", results_file, "."))
    }
    tryCatch(
      single_report(source_file, results_files_dir, report_template, element_list, write_pdf, write_csv, verbose = verbose, ...),
      error = function(e) print(paste0("Error in generating report file from ", results_file, ".\nError message: ", e)),
      warning = function(w) print(paste0("Warning in generating report file from", results_file, ".\nWarning message: ", w))
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

  element_list<-check_heteroatoms(element_list)

  results <- as.data.frame(readxl::read_excel(results_file, col_names = FALSE))
  ctrow <- which(results[, 1] == "Compound Table")
  if(!(ctrow > 0)){
    stop("Could not find Compound Table in source file.")
  }

  header <- results[1:(ctrow - 1), ]
  if(nrow(header) == 0){
    stop("Could not find header information in source file.")
  }

  compound_table <- results[(ctrow + 1):nrow(results), ]

  header <- Filter(function(x) !all(is.na(x)), header)
  compound_table <- Filter(function(x) !all(is.na(x)), compound_table)

  header_vals <- data.frame(key = character(), value = character())


  for (i in seq(1, ncol(header), by = 2)) {
    h <- unname(header[, i:(i + 1)])
    h <- h[stats::complete.cases(h), ]
    colnames(h) <- c("key", "value")
    header_vals <- rbind(header_vals, h)
  }

  if (nrow(header_vals) == 0){
    stop("Header information not found in source file.")
  }

  rm(header, h, results)

  data_file_name <- extract_key_value(header_vals, "Data File")
  sample_id <- extract_key_value(header_vals, "Sample Name")
  sample_name <- extract_key_value(header_vals, "Comment")
  if(verbose == 2){
    print(paste0("Processing sample ", sample_name," results."))
  }
  analysis_date <- extract_key_value(header_vals, "Acquired Time")
  acquisition_method <- extract_key_value(header_vals, "Acq Method")
  data_analysis_method <- extract_key_value(header_vals, "DA Method")
  operator_name <- extract_key_value(header_vals, "User Name")

  #fixing usernames to not mess up tex
  operator_name <- stringr::str_replace_all(operator_name, "\\\\", "_")  #four backslashes because escapes suck
  operator_name <- stringr::str_replace_all(operator_name, "/", "_")

  colnames(compound_table) <- compound_table[1, ]
  compound_table <- compound_table[2:(nrow(compound_table) - 2), ]
  compound_table <- compound_table[compound_table$RT != "RT", ]

  V_element_count <- Vectorize(element_count, vectorize.args = c("formula"), SIMPLIFY = TRUE)

  for (i in 1:length(element_list)) {
    compound_table[element_list[i]] <- V_element_count(formula = compound_table$`Molecular Formula`, element = element_list[i])
  }

  compound_table$Area <- as.numeric(compound_table$Area)
  total_area <- sum(compound_table$Area, na.rm = TRUE)

  # by_all <- compound_table %>%
  #   dplyr::select(one_of(c(element_list, 'Area'))) %>%
  #   dplyr::group_by(one_of(element_list)) %>%
  #   dplyr::summarise(Area = sum(Area)) %>%
  #   dplyr::mutate(Area.Percent = Area/total_area*100) %>%
  #   dplyr::arrange(C, H, Area)
  #
  # by_c <- compound_table %>%
  #   dplyr::select(one_of(c(element_list, 'Area'))) %>%
  #   dplyr::group_by(C) %>%
  #   dplyr::summarise(Area = sum(Area)) %>%
  #   dplyr::mutate(Area.Percent = Area/total_area*100) %>%
  #   dplyr::arrange(C)
  #
  # by_ch <- compound_table %>%
  #   dplyr::select(one_of(c(element_list, 'Area'))) %>%
  #   dplyr::group_by(C, H) %>%
  #   dplyr::summarise(Area = sum(Area)) %>%
  #   dplyr::mutate(Area.Percent = Area/total_area*100) %>%
  #   dplyr::arrange(C, H)

  #not great practice, but clears cran
  C<-H<-Area<-NULL

  by_c <- dplyr::select(compound_table, dplyr::one_of(c(element_list, 'Area')))
  by_c <- dplyr::group_by(by_c, C)
  by_c <- dplyr::summarise(by_c, Area = sum(Area))
  by_c <- dplyr::mutate(by_c, Area.Percent = Area/total_area*100)
  by_c <- dplyr::arrange(by_c, C)

  by_ch <- dplyr::select(compound_table, dplyr::one_of(c(element_list, 'Area')))
  by_ch <- dplyr::group_by(by_ch, C, H)
  by_ch <- dplyr::summarise(by_ch, Area = sum(Area))
  by_ch <- dplyr::mutate(by_ch, Area.Percent = Area/total_area*100)
  by_ch <- dplyr::arrange(by_ch, C, H)

  by_all <- dplyr::select(compound_table, dplyr::one_of(c(element_list, 'Area')))
  by_all <- dplyr::select(by_all, which(!colSums(by_all, na.rm=TRUE) == 0))
  by_all <- dplyr::group_by_at(by_all, dplyr::vars(names(by_all)[names(by_all) %in% element_list]))
  by_all <- dplyr::summarise(by_all, Area = sum(Area))
  by_all <- dplyr::mutate(by_all, Area.Percent = Area/total_area*100)
  by_all <- dplyr::arrange(by_all, C, H, Area)

  element_included_list <- colnames(by_all)[1:(length(colnames(by_all)) - 2)]

  if(write_pdf){
    rmarkdown::render(input = system.file("rmd/genericreport.rmd", package = "GCMSResultsCompiler"), output_format = "pdf_document",
                      output_file = paste(sample_id, "_breakdown.pdf", sep = ""), output_dir = results_files_dir,
                      params = list(set_title = paste0(sample_id, " Breakdown Report"), set_date = Sys.Date()), quiet = !(verbose == 2))
    if(verbose==1){
      print(paste0("PDF created: ", file.path(results_files_dir, paste0(sample_id, "_breakdown.csv"))))
    }
  }

  if(write_csv){
    utils::write.csv(x = by_c, file = file.path(results_files_dir, paste0(sample_id, "_by_c.csv")), row.names = FALSE)
    if(verbose == 2){
      print(paste0("Output created: ", file.path(results_files_dir, paste0(sample_id, "_by_c.csv"))))
    }
    utils::write.csv(x = by_ch, file = file.path(results_files_dir, paste0(sample_id, "_by_ch.csv")), row.names = FALSE)
    if(verbose == 2){
      print(paste0("Output created: ", file = file.path(results_files_dir, paste0(sample_id, "_by_ch.csv"))))
    }
    utils::write.csv(x = by_all, file = file.path(results_files_dir, paste0(sample_id, "_by_all.csv")), row.names = FALSE)
    if(verbose == 2){
      print(paste0("Output created: ", file.path(results_files_dir, paste0(sample_id, "_by_all.csv"))))
    } else if (verbose == 1) {
      print(paste0("CSV files created for sample: ", sample_id))
    }

  }

}
