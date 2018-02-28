globalVariables(c("elements"))

#' Check Heteroatoms
#'
#' Make sure all atoms requested to be in summary table are true atoms, and not other words, or non-character. Allows for Deuterium (D) atoms.
#'
#' List of allowable elements can be seen by calling \code{GCMSResultsCompiler::elements}
#'
#' @param heteroatoms string vector of heteroatoms for by_all table.
#'
#' @return proper heteroatom list, dropping non-elements and including CH
#'
#' @keywords Internal
check_heteroatoms<-function(heteroatoms){

  if(is.null(heteroatoms)){
    return(c("C", "H", "N", "O", "S", "F", "Cl", "Br", "I", "P", "B", "Si"))
  } else {
    if(!is.character(unlist(heteroatoms))){
      stop("Heteroatoms must be a character only vector")
    }
    heteroatoms<-as.vector(heteroatoms)
    if (!c('C', 'H') %in% heteroatoms){
      message("Adding C, H to heteroatoms list")
      element_list<-c("C", "H", heteroatoms)
    }
    if (!'C' %in% heteroatoms){
      message("Adding C to heteroatoms list")
      element_list <-c("C", heteroatoms)
    }
    if (!'H' %in% heteroatoms){
      message("Adding H to heteroatoms list")
      element_list <-c("H", heteroatoms)
    }

    if(!heteroatoms %in% elements){
      element_list<-heteroatoms[heteroatoms %in% elements]
      message(paste("Removed", length(heteroatoms)-length(element_list), "non-atoms from heteroatom list:", paste0(heteroatoms[!heteroatoms %in% elements], collapse=", ")))
    }
  }
}


#' Element Counter
#'
#' A simple wrapper for str_match regex to find the number of atoms of a provided \code{element} in a chemical \code{formula}. Returns the number of atoms, or 0 if not found.
#'
#' @param formula A single molecular formula as a string (e.g. C12H26).
#' @param element The element of which you want the number of atoms.
#'
#' @return The number of atoms of the requested element in the formula
#'
#' @keywords Internal
element_count <- function(formula, element) {
    if (is.na(formula)) {
        return(0)
    }
    count <- stringr::str_match(formula, paste0(element, "([0-9]*)"))[2]
    if (is.na(count)) {
        return(0)
    } else if (count == "") {
        count <- "1"
    }
    return(as.numeric(count))
}

V_element_count <- Vectorize(element_count, vectorize.args = c("formula"), SIMPLIFY = TRUE)


#THIS LIKELY NEEDS REWORKING TO GET PROPER
summarize_table <- function(ecount, ct){#} = compound_table) {
    cut_table <- ct[ct$C == ecount[1] & ct$H == ecount[2] & ct$N == ecount[3] & ct$O == ecount[4] & ct$S == ecount[5] & ct$F ==
        ecount[6] & ct$Cl == ecount[7] & ct$Br == ecount[8] & ct$I == ecount[9] & ct$P == ecount[10] & ct$B == ecount[11] &
        ct$Si == ecount[12], ]
    return(sum(as.numeric(cut_table$Area)))
}

# loop through the file list to read in data and clean it up
dummyfunction<-function(filelist, directory, element_list, report_dir, compound_table){
for (results_file in filelist) {

    results <- as.data.frame(readxl::read_excel(paste0(directory, results_file), col_names = FALSE))

    ctrow <- which(results[, 1] == "Compound Table")

    header <- results[1:(ctrow - 1), ]
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

    rm(header, h, results)

    data_file_name <- header_vals[header_vals$key == "Data File", 2]
    sample_id <- header_vals[header_vals$key == "Sample Name", 2]
    sample_name <- header_vals[header_vals$key == "Comment", 2]
    analysis_date <- header_vals[header_vals$key == "Acquired Time", 2]
    acquisition_method <- header_vals[header_vals$key == "Acq Method", 2]
    data_analysis_method <- header_vals[header_vals$key == "DA Method", 2]
    operator_name <- header_vals[header_vals$key == "User Name", 2]
    operator_name <- stringr::str_replace_all(operator_name, "\\\\", "_")  #four backslashes because escapes suck
    operator_name <- stringr::str_replace_all(operator_name, "/", "_")

    colnames(compound_table) <- compound_table[1, ]
    compound_table <- compound_table[2:(nrow(compound_table) - 2), ]
    compound_table <- compound_table[compound_table$RT != "RT", ]

    for (i in 1:length(element_list)) {
        compound_table[element_list[i]] <- V_element_count(formula = compound_table$`Molecular Formula`, element = element_list[i])
    }

    etable <- compound_table[, colnames(compound_table) %in% element_list]

    total_area <- sum(as.numeric(compound_table$Area), na.rm = TRUE)

    by_all <- unique(etable)

    # Remove empty rows that sneak trhough sometimes
    zeroRows <- unname((rowSums(by_all, na.rm = TRUE) == 0))
    if (sum(zeroRows) > 0) {
        by_all <- by_all[!zeroRows, ]
    }

    by_all$Area <- apply(by_all, 1, function(x) summarize_table(x, compound_table))
    by_all$Area.Percent <- (by_all$Area/total_area) * 100

    by_c <- data.frame(`Carbon Count` = integer(), Area = integer(), Area.Percent = numeric())
    by_ch <- data.frame(`Carbon Count` = integer(), `Hydrogen Count` = integer(), Area = integer(), Area.Percent = numeric())

    for (i in unique(compound_table$C)) {
        ct <- compound_table[compound_table$C == i, ]
        nr <- data.frame(`Carbon Count` = i, Area = sum(as.numeric(ct$Area)), Area.Percent = (sum(as.numeric(ct$Area))/total_area) *
            100)
        by_c <- rbind(by_c, nr)
        for (j in unique(ct$H)) {
            ch <- ct[ct$H == j, ]
            nh <- data.frame(`Carbon Count` = i, `Hydrogen Count` = j, Area = sum(as.numeric(ch$Area)), Area.Percent = (sum(as.numeric(ch$Area))/total_area) *
                100)
            by_ch <- rbind(by_ch, nh)
        }
    }

    by_c <- by_c[order(by_c$Carbon.Count), ]
    by_ch <- by_ch[order(by_ch$Carbon.Count, by_ch$Hydrogen.Count), ]
    by_all <- by_all[order(by_all$C, by_all$H, by_all$N, by_all$O, by_all$S, by_all$F, by_all$Cl, by_all$Br, by_all$I, by_all$P,
        by_all$B, by_all$Si), ]

    # Remove columns with elements that never show up.
    zeroCols <- (colSums(by_all, na.rm = TRUE) == 0)
    if (sum(zeroCols) > 0) {
        by_all <- by_all[, !zeroCols]
    }
    element_included_list <- colnames(by_all)[1:(length(colnames(by_all)) - 2)]


    rownames(by_all) <- NULL
    rownames(by_c) <- NULL
    rownames(by_ch) <- NULL

    rmarkdown::render(input = system.file("rmd/genericreport.rmd", package = "GCMSResultsCompiler"), output_format = "pdf_document",
        output_file = paste(sample_id, "_breakdown.pdf", sep = ""), output_dir = report_dir, params = list(set_title = paste0(sample_id,
            " Breakdown Report"), set_date = Sys.Date()))

    utils::write.csv(x = by_c, file = paste0(report_dir, "/", sample_id, "_by_c.csv"), row.names = FALSE)
    message("Output created: ", paste0(report_dir, "/", sample_id, "_by_c.csv"))
    utils::write.csv(x = by_ch, file = paste0(report_dir, "/", sample_id, "_by_ch.csv"), row.names = FALSE)
    message("Output created: ", paste0(report_dir, "/", sample_id, "_by_ch.csv"))
    utils::write.csv(x = by_all, file = paste0(report_dir, "/", sample_id, "_by_all.csv"), row.names = FALSE)
    message("Output created: ", paste0(report_dir, "/", sample_id, "_by_all.csv"))

}
}
