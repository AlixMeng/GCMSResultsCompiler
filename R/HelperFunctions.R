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
    return(elements)
  }
  element_list<-unlist(heteroatoms)
  if(any(grepl("[^A-Za-z]", element_list))){  #Any non-letter characters in set
    stop("Heteroatoms must be a character only vector")
  }
  if(any(!(element_list %in% elements))){
    element_list<-element_list[element_list %in% elements]
    message(paste("Removed", length(heteroatoms)-length(element_list), "non-atoms from heteroatom list:", paste0(heteroatoms[!(heteroatoms %in% elements)], collapse=", ")))
  }
  if (!("C" %in% element_list) && !("H" %in% element_list)){
    message("Adding C, H to heteroatoms list")
    element_list<-c("C", "H", element_list)
  } else if (!('C' %in% element_list)){
    message("Adding C to heteroatoms list")
    element_list <-c("C", element_list)
  } else if (!('H' %in% element_list)){
    message("Adding H to heteroatoms list")
    element_list <-c("H", element_list)
  }
  return(element_list)
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
    if (is.na(formula) || is.numeric(element)) {
        return(0)
    }
    count <- stringr::str_match(formula, paste0(element, "(?![a-z])([0-9]*)"))[2]  # The (?![^a-z]) makes sure that C doesn't hit on Ca
    if (is.na(count)) {
        return(0)
    } else if (count == "") {
        count <- "1"
    }
    return(as.numeric(count))
}

#' Extract Header Info
#'
#' Get values out of a data frame with key:value columns. Return the value if string, or stop() if not found
#'
#' @param info_frame The data frame, with two columns: \code{key} and \code{value}
#' @param info_key The key from which to extract data, if it exists.
#'
#' @keywords Internal
#'
#' @return value from key:value pair
extract_key_value <- function(info_frame, info_key){
  if(!(info_key %in% info_frame$key)){
    stop(paste0(info_key, " not found in source file headers."))
  }
  return(info_frame[info_frame$key == info_key, ]$value)
}
