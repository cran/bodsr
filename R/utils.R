#' @name not_null
#' @title Join together a value and an associated API string if the value
#' is not NULL
#'
#' @param obj R object to check whether it is NULL or not
#' @param string API string to append to R object

not_null <- function(obj, string) {

  if(!is.null(obj)) {
    paste0(string, "=", obj, "&")
  } else{
    NULL
  }
}

#' @name not_null_date
#' @title Join together a date value and an associated API string if the value
#' is not NULL
#'
#' @param date R object to check whether it is NULL or not
#' @param string API string to append to R object

not_null_date <- function(date, string) {

  if(!is.null(date)) {

    if(!class(date) %in% c("Date", "POSIXct")){
      stop("Object must be of the class DATE")
    }

    paste0(string,
           "=",
           ##Convert to the right date format
           gsub(":",
                "%3A",
                format(as.Date(date), "%Y-%m-%dT%H:%M:%S"),
                fixed = TRUE),
           "&")
  } else{
    NULL
  }
}

##Utility functions for working with XML

#' @name find_node_value
#' @title Search an xml file for a specific named mode and return the value(s) stored in it
#'
#' @param x An xml object
#' @param xpath string. The node name to search for within the xpath.
#'
#' @importFrom xml2 xml_find_all as_list xml_ns
#'
#' @return Returns a single string of values from the specified node

##Return a specific value from xml
find_node_value <- function(x, xpath){

  ##If the d1 namespace is missing, return empty value
  if(any(grepl("d1", names(xml2::xml_ns(x))))){
    xml2::xml_find_all(x = x, xpath = xpath) %>%
      xml2::as_list() %>%
      unlist()
  } else{
    NULL
  }

}

##Utility functions for working with XML

#' @name count_nodes
#' @title Search an xml file for a specific named node and count the number of instances
#'
#' @param x An xml object
#' @param xpath string. The node name to search for within the xpath.
#'
#' @importFrom xml2 xml_find_all as_list xml_ns
#'
#' @return Returns a numeric count value

count_nodes <- function(x, xpath){

  x %>%
    xml2::xml_find_all(xpath) %>%
    length()

}

#' @name poss_xml
#' @title Try to read an xml file using read_xml; where this fails, quietly return a NULL value
#'
#' @param ... arguments to pass to the read_xml function
#' @importFrom xml2 read_xml
#' @importFrom purrr possibly

poss_xml <- purrr::possibly(xml2::read_xml, otherwise = NULL)
