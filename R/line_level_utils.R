#' @name line_level_xml
#'
#' @param x An xml object
#' @param count numeric. Where the xml file is taken from a zip collection, the number file it is. Defaults to 1.
#' @param total_count numeric. Where the xml file is taken from a zip collection, the total number of files in the zip. Defaults to 1.
#'
#' @title Pull a table of relevant values from specified nodes in the xml
#'
#' @importFrom xml2 read_xml
#' @importFrom tibble tibble
#'
#' @return Returns a table of values extracted from specified nodes of
#' an xml document

##Get line-level details from an xml file
line_level_xml <- function(x, count = 1, total_count = 1){

  message("Reading file ", count, " of ", total_count)

  #Read in xml safely
  xml <- poss_xml(x)

  if(!is.null(xml)){
  ##Create a table of values
      values <- tibble::tibble(
        ##Get operator name
        "tradingName" = find_node_value(xml, "//d1:TradingName"),
        #Operator code
        "operatorCode" = find_node_value(xml, "//d1:OperatorCode"),
        #Licence number
        "licenceNumber" = find_node_value(xml, "//d1:LicenceNumber[1]"),
        #Service code
        "serviceCode" = paste(find_node_value(xml, "//d1:LicenceNumber[1]"),
                              find_node_value(xml, "//d1:ServiceCode"), sep = ":"),
        ##Line names
        "lineName" = find_node_value(xml, "//d1:LineName"))
      } else{

        #Create a blank table of values
        values <- tibble::tibble(
          ##Get operator name
          "tradingName" = character(),
          #Operator code
          "operatorCode" = character(),
          #Licence number
          "licenceNumber" = character(),
          #Service code
          "serviceCode" = character(),
          ##Line names
          "lineName" = character())
      }

    #If our tibble is blank, give a warning
  if(nrow(values) == 0){
    warning("File could not be read in:", x)
    }

  return(values)
  }

#' @name open_all_xml
#' @title Open every xml file within a zip object and extract data of interest from it using a given function
#'
#' @param url A url pointing towards a zip object
#' @param fun name of a data extracting function to apply to the zip folder
#'
#' @importFrom utils unzip
#' @importFrom httr write_disk GET
#' @importFrom purrr map_df
#'
#'
#' @return returns a dataframe of information extracted from xml documents

##Open every XML file in a zip and link them up to the names
open_all_xml <- function(url, fun){

  ##Download to temp location
  zip_loc <- tempfile()
  folder <- tempdir()
    #Remove everything from the folder
  unlink(paste0(folder, "/*"))

  httr::GET(
    url = url,
    httr::write_disk(zip_loc, overwrite = TRUE)
  )

  ##Unzip the zip file to the temp location
  utils::unzip(zip_loc, exdir = folder, overwrite = TRUE)

  ##Files to read in
  files_to_read <- list.files(folder, full.names = TRUE, pattern = "\\.xml")

  ##For each item in the folder, run extracting the single line over it
  files <- purrr::map2_df(.x = files_to_read,
                           .y = 1:length(files_to_read),
                           .f = fun,
                           .id = "filepath",
                           total_count = length(files_to_read))

  return(files)
}

#' @name extract_line_level_data
#' @title Open data from a single line metadata table where it's zip or xml format
#'
#' @param file A single row of table metadata extracted using get_timetable_metadata()
#'
#' @importFrom httr write_disk GET
#'
#' @return returns a dataframe of information extracted from the given xml or zip url

extract_line_level_data <- function(file){

  ##Try to unzip with names if it's a zip
  if(file$extension == "zip"){

    open_all_xml(file$url, line_level_xml)

  } else if(file$extension == "xml"){

    ##Download and open xml file
    xml_loc <- tempfile(fileext = ".xml")

    httr::GET(
      url = file$url,
      httr::write_disk(xml_loc, overwrite = TRUE)
    )

    line_level_xml(xml_loc)

  }else{
    stop("Unsupported file type")
  }

}
