#' @name noc_lookup
#' @title Lookup between operator names and national operator code ('NOC') lookup
#' @export
#'
#' @importFrom utils download.file read.csv unzip
#'
#' @return Returns a data frame of operator names and their corresponding national
#' operator code from the 'BODS' API.
#'
#' @examples
#'
#' ##Check operator name lookup
#' \dontrun{
#' noc_lookup()
#' }

noc_lookup <- function(){

  ##Create temporary zip folder locations
  tmp <- tempfile(fileext = ".zip")
  tmp_csv <- tempdir()

  #Download zip to location
  download.file("https://data.bus-data.dft.gov.uk/catalogue/", tmp)

  ##Extract only NOC lookup to another temp location
  unzip(tmp, files = "operator_noc_data_catalogue.csv", exdir = tmp_csv)

  ##Read in data
  read.csv(file.path(tmp_csv, "operator_noc_data_catalogue.csv"))

}
