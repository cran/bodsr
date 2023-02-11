##Return fare metadata table from the 'BODS' API, including urls of full datasets

#' @name get_fares_metadata
#' @title Return fares metadata from the 'BODS' API
#' @export
#'
#' @param api_key API key for the 'BODS' dataset passed as a string.
#' Can be obtained from \href{https://data.bus-data.dft.gov.uk/api/}{the 'BODS' API login}
#' @param limit integer. Maximum number of records to return for a query. Defaults to 25
#' @param noc string or vector of strings. Limit results to fares data sets for specified National Operator Codes.
#' A full lookup of NOC to bus operator names can be seen using noc_lookup().
#' Defaults to NULL.
#' @param status string. Limit results to fares data sets for specified status,
#' accepted values are "published" or "inactive". Defaults to NULL.
#' @param bounding_box vector of four numerics. Limit results to fares data sets
#' that contain information for the area within the rectangular boundingBox
#' you set using co-ordinates [minLatitude, maxLatitude, minLongitude, maxLongitude].
#' Defaults to NULL.
#'
#' @importFrom httr GET content http_status
#' @importFrom jsonlite fromJSON
#'
#' @return Returns a data frame of fares metadata including links to data from the 'BODS' API.
#' @examples
#'
#' \dontrun{
#' #Before running these examples, ensure you have an API key saved
#' #Return the first 25 results of fares metadata with no filters
#' get_fares_metadata()
#'
#' #Return fares metadata for National Express
#' get_fares_metadata(noc = "NATX")
#'
#' #Return only published fares metadata for Go Ahead
#' get_fares_metadata(noc = "BHBC", status = "published")
#'
#' #Return fares metadata for the specified bounding box
#' get_fares_metadata(bounding_box = c(51.401, 51.509, 0.01, 0.201))
#' }

#Function to pull in metadata
get_fares_metadata <- function(api_key = Sys.getenv("BODS_KEY"),
                               limit = 25,
                               noc = NULL,
                               status = NULL,
                               bounding_box = NULL) {

  ##Set user agent so BODS can track R users
  ua <- httr::user_agent("https://github.com/department-for-transport/bodsr")

  ##Check data values received
  if(!is.numeric(limit)){
    stop("Please provide an integer value to the limit argument")
  }

  ##Use noc values to search on if not null
  if(!is.null(noc)) {

    noc_check <- noc_lookup()$noc
    ##Give an error if one or more NOC values aren't in the lookup
    if(!all(noc %in% noc_check)){

      stop("Invalid NOC codes:", noc[!(noc %in% noc_check)])
    }

    noc <- paste0("noc=", paste(noc, collapse = ","), "&")

  }

  ##Use status value to search on if not null
  status <- not_null(status, "status")

  ##Use bounding box coordinates to search on
  if(!is.null(bounding_box)){

    ##If there's not 4 coordinates, stop
    if(length(bounding_box) != 4){
      stop("Incorrect number of coordinates provided to bounding_box argument")
    }

    bounding_box <- paste0("boundingBox=",
                           paste0(bounding_box, collapse = "&boundingBox="),
                           "&")

  }


  #Paste together URL for API
  url <- paste0("https://data.bus-data.dft.gov.uk/api/v1/fares/dataset?limit=",
                limit,
                "&",
                noc,
                status,
                bounding_box,
                "api_key=",
                api_key)

  #Raw content from api
  download <- httr::GET(url, ua)

  ##Return error message if authentication failed
  if(httr::http_status(download)$reason != "OK"){
    stop("Authentication credentials are not valid; please check you are using a valid BODS API key")
  } else if(httr::http_status(download)$reason == "Bad Request"){
    stop("Bad request; please check you have passed arguments to the function correctly")
  } else{

  data <- jsonlite::fromJSON(
    httr::content(download, as = "text", encoding = "UTF-8"))$results

  message(paste("Returning", nrow(data), "records"))

  return(data)
  }

}

