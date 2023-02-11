##Return vehicle location data in GTFS-RT format from the 'BODS' API

#' @name get_location_gtfs
#' @title Return GTFS-RT location data from the 'BODS' API
#' @export
#'
#' @param api_key API key for the 'BODS' dataset passed as a string. Can be obtained from \href{https://data.bus-data.dft.gov.uk/api/}{the 'BODS' API login}
#' @param bounding_box vector of four numerics. Limit results to location data
#' for vehicles within the rectangular boundingBox you set using co-ordinates
#' [minLatitude, maxLatitude, minLongitude, maxLongitude].
#' Defaults to NULL.
#' @param route_id string or vector of strings. Limit results to bus location data with the specified routeId.
#' Defaults to NULL.
#' @param start_time_after integer. Limit results to bus location data with a
#' start time after the specified Unix timestamp. Defaults to NULL.
#' @param start_time_before integer. Limit results to bus location data with a
#' start time before the specified Unix timestamp. Defaults to NULL.
#'
#' @importFrom httr GET content http_status
#'
#' @return Returns bus location data in GTFS-RT format. More detail on this format can be found \href{https://data.bus-data.dft.gov.uk/guidance/requirements/?section=dataformats}{the 'BODS' data formats documentation}
#'
#' @examples
#'
#' \dontrun{
#' #Before running these examples, ensure you have an API key saved
#'
#'
#' #Return data for specified route ID
#' get_location_gtfs(route_id = "45")
#'
#' #Return data within a specified bounding box
#' get_location_gtfs(bounding_box = c(51.401, 51.509, 0.01, 0.201))
#'
#' }


#Function to pull in metadata
get_location_gtfs <- function(api_key = Sys.getenv("BODS_KEY"),
                             bounding_box = NULL,
                             route_id = NULL,
                             start_time_after = NULL,
                             start_time_before = NULL) {

  ##Set user agent so BODS can track R users
  ua <- httr::user_agent("https://github.com/department-for-transport/bodsr")

  ##Use bounding box coordinates to search on
  if(!is.null(bounding_box)){

    ##If there's not 4 coordinates, stop
    if(length(bounding_box) != 4){
      stop("Incorrect number of coordinates provided to bounding_box argument")
    }

    bounding_box <- paste0("boundingBox=",
                           paste0(bounding_box, collapse = ","),
                           "&")
  }

  ##Create search strings for arguments if they are not null
  route_id <- not_null(route_id, "routeId")
  start_time_after <- not_null(start_time_after, "startTimeAfter")
  start_time_before <- not_null(start_time_before, "startTimeBefore")


  #Paste together URL for API
  url <- paste0("https://data.bus-data.dft.gov.uk/api/v1/gtfsrtdatafeed/?",
                "&",
                bounding_box,
                route_id,
                start_time_after,
                start_time_before,
                "api_key=", api_key)

  ##Read from url
  download <- httr::GET(url, ua)


  ##Return error message if authentication failed
  if(httr::http_status(download)$reason != "OK"){
    stop(httr::http_status(download)$message)
  } else if(httr::http_status(download)$reason == "Bad Request"){
    stop("Bad request; please check you have passed arguments to the function correctly")
  } else{

  ##Return GTFS-RT data
  return(download)
  }

}
