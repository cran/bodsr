##Return timetable metadata table from the 'BODS' API, including urls of full datasets

#' @name get_timetable_metadata
#' @title Return timetable metadata from the 'BODS' API
#' @export
#'
#' @param api_key API key for the 'BODS' dataset passed as a string. Can be obtained from \href{https://data.bus-data.dft.gov.uk/api/}{the BODS API login}
#' @param limit integer. Maximum number of records to return for a query. Defaults to 25
#' @param noc string or vector of strings. Limit results to fares data sets for specified National Operator Codes.
#' A full lookup of NOC to bus operator names can be seen using noc_lookup().
#' Defaults to NULL.
#' @param search string to search records on; can be a value or partial value to
#' match the data set name, data set description, organisation name, or admin
#' area name. Defaults to NULL.
#' @param admin_area string or vector of strings. Limit results to datasets with services that stop
#' within the specified area(s).  'ATCO' Area Codes are as specified in the \href{https://www.data.gov.uk/dataset/3b1766bf-04a3-44f5-bea9-5c74cf002e1d/national-public-transport-gazetteer-nptg}{NPTG area codes}
#' Defaults to NULL.
#' @param status string. Limit results to data sets with the specified status,
#' accepted values are "published" or "inactive". Defaults to NULL.
#' @param end_date_start datetime. Limit results to data sets with services with
#' end dates after this date. Defaults to NULL.
#' @param end_date_end datetime. Limit results to data sets with services with
#' end dates before this date. Defaults to NULL.
#' @param modified_date datetime. Limit results to data sets that have been
#' created or updated since the specified date. Defaults to NULL.
#' @param start_date_start datetime. Limit results to data sets with services
#' with start dates after this date. Defaults to NULL.
#' @param start_date_end datetime. Limit results to data sets with services
#' with start dates before this date. Defaults to NULL.
#' @param dq_rag string. Limit results to data sets with the specified RAG status.
#' Accepted options are "red", "amber" and "green". Defaults to NULL.
#' @param bods_compliance logical. Limit results to datasets with the specified
#' BODS compliance status. Defaults to NULL.
#'
#' @importFrom httr GET content http_status
#' @importFrom jsonlite fromJSON
#'
#' @return Returns a data frame of timetable metadata including links to data from the 'BODS' API.
#' @examples
#'
#' \dontrun{
#' #Before running these examples, ensure you have an API key saved
#' #Return the first 25 results of timetable metadata with no filters
#' get_timetable_metadata()
#'
#' #Return timetable metadata for National Express
#' get_timetable_metadata(noc = "NATX")
#'
#' #Return only published timetable metadata for Go Ahead with a green RAG status
#' get_timetable_metadata(noc = "BHBC", status = "published", dq_rag = "green")
#'
#' #Return timetable metadata for the Devon admin area and search string
#' get_timetable_metadata(admin_area = "110", search = "Plymouth Citybus")
#' }

#Function to pull in metadata
get_timetable_metadata <- function(api_key = Sys.getenv("BODS_KEY"),
                                   limit = 25,
                                   search = NULL,
                                   noc = NULL,
                                   admin_area = NULL,
                                   status = NULL,
                                   end_date_start = NULL,
                                   end_date_end = NULL,
                                   modified_date = NULL,
                                   start_date_start = NULL,
                                   start_date_end = NULL,
                                   dq_rag = NULL,
                                   bods_compliance = NULL) {

  ##Set user agent so BODS can track R users
  ua <- httr::user_agent("https://github.com/department-for-transport/bodsr")

  ##Check data values received
  if(!is.numeric(limit)){
    stop("Please provide an integer value to the limit argument")
  }

  ##Use search string if it's not null
  if(!is.null(search)) {

    ##Swap spaces for character
    search <- paste0("search=", gsub(" ", "%20", search), "&")

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

  ##Use admin area values to search on if not null
  if(!is.null(admin_area)) {

    admin_area <- paste0("adminArea=", paste(admin_area, collapse = ","), "&")

  }

  ##Create search strings for arguments if they are not null
  status <- not_null(status, "status")
  end_date_start <- not_null_date(end_date_start, "endDateStart")
  end_date_end <- not_null_date(end_date_end, "endDateEnd")
  modified_date <- not_null_date(modified_date, "modifiedDate")
  start_date_start <- not_null_date(start_date_start, "startDateStart")
  start_date_end <- not_null_date(start_date_end, "startDateEnd")
  dq_rag <- not_null(dq_rag, "dqRag")

  #Paste together URL for API
  url <- paste0("https://data.bus-data.dft.gov.uk/api/v1/dataset?limit=",
                limit,
                "&",
                noc,
                admin_area,
                search,
                status,
                end_date_start,
                end_date_end,
                modified_date,
                start_date_start,
                start_date_end,
                dq_rag,
                "api_key=",
                api_key)

  #Raw content from api
  download <- httr::GET(url, ua)

  ##Return error message if authentication failed
  if(httr::http_status(download)$reason == "Unathorized"){
    stop("Authentication credentials are not valid; please check you are using a valid BODS API key")
  }

  if(httr::http_status(download)$reason == "Bad Request"){
    stop("Bad request; please check you have passed arguments to the function correctly")
  }

  data <- jsonlite::fromJSON(
    httr::content(download, as = "text", encoding = "UTF-8"))$results

  message(paste("Returning", nrow(data), "records"))

  return(data)
}

