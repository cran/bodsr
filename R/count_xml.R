#' @name xml_file_counter
#' @title Count the number of xml files included within a provided metadata dataframe, whether the provided file links are xml or zip
#'
#' @param timetable_metadata A table of metadata extracted using get_timetable_metadata()
#'
#' @importFrom httr write_disk GET
#' @importFrom purrr map
#' @export
#'
#' @return returns a numeric vector of number of xml files by row of the provided metadata
##Count files to open
xml_file_counter <- function(timetable_metadata){

    count_files <- function(data, x){

       meta <- data[x,]

      ##If it's a zip file, download and peek inside
      if(meta$extension == "zip"){

        ##Download to temp location
        zip_loc <- tempfile()

        httr::GET(
          url = meta$url,
          httr::write_disk(zip_loc, overwrite = TRUE)
        )

        ##List the names of the files inside and count them if they're xml paths
        count <- utils::unzip(zip_loc, list = TRUE)
        count <- nrow(count[grepl("[.]xml$", count$Name),])

      } else if(meta$extension == "xml"){
        ##If it's a xml file, just count one
        count <- 1
      } else{
        count <- 0
      }

    return(count)
    }

    ##Loop it over all the provided data
    list_count <- purrr::map(.x = 1:nrow(timetable_metadata),
                             .f = count_files,
                             data = timetable_metadata)

    #Return vector of files
    return(unlist(list_count))
}
