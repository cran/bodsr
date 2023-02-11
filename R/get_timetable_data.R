#' @name get_timetable_data
#' @title Extract line-level timetable data from all rows of the provided metadata table
#'
#' @param timetable_metadata A single row of table metadata extracted using get_timetable_metadata()
#'
#' @importFrom httr write_disk GET
#' @importFrom dplyr "%>%" bind_cols select
#' @importFrom purrr map
#' @importFrom rlang .data
#' @export
#'
#' @return returns list of timetable dataframes, with each dataframe corresponding to a
#' row on the provided timetable metadata
#'
#' @examples
#'
#' \dontrun{
#' #Before running these examples, ensure you have an API key saved
#' #Return the first 5 results of timetable metadata with no filters
#' metadata <- get_timetable_metadata(limit = 5)
#'
#'
#' }
#'

get_timetable_data <- function(timetable_metadata){

  ##Extract metadata that applies to all files
  meta <- timetable_metadata %>%
    dplyr::select(.data$url, "dataSetID" = .data$id,
                  .data$operatorName, .data$description, .data$status,
                  .data$extension, .data$dqScore, .data$dqRag)

  ##Message warning how long this is going to take
  ##Return number of files
  message("This metadata contains ", sum(xml_file_counter(meta)), " xml file(s)")

  rowwise_extract <- function(i){
    message("Extracting row ", i, " of ", nrow(meta))

    x <- meta[i, ]

      x %>%
        ##Read in the xml
        extract_line_level_data() %>%
        ##Join to the URL deets
        dplyr::bind_cols(x)
  }

  #Loop over all the rows
  purrr::map(.x = 1:nrow(meta),
             .f = rowwise_extract)
}
