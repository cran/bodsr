The goal of bodsr is to allow easy interface between the Bus Open Data Service (BODS) API and R. The BODS dataset provides fares, timetable and vehicle location information about bus services in England. Further details and documentation on the BODS API can be found [here](https://www.gov.uk/government/collections/bus-open-data-service).

## Installation
You can install the development version of bodsr from GitHub with:

# install.packages("devtools")
devtools::install_github("department-for-transport-public/bodsr")

## Usage
bodsr has a range of functions designed to make it easy for you to interrogate the BODS API and receive the results as R data objects.

To begin, you will need to [create a BODS account](https://data.bus-data.dft.gov.uk/) and obtain your BODS access token. You can pass this to individual bodsr functions, or save it as an environmental variable called BODS_KEY which the functions will automatically check.  

### Fare and timetable metadata

The BODS API initially returns metadata about the fare and timetable data held. You can use this metadata to understand the data that is available, as well as locate download links to download full data sets.

The functions `get_timetable_metadata()` and `get_fares_metadata()` allow you to return records for timetable and fare metadata respectively. You can filter the records on a number of variables including:

#### For fares:

* National Operator Codes
* Status
* Bounding box

#### For timetables:

* National Operator Codes
* Status
* BODS compliance
* Modified date
* Admin area
* Search terms

Check individual function documentation and [BODS API help](https://data.bus-data.dft.gov.uk/guidance/requirements/?section=api) for further details on these variables.

## Location data

Granular vehicle-level location data can be extracted from the API in two different formats ([more detail of different data formats can be found here](https://data.bus-data.dft.gov.uk/guidance/requirements/?section=dataformats)):

* `get_location_gtfs()`: returns location data in GTFS-RT format
* `get_location_xml()`: returns location data in SIRI-VM XML format

As for fare and timetable data, location data can be filtered on a range of parameters including location bounding box, provider, line and vehicle reference.

## Timetable data

Once timetable metadata has been returned, this data can be provided to the `get_timetable_data()` function, which will parse the xml/zip files specified and return the timetable data as a list with one bus line per row and one dataframe per parsed file.

Please note that due to the size of the data files involved, queries using this function can be slow to run and use a large amount of RAM to perform.
