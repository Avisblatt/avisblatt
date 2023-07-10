#' Read Avisblatt Collection of Data and Metadata
#'
#' In a separation of concerns approach the avisblatt project splits data and
#' data descriptions into two files best suited for the different nature of the
#' information represented. Doing so makes the entire information machine
#' friendly as the split allows to use standard read/write functions for .csv
#' and .json file formats.
#'
#' @param name_on_disk character path to collection w/o file extension.
#' @param just_meta boolean only use meta information? Defaults to FALSE.
#' @import data.table
#' @importFrom jsonlite fromJSON
#' @importFrom quanteda corpus
#' @export
read_collection <- function(name_on_disk,
                            just_meta = FALSE){
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  if(!just_meta){
    dt <- fread(data_file, encoding="UTF-8")
    crps <- corpus(dt, docid_field = "id",
                   text_field = "text")
  }

  mi <- fromJSON(meta_file)
  d <- data.table::rbindlist(mi)
  d[, id := names(mi)]
  d[, date := as.Date(date)]
  setcolorder(d, neworder = c("id",
                              setdiff(names(d),"id")))
  if(just_meta){
    collect <- Collection$new(NULL, d)
  } else {
    collect <- Collection$new(crps, d)
  }

  collect
}

