#' Write collection to Disk
#'
#' Write a collection to disk using two separate files: a .csv file for the data
#' and a .json file for the metainformation. Objects are linked across files using
#' ids.
#'
#' @param x object of class Collection.
#' @param name_on_disk character name of the file on disk
#' @param pretty_json boolean should json be prettified? Defaults to TRUE.
#' @param zip boolean should data and data description be put together in one zip archive?
#' Defaults to FALSE.
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom quanteda corpus docvars texts
#' @importFrom quanteda.textstats textstat_dist
#' @import data.table
#' @export
write_collection <- function(x,
                             name_on_disk,
                             pretty_json = TRUE,
                             zip = FALSE){
  ..dtcols <- NULL
  # sanity checks
  stopifnot(inherits(x, "Collection"))
  stopifnot(inherits(x, "R6"))

  # name of the two files
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  # meta information and data are treated separately
  # following the swissdata idea (github.com/swissdata/demo)
  # Meta information to JSON
  # turn all environments to lists
  # environments work with reference and thus better than lists
  # for in memory updates. lists are easier to handle when writing
  # to a JSON string.
  message("Processing data description...")
  dtcols <- setdiff(names(x$meta),"id")
  dt_chunks <- split(x$meta[, ..dtcols], as.factor(1:nrow(x$meta)))
  names(dt_chunks) <- x$meta$id
  writeLines(
    toJSON(dt_chunks, pretty = pretty_json,
           auto_unbox = TRUE,
           null = "null",
           na = "string"),
    meta_file
  )
  message(sprintf("Meta information written to %s",meta_file))

  dt <- data.table(
    id = names(x$corpus),
    text = as.character(x$corpus),
    docvars(x$corpus))
  fwrite(dt, file = data_file, quote = TRUE)
  message(sprintf("Data written to %s",data_file))

  if(zip){
    zip_file <- paste0(name_on_disk,".zip")
    zip(zip_file, c(data_file, meta_file))
    file.remove(data_file)
    file.remove(meta_file)
    message("Zip archive containing data and meta data created.")
  }
}
