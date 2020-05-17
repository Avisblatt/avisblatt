#' @importFrom jsonlite fromJSON toJSON
#' @importFrom quanteda corpus docvars texts
#' @import data.table
#' @export
write_collection <- function(x,
                             name_on_disk,
                             pretty_json = TRUE,
                             zip = FALSE){
  # sanity checks
  stopifnot(inherits(x, "Collection"))
  stopifnot(inherits(x, "R6"))

  # name of the two files
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  # meta information and data are treated separately
  # following the swissdata idea (github.com/swissdata/demo)
  # Meta information to JSON ##############################
  # turn all environments to lists
  # environments work with reference and thus better than lists
  # for in memory updates. lists are easier to handle when writing
  # to a JSON string.
  message("Processing data description...")
  dtcols <- setdiff(names(x$meta),"id")
  dt_chunks <- split(x$meta[, ..dtcols], as.factor(1:nrow(x$meta)))
  names(dt_chunks) <- x$meta$id
  j <- toJSON(dt_chunks, pretty=TRUE)
  writeLines(
    toJSON(dt_chunks, pretty = pretty_json,
           auto_unbox = TRUE,
           null = "null"),
    meta_file
  )
  message(sprintf("Meta information written to %s",meta_file))

  dt <- data.table(
    id = names(x$corpus),
    text = texts(x$corpus),
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

#' @importFrom data.table fread
#' @importFrom jsonlite fromJSON
#' @importFrom quanteda corpus
#' @export
read_collection <- function(name_on_disk){
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  dt <- fread(data_file)
  crps <- corpus(dt, docid_field = "id",
                 text_field = "text")
  mi <- fromJSON(meta_file)
  ac <- AvisCollection$new(crps, mi)
  ac
}



#' Might want to deprecate this with quanteda 2.0
#' @export
get_text_by_id <- function(corp, ids, n = NULL,
                           identifier = "id",
                           txt = "texts"){
  tf <- docvars(corp, identifier) %in%
    ids

  if(is.null(n)) {
    return(corp$documents[,txt][tf])
  }

  corp$documents[,txt][tf][1:n]


}


#' Might want to deprecate this with quanteda 2.0
#' @export
get_subcorpus_by_id <- function(corp, ids, idvar = "id"){
  corpus_subset(corp,
                (docvars(corp, idvar) %in% ids)
  )
}




