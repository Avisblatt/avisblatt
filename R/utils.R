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
  x$meta$reprint_of[is.na(x$meta$reprint_o)] <- "NA" #otherwise json items have different numbers of columns
  x$meta$language[is.na(x$meta$language)] <- "NA" #otherwise json items have different numbers of columns
  message("Processing data description...")
  dtcols <- setdiff(names(x$meta),"id")
  dt_chunks <- split(x$meta[, ..dtcols], as.factor(1:nrow(x$meta)))
  names(dt_chunks) <- x$meta$id
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

#' @import data.table
#' @importFrom jsonlite fromJSON
#' @importFrom quanteda corpus
#' @export
read_collection <- function(name_on_disk, just_meta = FALSE){
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

# shortcut to read & merge multiple years to one working collection
gather_yearly_collections <- function(AVIS_YEARS, just_meta = TRUE, path = "../avis-data/collections/yearly_"){
  AVIS_YEARS <- as.numeric(AVIS_YEARS)
  number_of_years <- length(AVIS_YEARS)
  fn <- paste(path, AVIS_YEARS[1], sep = "")
  c_all <- read_collection(fn, just_meta)
  if(number_of_years > 1){
    for (i in AVIS_YEARS[2:number_of_years]){
    fn <- paste(path, i, sep = "")
    coll <- read_collection(fn, just_meta)
    c_all <- merge_collections(c(c_all, coll))
    }
  }
  c_all
}


#' Clean up Human Tags (groundtruth)
#'
#' Removes leading numbers from tag and turn comma separated strings
#' into true character vectors.
#'
#' @param x character vector or list of tags.
#' @export
clean_manual_tags <- function(x){
  unique(gsub("(^[0-9]{2})(.+)","\\2",
              unlist(strsplit(x, ","))))
}


purge_spacing <- function(txtlist){
  splits <- strsplit(txtlist, "\\s")
  more_than_1 <- lapply(splits, grepl, pattern = "\\S{2,}")
  single_digit <- lapply(splits, grepl, pattern = "^\\d{1}$")
  not_single_letter <- mapply("|", single_digit, more_than_1, SIMPLIFY = FALSE)
  not_single_letter_shifted <- mapply(c, FALSE, not_single_letter, SIMPLIFY = FALSE)
  not_single_letter_shifted <- lapply(not_single_letter_shifted, head, -1, SIMPLIFY = FALSE)
  tf <- mapply("|", not_single_letter, not_single_letter_shifted, SIMPLIFY = FALSE)
  # cumsum is an elegant way to keep track of T/F swaps
  tfcs <- lapply(tf, cumsum)
  out <- mapply(split, splits, tfcs)
  # reconstruct
  out <- lapply(out, sapply, paste, collapse = "")
  unlist(lapply(out, paste, collapse = " "))
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


