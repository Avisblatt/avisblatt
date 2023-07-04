#' Selects data from a collection object based on date range
#'
#' @param coll A Collection object.
#' @param ids A character vector of ids to select from. If NULL, all ids are selected.
#' @param min A character string representing the minimum date in format "YYYY-MM-DD".
#' @param max A character string representing the maximum date in format "YYYY-MM-DD".
#'
#' @return A character vector of ids that meet the date range criteria.
#'
#' @export
select_by_date <- function(coll,
                           ids = NULL,
                           min = "1729-01-01",
                           max = "1844-12-31"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  min_is_date <- tryCatch(!is.na(as.Date(min, format = "%Y-%m-%d")),
                          error = function(err) {FALSE})
  max_is_date <- tryCatch(!is.na(as.Date(max, format = "%Y-%m-%d")),
                          error = function(err) {FALSE})
  if (min_is_date & max_is_date){
    min <- as.Date(min)
    max <- as.Date(max)
    ids <- intersect(ids, coll$meta[date >= min, id])
    intersect(ids, coll$meta[date <= max, id])
  } else{
    message("If giving a min or max date, it must be in format YYYY-MM-DD.")
  }
}

#' Selects documents from a collection based on their publication date
#'
#' @param coll A Collection object.
#' @param ids A vector of document IDs to select. If NULL, all documents are selected.
#' @param date_MM_DD A string indicating the month and day of the year to select documents from. If NULL, all documents are selected.
#' @param days_before An integer indicating how many days before the specified date to include in the selection.
#' @param days_after An integer indicating how many days after the specified date to include in the selection.
#'
#' @return A Collection object containing only the selected documents.
#'
#' @export
select_by_season <- function(coll,
                             ids = NULL,
                             date_MM_DD = NULL,
                             days_before = 0,
                             days_after = 0){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(!(is.numeric(days_before) & is.numeric(days_after))
     |days_before<0|days_after<0)
    {
    stop("days_before and days_after need to be 0 [default] or positive integers")
  }
  #Converting Xmas/fair to dates
  if(date_MM_DD=="Christmas"){
    date_MM_DD <- "12-25"
    if (days_before+days_after==0){
      warning("Use the parameters days_before and/or days_after to specifiy a timeframe that extends beyond Christams Day itself. In most years, no issue of the Avisblatt was published on that day.")
      }
    }
  else if(date_MM_DD=="Fair"){
    date_MM_DD <- "10-27"
    days_after <- days_after+13
    }
  if(tryCatch(!is.na(as.Date(date_MM_DD, format = "%m-%d")))){
    min <- as.Date(paste(seq.int(1729, 1844, 1), rep(paste("-", date_MM_DD, sep=""), 116), sep="")) - days_before
    max <- as.Date(paste(seq.int(1729, 1844, 1), rep(paste("-", date_MM_DD, sep=""), 116), sep="")) + days_after
  } else if (date_MM_DD=="Easter"){
    if (days_before+days_after==0){
      warning("Use the parameters days_before and/or days_after to specifiy a timeframe that extends beyond Easter Sunday itself. On that day, no issue of the Avisblatt was ever published.")
    }
    # Get Easter Sunday dates for 1729-1844 (according to Grotefend)
    easter_sundays <- fread("../avis-analysis/data/easter_sunday.csv", encoding="UTF-8")
    min <- as.Date(unlist(easter_sundays - days_before), origin = "1970-01-01")
    max <- as.Date(unlist(easter_sundays + days_after), origin = "1970-01-01")

  } else {
    stop("Date must be either ''Easter'' (= Easter Sunday), ''Christmas'' (Xmas day, 25th), 'Fair' (duration of Basel autumn fair), or an in-year-date in format MM-DD.")
  }
  dt <- coll$meta
  # Limit dt to records in ids
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]}
  # Check for each record in dt, if its date (x)
  # is in between ANY of the 116 min-max intervals.
  # Hence between() will give 116 boolean values here.
  # Sum counts all TRUE occurrences,
  # so if date x is in any of the 116 intervals,
  # sum(between()) will deliver 1, otherwise 0.
  dt[unlist(lapply(dt$date,  function(x) (sum(between(x, min, max)) == 1))), id]
}


#' Selects documents from a collection based on their length
#'
#' @param coll A collection object
#' @param ids A vector of document ids to select from. If NULL, all documents are selected.
#' @param min Minimum length of the document in tokens or characters. Default is 0.
#' @param max Maximum length of the document in tokens or characters. Default is 1000000.
#' @param unit Unit of length. Either "tokens" (default) or "char".
#'
#' @return A vector of document ids that meet the length criteria.
#'
#' @export
select_by_length <- function(coll,
                             ids = NULL,
                             min = 0,
                             max = 1000000,
                             unit = "tokens"){
  ntokens <- NULL
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(unit == "tokens"){
    ids <- intersect(ids, coll$meta[ntokens >= min, id])
    intersect(ids, coll$meta[ntokens <= max, id])
  } else if(unit == "char"){
    ids <- intersect(ids, coll$meta[nchar >= min, id])
    intersect(ids, coll$meta[nchar <= max, id])
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}


#' Selects records from a collection by tags
#'
#' This function selects records from a collection based on the tags associated with each record.
#'
#' @param coll A collection object.
#' @param ids A vector of record IDs to select. If NULL, all records are selected.
#' @param tagslist A vector of tag substrings. Only records containing all of these substrings will be selected.
#' @param headerlist A vector of header substrings. Only records containing all of these substrings in their header will be selected.
#' @param manualtagslist A vector of manual tag substrings. Only records containing all of these substrings in their manual tags will be selected.
#'
#' @return A vector of record IDs that match the selection criteria.
#'
#' @export
select_by_tags <- function(coll,
                           ids = NULL,
                           tagslist = NULL,
                           headerlist = NULL,
                           manualtagslist = NULL){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  # Limit dt to records in ids
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]}
  tags <- dt$id
  header <- dt$id
  manual <- dt$id
  if(!is.null(tagslist)){
    for (i in 1:length(tagslist)){
      tags <- intersect(tags, dt[grepl(tagslist[i], dt$tags), id])
    }
  }
  if(!is.null(headerlist)){
    for (i in 1:length(headerlist)){
      header <- intersect(header, dt[grepl(headerlist[i], dt$tags_section), id])
    }
  }
  if(!is.null(manualtagslist)){
    for (i in 1:length(manualtagslist)){
      manual <- intersect(manual, dt[grepl(manualtagslist[i], dt$tags_manual), id])
    }
  }
  intersect(tags, intersect(manual, header))
}


#' Selects ad by meta information
#'
#'
#' @param coll an object of class Collection.
#' @param ids character vector of ids to be searched. Defaults to NULL, searching all
#' available character.
#' @param search character what look for
#' @param fields character where to search
#' @param exact boolean should matches be exact? Defaults to FALSE.
#' @export
select_by_meta <- function(coll,
                           ids = NULL,
                           search = NULL,
                           fields = NULL,
                           exact = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(fields)){
    message("As the fields parameter was not specified, records will be selected if the search matches any of their metadata.")
    fields <- colnames(coll$meta)
  }
  invalid_fields <- setdiff(fields, colnames(coll$meta))
  fields <- setdiff(fields, invalid_fields)
    if (length(invalid_fields)>0){
    stop(paste("The following metadata fields you wanted to search in are not part of the collection:",
               paste(paste0("- ", invalid_fields), collapse = "\n"),
               "The collection does only contain the following metadata fields:",
               paste(paste0("- ", colnames(coll$meta)), collapse = "\n"),
               sep = "\n")
    )
  }
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  # Limit dt to records in ids
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]}
  result <- dt$id
  if(!is.null(search)){
    for (i in 1:length(search)){
      temp_result <- NULL
      for (j in 1:length(fields)){
        if(exact){
          temp_result <- union(temp_result, coll$meta[get(fields[j]) == search[i], id])
        } else {
          temp_result <- union(temp_result, dt[grepl(search[i], dt[,get(fields[j])]), id])
        }
      }
      result <- intersect(result, temp_result)
    }
  }
  result
}


#' select_by_text
#'
#' @description
#' Selects documents from a collection based on a search list
#'
#' @param coll A collection object.
#' @param ids A vector of document ids to search in. If NULL, all documents in the collection are searched.
#' @param searchlist A character vector of search terms.
#'
#' @return A vector of document ids that match the search terms.
#'
#' @importFrom R6 R6Class
#' @importFrom utils read.csv
#' @export
select_by_text <- function(coll,
                           ids = NULL,
                           searchlist = ""){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids) == 1){
    if(ids == "all"){
      ids <- coll$meta$id
    }
    }
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections to be able to search in texts")
  } else{
    if(length(searchlist) > 0){
      for (i in 1:length(searchlist)){
        ids <- intersect(ids, names(coll$corpus[grepl(searchlist[i], as.character(coll$corpus), perl = TRUE)]))
      }
    }
    ids
  }
}


#' Selects records from a collection based on reprint status
#'
#' This function selects records from a collection based on their reprint status.
#' It can select records that are reprinted originals, unreprinted originals,
#' postings, reprints, other or ads.
#'
#' @param coll A collection object.
#' @param ids A vector of record ids to select. If NULL (default), all records are selected.
#' @param status A character string specifying the reprint status to select. Possible values are:
#' \itemize{
#'   \item "reprinted_orig": Reprinted originals
#'   \item "unreprinted_orig": Unreprinted originals
#'   \item "postings": Postings
#'   \item "reprints": Reprints
#'   \item "other": Other
#'   \item "ads": Ads
#' }
#'
#' @return A vector of record ids that match the specified reprint status.
#'
#' @export
select_by_reprint_status <- function(coll,
                                     ids = NULL,
                                     status = c("reprinted_orig",
                                                "unreprinted_orig",
                                                "postings", "reprints",
                                                "other", "ads")){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  match.arg(status)
  switch(status,
         reprinted_orig   = intersect(ids, coll$meta[grepl("orig_r",  coll$meta$reprint_status), id]),
         unreprinted_orig = intersect(ids, coll$meta[grepl("orig_u",  coll$meta$reprint_status), id]),
         postings         = intersect(ids, coll$meta[grepl("orig",    coll$meta$reprint_status), id]),
         reprints         = intersect(ids, coll$meta[grepl("reprint", coll$meta$reprint_status), id]),
         other            = intersect(ids, coll$meta[is.na(coll$meta$reprint_status), id]),
         ads              = intersect(ids, coll$meta[!is.na(coll$meta$reprint_status), id]))
}


#' Selects documents by language
#'
#' This function selects documents from a collection based on their language.
#'
#' @param coll A collection object.
#' @param ids A vector of document IDs to select. If NULL, all documents are selected.
#' @param status A character string indicating the language status of the documents to select. Possible values are "de", "fr", and "unknown".
#'
#' @return A vector of document IDs that match the specified criteria.
#'
#' @export
select_by_language <- function(coll,
                               ids = NULL,
                               status = c("de", "fr", "unknown")){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  match.arg(status)
  switch(status,
         de      = intersect(ids, coll$meta[grepl("de",  coll$meta$language), id]),
         fr      = intersect(ids, coll$meta[grepl("fr",  coll$meta$language), id]),
         unknown = intersect(ids, coll$meta[is.na(coll$meta$language), id]))
}
