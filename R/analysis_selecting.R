#' @export
select_by_date <- function(ids = NULL, coll = c_all,
                          min = "1729-01-01", max = "1844-12-31"){
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


#' @export
select_by_season <- function(ids = NULL, coll = c_all, date_MM_DD = NULL,
                          days_before = 0, days_after = 0){
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
    date_MM_DD <- "12-25"}
  else if(date_MM_DD=="Fair"){
    date_MM_DD <- "10-27"
    days_after <- days_after+13
    } 
  if(tryCatch(!is.na(as.Date(date_MM_DD, format = "%m-%d")))){
    min <- as.Date(paste(seq.int(1729, 1844, 1), rep(paste("-", date_MM_DD, sep=""), 116), sep="")) - days_before
    max <- as.Date(paste(seq.int(1729, 1844, 1), rep(paste("-", date_MM_DD, sep=""), 116), sep="")) + days_after
  } else if (date_MM_DD=="Easter"){
    # Get Easter Sunday dates for 1729-1844 (according to Grotefend)
    easter_sundays <- fread("../avis-analysis/data/easter_sunday.csv", encoding="UTF-8")
    min <- as.Date(unlist(easter_sundays - days_before), origin = "1970-01-01")
    max <- as.Date(unlist(easter_sundays + days_after), origin = "1970-01-01")
  } else {
    stop("Date must be either ''Easter'' (= Easter Sunday), ''Christmas'' (Xmas day, 25th), 'Fair' (duration of Basel autumn fait), or an in-year-date in format MM-DD.")
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


#' @export
select_by_length <- function(ids = NULL, coll = c_all, min = 0, max = 1000000, unit = "tokens"){
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


#' @export
select_by_tags <- function(ids = NULL, coll = c_all, tagslist = NULL, headerlist = NULL, manualtagslist = NULL){
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


#' @export
select_by_text <- function(ids = null, coll = c_all, searchlist = ""){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    if(length(searchlist) > 0){
      for (i in 1:length(searchlist)){
        ids <- intersect(ids, names(coll$corpus[grepl(searchlist[i], as.character(coll$corpus), perl = TRUE)]))
      }
    }
    ids
  }
}


#' @export
select_by_reprint_status <- function(ids = NULL, coll = c_all, status = c("reprinted_orig","unreprinted_orig", "postings", "reprints", "other", "ads")){
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


#' @export
select_by_language <- function(ids = NULL, coll = c_all, status = c("de", "fr", "unknown")){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  match.arg(status)
  switch(status,
         de      = intersect(ids, coll$meta[grepl("de",  coll$meta$language), id]),
         fr      = intersect(ids, coll$meta[grepl("fr",  coll$meta$language), id]),
         unknown = intersect(ids, coll$meta[is.na(coll$meta$language), id]))
}