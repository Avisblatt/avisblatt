#' Records by Length
#'
#' Count the number of ads by length bins. Bins can be character or token
#' based
#'
#' @param coll object of class Collection.
#' @param ids character vector of ids to filter for. Defaults to NULL (all).
#' @param boundaries numeric boundaries of length bins
#' @param unit character denoting 'tokens' or 'character'
#' @export
count_records_by_length <- function(coll,
                                    ids = NULL,
                                    boundaries = c(0, 10, 20, 40, 80, 160, 1000),
                                    unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(boundaries) < 2 | !is.numeric(boundaries)){
    stop("The boundaries argument needs to be a list of at least two non-negative numbers.")
    }
  if(!all(diff(boundaries) > 0)){
    stop("The boundaries need to ascending numbers.")
    }
  if(length(ids) == 1){
    if(ids == "all"){
      ids <- coll$meta$id
      }
    }
  dt <- coll$meta
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]
    }
  if(unit == "tokens"){
    dt[, .N, keyby = .(interval = cut(dt$ntoken, boundaries, dig.lab = 4))]
  } else if(unit == "char"){
    dt[, .N, keyby = .(interval = cut(dt$nchar, boundaries, dig.lab = 4))]
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}

#' Average Ad length by Date
#'
#' @param coll object of class Collection
#' @param ids character vector of ids to filter for. Defaults to NULL (all).
#' @param level character year, quarter, month, week. Defaults to NULL (average).
#' @param unit character denoting 'tokens' or 'character'
#' @export
average_length_by_date <- function(coll,
                                   ids = NULL,
                                   level = NULL,
                                   unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids) == 1){
    if(ids=="all"){
      ids <- coll$meta$id
    }
    }
  dt <- coll$meta
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]
    }
  if(unit == "tokens"){
    if(is.null(level)){
      dt[, list(average_length = round(mean(ntokens), 1)),
         by = date][order(date)]
    } else if(level == "year"){
      dt[, list(average_length = round(mean(ntokens), 1)),
         by = list(year = as.numeric(format(date, "%Y")))][order(year)]
    } else if(level == "quarter"){
      dt[, list(average_length = round(mean(ntokens), 1)),
         by = list(year = as.numeric(format(date, "%Y")),
                   quarter = ceiling(as.numeric(format(date, "%m"))/3))][order(year, quarter)]
    } else if(level == "month"){
      dt[, list(average_length = round(mean(ntokens), 1)),
         by = list(year = as.numeric(format(date, "%Y")),
                   month = as.numeric(format(date, "%m")))][order(year, month)]
    } else if(level == "week"){
      dt[, list(average_length = round(mean(ntokens), 1)),
         by = list(year = as.numeric(format(date, "%Y")),
                   week = as.numeric(format(date, "%V")))][order(year, week)]
    } else{
      message("Only supports 'year', 'quarter', 'month' and 'week' based calculation. If level is left NULL, average per issue is calculated.")
    }
  } else if(unit == "char"){
    if(is.null(level)){
      dt[, list(average_length = round(mean(nchar), 1)), by = date][order(date)]
    } else if(level == "year"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")))][order(year)]
    } else if(level == "quarter"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                   quarter = ceiling(as.numeric(format(date, "%m"))/3))][order(year, quarter)]
    } else if(level == "month"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                   month = as.numeric(format(date, "%m")))][order(year, month)]
    } else if(level == "week"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                   week = as.numeric(format(date, "%V")))][order(year, week)]
    } else{
      message("Only supports 'year', 'quarter', 'month' and 'week' based calculation. If level is left NULL, average per issue is calculated.")
    }
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}
