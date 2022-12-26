#' @export
count_records_by_date <- function(ids = NULL, coll = c_all, level = "year", colnames = NULL, per1k = FALSE, trim = TRUE, min = "1729-01-01", max = "1844-12-31", bundle_periods = 1){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  stopifnot(length(ids)>0)
  if(!(level %in% c("year", "quarter", "month", "week", "issue"))){stop("Only supports 'year', 'quarter', 'month', 'week' and 'issue' based counting.")}
  if(is.null(ids)){
    {stop("Nothing to count. No IDs or list of IDs received.")}
  }
  # ids can either be one set of ids or a list of sets of ids;
  # if it is only one set, turn it into a list 
  if(class(ids) == "character"){
    message("Received one set of records. 
    If you want to count several sets of records at once, you need to pass them as a list, e.g.
    ids = list(ids1, ids2, ids3)")
    ids <- list(ids)}
  if(length(ids)==1){if(ids[1]=="all"){ids[1] <- coll$meta$id}}
  
  if(length(ids)!=length(colnames)){
    message("No colnames given or number of colnames does not match dimension of ids. Colnames are created automatically")
    colnames <- sprintf("[%s]",seq(1:length(ids)))
  }
  alldates <- sort(unique(coll$meta[max >= date & date >= min]$date))
  
  if (level == "issue") {
    data <- data.table(date = unique(alldates))
    for(i in 1:length(ids)){
      dt <- coll$meta
      dt <- dt[id %in% ids[[i]],]
      data <- merge(data,
                    dt[, .(N = .N), by = date][order(date)],
                    all.x = TRUE)
    } 
    data$year <- year(data$date) #need year info to merge pop data for per1k = T
    colnames(data) <- c("issue", colnames, "year")
    
  } else if (level == "year"){
    data <- data.table(year = unique(year(alldates)))
    for(i in 1:length(ids)){
      dt <- coll$meta
      dt <- dt[id %in% ids[[i]],]
      data <- merge(data,
                    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")))][order(year)],
                    all.x = TRUE)
    }
    colnames(data) <- c("year", colnames)

  } else if(level == "quarter"){
    data <- unique(data.table(year = as.numeric(format(alldates, "%Y")),
                              quarter = ceiling(as.numeric(format(alldates, "%m"))/3)))
    for(i in 1:length(ids)){
      dt <- coll$meta
      dt <- dt[id %in% ids[[i]],]
      data <- merge(data,
                    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                                              quarter = ceiling(as.numeric(format(date, "%m"))/3))][order(year, quarter)],
                    all.x = TRUE)
    }
    colnames(data) <- c("year", "quarter", colnames)

  } else if(level == "month"){
    data <- unique(data.table(year = as.numeric(format(alldates, "%Y")),
                              month = as.numeric(format(alldates, "%m"))))
    for(i in 1:length(ids)){
      dt <- coll$meta
      dt <- dt[id %in% ids[[i]],]
      data <- merge(data,
                    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                                              month = as.numeric(format(date, "%m")))][order(year, month)],
                    all.x = TRUE)
    }
    colnames(data) <- c("year", "month", colnames)

  } else if(level == "week"){
    data <- unique(data.table(year = as.numeric(format(alldates, "%Y")),
                              week = as.numeric(format(alldates, "%V"))))
    for(i in 1:length(ids)){
      dt <- coll$meta
      dt <- dt[id %in% ids[[i]],]
      data <- merge(data,
                    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                                              week = as.numeric(format(date, "%V")))][order(year, week)],
                    all.x = TRUE)
    }
    colnames(data) <- c("year", "week", colnames)
  }

  data[is.na(data)] <- 0
  if(per1k){
    pop <- fread("../avis-data/data/population.csv")
    pop$N <- 1000/pop$N
    pop <- pop[1:116]
    colnames(pop) <- c("year", "coefficient")
    data <- merge(data, pop, all = TRUE, by = "year")
    data[,2:(1+length(ids))]
    data[,(ncol(data)-length(ids)):(ncol(data)-1)] <- data[,(ncol(data)-length(ids)):(ncol(data)-1)] * data$coefficient
    data$coefficient <- NULL
  }
  # For count per issue, a year column needed to be included to merge the population data. 
  # Remove it before output
  if (level=="issue"){data$year <- NULL}
  
  if (trim & nrow(data)>1){
    # add a column with cumulating row sums, 
    # then remove all rows for which this is zero, i.e.,
    # remove the leading block of rows containing nothing but zeros
    data$z <- cumsum(rowSums(data[,(ncol(data) - length(colnames) +1):ncol(data)]))
    data <- data[z!=0]
    data$z <- NULL
    # same in reverse: from bottom to top,
    # to remove the concluding block of rows with just zeros
    data$z <- rev(cumsum(rev(rowSums(data[,(ncol(data) - length(colnames) +1):ncol(data)]))))
    data <- data[z!=0]
    data$z <- NULL
  }
  if (bundle_periods > 1){
    data$group <- floor((row(data[,1])-0.5)/bundle_periods)
    if (level == "issue"){
      data$date <- data$issue
      data$issue <- NULL
    } else if (level == "week"){
      data$date <- paste(data$year, sprintf("%02d", data$week), sep = ".")
      data$year <- NULL
      data$week <- NULL
    } else if (level == "month"){
      data$date <- paste(data$year, sprintf("%02d", data$month), sep = ".")
      data$year <- NULL
      data$month <- NULL
    } else if (level == "quarter"){
      data$date <- paste(data$year, data$quarter, sep = ".")
      data$year <- NULL
      data$quarter <- NULL
    } else if (level == "year"){
      data$date <- data$year
      data$year <- NULL
    } 
    period_dates <- merge(aggregate(date ~ group, data, head, 1),
                          aggregate(date ~ group, data, tail, 1),
                          by = "group")
    sum_data   <- aggregate(x = data[,1:length(ids)], by = list(data$group), sum)
    colnames(sum_data) <- c("group", colnames)
    data <- merge(period_dates, sum_data, by = "group")
    data$group <- NULL
    colnames(data) <- c("start", "end", colnames)
  }
  data
}


#' @export
count_records_by_length <- function(ids = NULL, coll = c_all, boundaries = c(0, 10, 20, 40, 80, 160, 1000), unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(boundaries)<2|!is.numeric(boundaries)){stop("The boundaries argument needs to be a list of at leats two non-negative numbers - default is c(0, 100000).")}
  if(!all(diff(boundaries)>0)){stop("The boundaries need to ascending numbers.")}
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  if(!is.null(ids)){dt <- dt[id %in% ids,]}
  if(unit == "tokens"){
    dt[, .N, keyby = .(interval=cut(dt$ntoken, boundaries, dig.lab = 4))]
  } else if(unit == "char"){
    dt[, .N, keyby = .(interval=cut(dt$nchar, boundaries, dig.lab = 4))]
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}


#' @export
average_length_by_date <- function(ids = NULL, coll = c_all, level = NULL, unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  if(!is.null(ids)){dt <- dt[id %in% ids,]}
  if(unit == "tokens"){
    if(is.null(level)){
      dt[, list(average_length = round(mean(ntokens), 1)), by = date][order(date)]
    } else if(level == "year"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")))][order(year)]
    } else if(level == "quarter"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                     quarter = ceiling(as.numeric(format(date, "%m"))/3))][order(year, quarter)]
    } else if(level == "month"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                     month = as.numeric(format(date, "%m")))][order(year, month)]
    } else if(level == "week"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")),
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