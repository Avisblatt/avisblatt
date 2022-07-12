#' @export
group_records_by_length = function(ids = NULL, coll = c_all, boundaries = c(0, 10, 20, 40, 80, 160, 1000), unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(boundaries)<2|!is.numeric(boundaries)){stop("The boundaries argument needs to be a list of at leats two non-negative numbers - default is c(0, 100000).")}
  if(!all(diff(boundaries)>0)){stop("The boundaries need to ascending numbers.")}
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  if(!is.null(ids)){dt <- dt[id %in% ids,]}
  if(unit == "tokens"){
    dt <- dt[dt$ntoken > boundaries[1]]
    dt <- dt[dt$ntoken < boundaries[length(boundaries)]+1]
    dtgs <- group_by(dt, interval = cut(dt$ntoken, boundaries, dig.lab = 4)) %>% group_split()
    results=list(list())
    for (i in 1:length(dtgs)){
      results[[i]] <- unlist(dtgs[[i]][1])
    }
    names(results) <- levels(dtgs[[1]]$interval)
    results
  } else if(unit == "char"){
    dt <- dt[dt$char > boundaries[1]]
    dt <- dt[dt$char < boundaries[length(boundaries)]+1]
    dtgs <- group_by(dt, interval = cut(dt$nchar, boundaries, dig.lab = 4)) %>% group_split()
    results=list(list())
    for (i in 1:length(dtgs)){
      results[[i]] <- unlist(dtgs[[i]][1])
    }
    names(results) <- levels(dtgs[[1]]$interval)
    results
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}


#' @export
group_records_by_tags = function(ids = NULL, coll = c_all, tagslist = NULL, namelist = NULL){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(namelist)){namelist<-tagslist}
  if(!length(tagslist)==length(namelist)){
    stop("The list of names does not have the same length as the list of search terms.")
  }
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(!is.null(tagslist)){
    results=list(list())
    for (i in 1:length(tagslist)){
      results[[i]] <- intersect(ids, coll$meta[grepl(tagslist[i], coll$meta$tags), id])
    }
    names(results) <- namelist
    results
  } else{
    stop("No tags to group by - tagslist is empty")
  }
}


#' @export
group_records_by_header = function(ids = NULL, coll = c_all, headerlist = NULL, namelist = NULL){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(namelist)){namelist<-headerlist}
  if(!length(headerlist)==length(namelist)){
    stop("The list of names does not have the same length as the list of search terms.")
  }
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(!is.null(headerlist)){
    results=list(list())
    for (i in 1:length(headerlist)){
      results[[i]] <- intersect(ids, coll$meta[grepl(headerlist[i], coll$meta$tags_section), id])
    }
    names(results) <- namelist
    results
  } else{
    stop("No header to group by - headerlist is empty")
  }
}


#' @export
group_records_by_text = function(ids = null, coll = c_all, searchlist = "", namelist = NULL){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(namelist)){namelist<-searchlist}
  if(!length(searchlist)==length(namelist)){
    stop("The list of names does not have the same length as the list of search terms.")
  }
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else if(length(searchlist) > 0){
    results=list(list())
    for (i in 1:length(searchlist)){
      results[[i]] <- intersect(ids, coll$meta[grepl(searchlist[i], as.character(coll$corpus), perl = TRUE), id])
    }
    names(results) <- namelist
    results
  } else{
    stop("No text to group by - searchlist is empty")
  }
}