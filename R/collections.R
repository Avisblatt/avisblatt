#' @importFrom quanteda docvars
#' @import data.table
#' @importFrom R6 R6Class
#' @export
Collection <- R6Class("Collection", list(
  corpus = NULL,
  meta = NULL,
  initialize = function(crps,
                        meta_table = NULL,
                        preprocess_docvars = NULL,
                        docvars_to_meta = NULL,
                        transform_docvars = NULL){
    if(!is.null(crps)){
      if(inherits(crps,"corpus")){
        self$corpus <- crps
      } else if(is.character(crps)){
        self$corpus <- avis_create_corpus(crps)
      } else{
        stop("unsupported class. collections can only be initialized from Corpus or links to
           .csvs containing corpora.")
      }

      if(!is.null(preprocess_docvars)){
        # freizo exports are not really homogeneous
        # this to standardize these inputs in order to
        # handle them independently of the period these inputs
        # were composed. Account for the woes caused by manual work ...
        self$corpus <- pre_process_docvars(self$corpus)
      }

      if(!is.null(docvars_to_meta)){
        # find out which columns to keep, cause
        # we want only to remove the information from corpus that
        # will be transferred to meta information
        keep_cols <- c("docname_", "docid_", "segid_",
                       setdiff(names(docvars(self$corpus)), docvars_to_meta))
        dv_list <- as.list(docvars(self$corpus)[,docvars_to_meta])
        attr(self$corpus, "docvars") <- attr(self$corpus, "docvars")[,keep_cols]


        # TODO discuss whether we want to
        # keep this consolidation of meta information
        # the alternative would be a nested list
        if(length(docvars_to_meta) > 1){
          dv_list <- as.list(do.call("paste", c(dv_list, sep = ",")))
        }

        if(!is.null(transform_docvars)){
          dv_list <- lapply(dv_list, transform_docvars)
        }

        names(dv_list) <- names(self$corpus)
      }

    } else {
      self$corpus <- NULL
    }

    # meta information can be passed on directly when read from
    # a file before...
    if(is.null(meta_table)){

      # ifelse is not for vectors !
      # use if() else here.
      if(exists("dv_list")){
        tm <- dv_list
      } else{
        tm <- list()
      }

      if("isheader" %in% names(docvars(self$corpus))){
        is_h <- docvars(self$corpus, "isheader")
        is_h[is.na(is_h)] <- 0
        is_h <- is_h
      } else{
        is_h <- NA
      }

      meta_table <- data.table(
        id = names(self$corpus),
        isheader = as.logical(is_h),
        date = as.Date(docvars(self$corpus, "date")),
        tags = list(),
        tags_manual = tm,
        language = NA_character_
      )
    }
    self$meta <- meta_table
  },
  add_tags = function(ids = NULL, tags_vec, overwrite = FALSE){
    if(is.null(ids)){
      if(overwrite){
        return(self$meta[, tags := tag_vec])
      } else {
        return(self$meta[, tags := current_tag])
      }
    }
    if(!overwrite){
      current_tag <- self$meta[id %in% ids, tags]
      current_tag <- lapply(current_tag, function(x, tags_vec){
        unique(c(x, tags_vec))
      }, tags_vec = tags_vec)
      self$meta[id %in% ids, tags := current_tag]
    } else {
      self$meta[id %in% ids, tags := tags_vec]
    }
  },
  add_language = function(ids = NULL, lang){
    if(is.null(ids)) return(self$meta[, language := lang])
    self$meta[id %in% ids, language := lang]

  },
  list_records = function(ids = NULL){
    if(is.null(ids)) return(self$meta)
    self$meta[id %in% ids,]
  },
  count_records_by_date = function(ids = NULL,
                                   level = NULL){
    dt <- self$meta
    if(!is.null(ids)){
      dt <- dt[id %in% ids,]
    }
    if(is.null(level)){
      dt[, .(N = .N), by = date]
    } else if(level == "year"){
      dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")))]
    } else if(level == "month"){
      dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                                month = as.numeric(format(date, "%m")))]
    } else if(level == "week"){
      dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                                week = as.numeric(format(date, "%V")))]
    } else{
      message("Only supports year, month and week ased aggregation.")
    }

  },
  apply_tagfilters = function(flist, nms = NULL){
    if(is.null(names(flist))) stop("List of filter functions needs to be named (using the names of the function).")
    if(!is.null(nms) & length(nms) != length(flist)) stop("There have to be as many tag labels as tags.")
    if(is.null(nms)){
      nms <- gsub("tagfilter_", "", names(flist))
    }
    e <- sprintf("names(flist$%s$filtrate(self$corpus))", names(flist))
    out <- lapply(e,function(x) eval(parse(text = x)))
    names(out) <- nms
    lapply(names(out),function(x){
      self$add_tags(out[[x]], x)
    })
    invisible(self)
  },
  search_tags = function(search, search_manual = FALSE){
    if(search_manual) return(self$meta[grepl(search, self$meta$tags_manual), id])
    self$meta[grepl(search, self$meta$tags), id]
  },
  show_distinct_tags = function(manual = FALSE){
    if(manual){
      unique(unlist(self$meta$tags_manual))
    } else{
      unique(unlist(self$meta$tags))
    }
  },
  get_headers = function(){
    self$meta[(isheader), id]
  },
  subset_collect = function(ids){
    # this should be used with deep clone
    # otherwise the current collection istance is subsetted!
    self$corpus <- self$corpus[ids]
    self$meta <- self$meta[id %in% ids,]
  }

))
