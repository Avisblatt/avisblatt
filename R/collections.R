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
        ntokens = ntoken(texts(self$corpus)),
        nchar = nchar(texts(self$corpus)),
        language = NA_character_
      )
    }
    self$meta <- meta_table
  },
  add_tags = function(ids = NULL, tags_vec, overwrite = FALSE){
    if(is.null(ids)){
      current_tag <- self$meta[, tags]
    } else{
      current_tag <- self$meta[id %in% ids, tags]
    }

    # simplest case, no ids assumes all tags get replaced by
    # tag vector specified.
    if(overwrite & is.null(ids)){
      return(self$meta[, tags := tags_vec])
    }

    # only those tags of the ids specified in the ids
    # vector get replaced
    if(overwrite & !is.null(ids)){
      return(self$meta[id %in% ids, tags := tags_vec])
    }

    # we can run this for all cases because of the
    # above return statements, simply appends
    # tag vector to whatever current tag is w/o overwriting
    current_tag <- lapply(current_tag, function(x, tags_vec){
      unique(c(x, tags_vec))
    }, tags_vec = tags_vec)


    if(is.null(ids)){
      return(self$meta[, tags := current_tag])
    } else{
      return(self$meta[id %in% ids, tags := current_tag])
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
      dt[, .(N = .N), by = date][order(date)]
    } else if(level == "year"){
      dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")))][order(year)]
    } else if(level == "month"){
      dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                                month = as.numeric(format(date, "%m")))][order(year, month)]
    } else if(level == "week"){
      dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                                week = as.numeric(format(date, "%V")))][order(year, week)]
    } else{
      message("Only supports year, month and week based aggregation.")
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
  get_headers = function(text = FALSE){
    if(text){
      if(is.null(self$corpus)){
        stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to get header texts")
      } else{
        texts(corpus_subset(self$corpus, isheader == TRUE))}
    } else{
      self$meta[(isheader), id]
    }
  },
  get_reprints = function(status = c("reprinted_orig","unreprinted_orig", "no_reprints", "reprints", "other")){
    match.arg(status)
    switch(status,
           reprinted_orig = self$meta[grepl("original", self$meta$reprint_of), id],
           unreprinted_orig = self$meta[grepl("none", self$meta$reprint_of), id],
           no_reprints = self$meta[grepl("original|none", self$meta$reprint_of), id],
           other = self$meta[is.na(c_all$meta$reprint_of), id],
           # and finally reprints, i.e. all that is not "NA", "none", "original",
           # i.e. does not start with N, n or o
           reprints = self$meta[grepl("^[^Nno]", self$meta$reprint_of), id])
  },
  select_reprints = function(ids = NULL, status = c("reprinted_orig","unreprinted_orig", "no_reprints", "reprints", "other")){
    match.arg(status)
    switch(status,
           reprinted_orig = intersect(ids, self$meta[grepl("original", self$meta$reprint_of), id]),
           unreprinted_orig = intersect(ids, self$meta[grepl("none", self$meta$reprint_of), id]),
           no_reprints = intersect(ids, self$meta[grepl("original|none", self$meta$reprint_of), id]),
           other = self$meta[grepl("NA", self$meta$reprint_of), id],
           # and finally reprints, i.e. all that is not "NA", "none", "original",
           # i.e. does not start with N, n or o
           reprints = intersect(ids, self$meta[grepl("^[^Nno]", self$meta$reprint_of), id]))
  },
  select_by_length = function(ids = NULL, min = 0, max = 1000000, unit = "tokens"){
    if(unit == "tokens"){
      ids <- intersect(ids, self$meta[ntokens >= min, id])
      intersect(ids, self$meta[ntokens <= max, id])
    } else if(unit == "char"){
      ids <- intersect(ids, self$meta[nchar >= min, id])
      intersect(ids, self$meta[nchar <= max, id])
    } else{
      message("Unit must be 'tokens' (default) or 'char' for characters")
    }
  },
  select_text = function(ids = null, searchlist = ""){
    if(is.null(self$corpus)){
      stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
    } else{
      if(length(searchlist) > 0){
        for (i in 1:length(searchlist)){
          ids <- intersect(ids, self$meta[grepl(searchlist[i], texts(self$corpus)), id])
        }
      }
      ids
    }
  },
  search_text = function(searchlist = ""){
    if(is.null(self$corpus)){
      stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
    } else{
      ids <- self$meta[grepl(searchlist[1], texts(self$corpus)), id]
      if(length(searchlist) > 1){
        for (i in 2:length(searchlist)){
          ids <- intersect(ids, self$meta[grepl(searchlist[i], texts(self$corpus)), id])
        }
      }
      ids
    }
  },
  search_tags = function(search, search_type = c("tags","manual","header")){
    match.arg(search_type)
    switch(search_type,
           tags = self$meta[grepl(search, self$meta$tags), id],
           manual = self$meta[grepl(search, self$meta$tags_manual), id],
           header = self$meta[grepl(search, self$meta$tags_section), id])
  },
  search_tags2 = function(tagslist = "", headerlist = "", manualtagslist = ""){
    tags <- self$meta[grepl(tagslist[1], self$meta$tags), id]
    if(length(tagslist) > 1){
      for (i in 2:length(tagslist)){
        tags <- intersect(tags, self$meta[grepl(tagslist[i], self$meta$tags), id])
      }
    }
    header <- self$meta[grepl(headerlist[1], self$meta$tags_section), id]
    if(length(headerlist) > 1){
      for (i in 2:length(headerlist)){
        header <- intersect(header, self$meta[grepl(headerlist[i], self$meta$tags_section), id])
      }
    }
    manual <- self$meta[grepl(manualtagslist[1], self$meta$tags_manual), id]
    if(length(manualtagslist) > 1){
      for (i in 2:length(manualtagslist)){
        manual <- intersect(manual, self$meta[grepl(manualtagslist[i], self$meta$tags_manual), id])
      }
    }
    intersect(tags, intersect(manual, header))
  },
  show_distinct_tags = function(manual = FALSE){
    if(manual){
      unique(unlist(self$meta$tags_manual))
    } else{
      unique(unlist(self$meta$tags))
    }
  },
  subset_collect = function(ids){
    # this should be used with deep clone
    # otherwise the current collection istance is subsetted!
    self$corpus <- self$corpus[ids]
    self$meta <- self$meta[id %in% ids,]
  },
  remove_records = function(ids, verbose = TRUE){
    # very similar to subset, but just a negative selection
    prev_l <- length(self$corpus)
    keep_tf <- names(self$corpus)[!(names(self$corpus) %in% ids)]
    self$corpus <- self$corpus[keep_tf]
    current_l <- length(self$corpus)
    self$meta <- self$meta[!(id %in% ids),]
    if(verbose){
      message(sprintf("%d record(s) removed from the collection.",
                      prev_l - current_l))
    }

  }

))
