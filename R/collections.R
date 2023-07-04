#' R6 Class Representing a Collection
#'
#' @description
#' Avisblatt collection of data and meta data.
#'
#' @details
#' Collections can be written to disk in two linked files: csv and json.
#' @field corpus object of class Corpus
#' @field meta object of class meta
Collection <- R6Class("Collection", list(
  corpus = NULL,
  meta = NULL,
  #' @param crps object of class quanteda corpus.
  #' @param meta_table meta information data.table
  #' @param preprocess_docvars docvars
  #' @param docvars_to_meta docvars to meta
  #' @param transform_docvars transform docvars
  #' @importFrom quanteda docvars
  #' @import data.table
  #' @importFrom R6 R6Class
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
        ntokens = ntoken(as.character(self$corpus)),
        nchar = nchar(as.character(self$corpus)),
        language = NA_character_
      )
    }
    self$meta <- meta_table
  },
  #' @param ids character vector of document ids
  #' @param tags_vec add vector of tags
  #' @param overwrite boolean should existing tags be overwritten. Defaults to FALSE
  add_tags = function(ids = NULL,
                      tags_vec,
                      overwrite = FALSE){
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
  #' @param ids character vector of ids
  #' @param lang character 2-digit ISO language
  add_language = function(ids = NULL, lang){
    if(is.null(ids)) return(self$meta[, language := lang])
    self$meta[id %in% ids, language := lang]

  },
  #' @param ids character vector of ids. Defaults to NULL, selecting all.
  list_records = function(ids = NULL){
    if(is.null(ids)) return(self$meta)
    self$meta[id %in% ids,]
  },
  #' @param flist list of filters
  #' @param nms character names
  #' @param ignore_case boolean Should case be ignored? Defaults to FALSE.
  apply_tagfilters = function(flist,
                              nms = NULL,
                              ignore_case = FALSE){
    if(is.null(names(flist))) stop("List of filter functions needs to be named (using the names of the function).")
    if(!is.null(nms) & length(nms) != length(flist)) stop("There have to be as many tag labels as tags.")
    if(is.null(nms)){
      nms <- gsub("tagfilter_", "", names(flist))
    }
    if(ignore_case){
      e <- sprintf("names(flist$%s$filtrate(self$corpus, ignore.case = T))", names(flist))
    } else {
      e <- sprintf("names(flist$%s$filtrate(self$corpus, ignore.case = F))", names(flist))
      }
    out <- lapply(e,function(x) eval(parse(text = x)))
    names(out) <- nms
    lapply(names(out),function(x){
      self$add_tags(out[[x]], x)
    })
    invisible(self)
  },
  #' @param ids character list of ids
  subset_collect = function(ids){
    # this should be used with deep clone
    # otherwise the current collection instance is subsetted!
    self$corpus <- self$corpus[ids]
    self$meta <- self$meta[id %in% ids,]
  },
  #' @param ids character vector of ids
  #' @param verbose boolean should things be verbose? Defaults to TRUE.
  remove_records = function(ids,
                            verbose = TRUE){
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
