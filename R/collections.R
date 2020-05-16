#' @importFrom R6 R6Class
MetaInfoRecord <- R6Class("MetaInfoRecord", list(
  id = NULL,
  date = NULL,
  language = NULL,
  tags = NULL,
  tags_manual = NULL,
  initialize = function(id,
                        date = NULL,
                        tags = NULL,
                        tags_manual = NULL,
                        language = NULL){
    self$id <-id
    self$date <- date
    self$tags <- tags
    self$tags_manual <- tags_manual
    self$language <- language
  },
  add_tag = function(tags){
    self$tags <- unique(c(self$tags, tags))
    invisible(self)
  },
  overwrite_tag = function(tags){
    self$tags <- unique(tags)
  },
  set_language = function(language){
    self$language <- unique(language)
  }
))


#'@export
AvisCollection <- R6Class("AvisCollection", list(
  corpus = NULL,
  meta = NULL,
  record_count = NULL,
  docvars_to_meta = NULL,
  transform_docvars = NULL,
  initialize = function(crps,
                        meta_list = NULL,
                        date = NULL,
                        docvars_to_meta = NULL,
                        transform_docvars = NULL){
    # either specify path to avisblatt type of .csv
    # quanteda corpus object
    if(inherits(crps,"corpus")){
      self$corpus <- crps
    } else if(is.character(crps)){
      self$corpus <- avis_create_corpus(crps)
    } else{
      stop("unsupported class. collections can only be initialized from Corpus or links to
           .csvs containing corpora.")
    }

    if(!is.null(docvars_to_meta)){
      # find out which columns to keep, cause
      # we want only to remove the information from corpus
      # will be transferred to meta information
      keep_cols <- setdiff(names(docvars(self$corpus)), docvars_to_meta)
      dv_list <- as.list(docvars(self$corpus)[,docvars_to_meta])
      attr(self$corpus, "docvars") <- docvars(self$corpus)[, keep_cols]


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


    if(is.null(meta_list)){
      l <- lapply(names(self$corpus), function(x){
        # avoid errors because of non-existing docvar
        # transformation which might be the case most
        # of the time
        if(exists("dv_list")){
          dv_vec <- unlist(dv_list[x])
          # remove vector names as idx based on vector length
          # is attached and be confusing w/o any added value
          names(dv_vec) <- NULL
          MetaInfoRecord$new(id = x, date,
                             tags_manual = dv_vec)
        } else {
          MetaInfoRecord$new(id = x, date)
        }
      })
      names(l) <- names(self$corpus)
      self$meta <- list2env(l)
    } else{
      l <- lapply(meta_list, function(x){
        MetaInfoRecord$new(id = x$id,
                           tags = x$tag,
                           date = x$date,
                           language = x$language,
                           tags_manual = x$tags_manual)
      })
      names(l) <- names(meta_list)
      self$meta <- list2env(l)
    }
  },
  count_records_in_collect = function(){
    length(self$corpus)
  },
  bulk_update_tags = function(ids = NULL, tags){
    if(is.null(ids)) {
      list_of_env <- mget(ls(self$meta),
                          self$meta)
    } else{
      list_of_env <- mget(ids, self$meta)
    }

    names(list_of_env) <- ids
    lapply(list_of_env, function(x) x$add_tag(tags))
    invisible(self)
  },
  bulk_update_language = function(ids = NULL, lang){
    if(is.null(ids)) {
      list_of_env <- mget(ls(self$meta),
                          self$meta)
    } else{
      list_of_env <- mget(ids, self$meta)
    }

    names(list_of_env) <- ids
    lapply(list_of_env, function(x) x$set_language(lang))
    invisible(self)
  },
  show_distinct_tags = function(manual = FALSE){
    if(manual){
      l <- unlist(eapply(self$meta, function(x) x$tags_manual),
                  recursive = F)
    } else{
      l <- unlist(eapply(self$meta, function(x) x$tags),
                  recursive = F)
    }

    unique(l)
  },
  search_tags = function(search, regex = FALSE, manual = FALSE){
    if(regex){
      e <- eapply(self$meta, function(x){
        any(grepl(search, ifelse(manual,x$tags_manual,x$tags)))
      })
    } else{
      e <- eapply(self$meta, function(x){
        any(search %in% ifelse(manual,x$tags_manual,x$tags))
      })
    }
    names(e)[unlist(e)]
  }

))





