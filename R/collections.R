#' @importFrom R6 R6Class
MetaInfoRecord <- R6Class("MetaInfoRecord", list(
  id = NULL,
  date = NULL,
  language = NULL,
  tags = NULL,
  initialize = function(id,
                        date = NULL,
                        tags = NULL,
                        language = NULL){
    self$id <-id
    self$date <- date
    self$tags <- tags
    self$language <- tags
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
AvisCollection <- R6Class("AvisCollection",list(
  corpus = NULL,
  meta = NULL,
  record_count = NULL,
  initialize = function(crps,
                        meta_list = NULL,
                        date = NULL){
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

    if(is.null(meta_list)){
      l <- lapply(names(self$corpus), function(x){
        MetaInfoRecord$new(id = x, date)
      })
      names(l) <- names(self$corpus)
      self$meta <- list2env(l)
    } else{
      l <- lapply(meta_list, function(x){
        MetaInfoRecord$new(id = x$id,
                           tag = x$tag,
                           date = x$date,
                           language = x$language
        )
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
  show_distinct_tags = function(){
    l <- unlist(eapply(self$meta, function(x) x$tags),
                recursive = F)
    unique(l)
  },
  search_tags = function(search, regex = FALSE){
    if(regex){
      e <- eapply(self$meta, function(x){
        any(grepl(search, x$tags))
      })
    } else{
      e <- eapply(self$meta, function(x){
        any(search %in% x$tags)
      })
    }
    names(e)[unlist(e)]
  }

))





