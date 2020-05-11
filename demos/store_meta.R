library(jsonlite)
library(R6)

MetaInfoRecord <- R6Class("MetaInfoRecord", list(
  id = NULL,
  year = NULL,
  language = NULL,
  date = NULL,
  tags = NULL,
  initialize = function(id,
                        year = NULL,
                        date = NULL,
                        tags = NULL,
                        language = NULL){
    self$id <-id
    self$year <- year
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

AvisCollection <- R6Class("AvisCollection",list(
  corpus = NULL,
  meta = NULL,
  record_count = NULL,
  initialize = function(crps,
                        meta_list = NULL,
                        year = NULL){
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
        MetaInfoRecord$new(id = x, year)
      })
      names(l) <- names(self$corpus)
      self$meta <- list2env(l)
    } else{
      l <- lapply(meta_list, function(x){
        MetaInfoRecord$new(id = x$id,
                           tag = x$tag,
                           date = x$date,
                           language = x$language,
                           year = x$year
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




write_collection <- function(x,
                             name_on_disk,
                             pretty_json = TRUE,
                             zip = FALSE){
  # sanity checks
  stopifnot(inherits(x, "AvisCollection"))
  stopifnot(inherits(x, "R6"))

  # name of the two files
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  # meta information and data are treated separately
  # following the swissdata idea (github.com/swissdata/demo)
  # Meta information to JSON ##############################
  # turn all environments to lists
  # environments work with reference and thus better than lists
  # for in memory updates. lists are easier to handle when writing
  # to a JSON string.
  ee <- eapply(x$meta, as.list.environment)
  # data slots need to be updated when collection class
  # changes as this set helps to distinguish non-data slots
  # such as functions from data slots...
  data_slots <- c("id","tags","year","date","language")
  li <- lapply(ee, function(e){
    n <- names(e)
    sel <- n[n %in% data_slots]
    e[sel]
  })

  writeLines(
    toJSON(li, pretty = pretty_json,
           auto_unbox = TRUE,
           null = "null"),
    meta_file
  )
  message(sprintf("Meta information written to %s",meta_file))

  dt <- data.table(
    id = names(x$corpus),
    collection_text = texts(x$corpus),
    docvars(x$corpus))
  fwrite(dt, file = data_file)
  message(sprintf("Data written to %s",data_file))

  if(zip){
    zip_file <- paste0(name_on_disk,".zip")
    zip(zip_file, c(data_file, meta_file))
    file.remove(data_file)
    file.remove(meta_file)
    message("Zip archive containing data and meta data created.")
  }
}

name_on_disk = "collection_1834.csv"

read_collection <- function(name_on_disk){
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  dt <- fread(data_file)
  crps <- corpus(dt, docid_field = "id",
                 text_field = "collection_text")
  mi <- fromJSON(meta_file)
  ac <- AvisCollection$new(crps, mi)
  ac
}


io <- read_collection("collection_1834")
identical(io$corpus,
          avis_1834$corpus,
          ignore.environment = TRUE)


str(avis_1834$meta)

io$count_records_in_collect()
io$search_tags("more")


io$meta$`0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a1`
m <- write_collection(avis_1834,"collection_1834")
