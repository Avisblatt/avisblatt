# MiRecord
#   - id
#   - date
#   - lang
#   - tag
#   - tags_manual
#
# AvisCollection
#   - id (vec) ok
#   - date (vec) ok
#   - lang (vec) ok
#   - tags (list of vectors)
#   - corpus ok
#   - list_record(id) ok
#   - subset_collect ok
#   - add_record
#   - remove_record
#   - initialize ok
#   - search_tags ok

corp <- ac$corpus
docvars(corp)$date

clean_manual_tags <- function(x){
  unique(gsub("(^[0-9]{2})(.+)","\\2",
              unlist(strsplit(x, ","))))
}



collection <- R6Class("collection", list(
  corpus = NULL,
  meta = NULL,
  initialize = function(crps,
                        docvars_to_meta = NULL,
                        transform_docvars = NULL){

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

    meta_table <- data.table(
      id = names(self$corpus),
      date = as.Date(docvars(self$corpus, "date")),
      tags = list(),
      tags_manual = dv_list,
      language = NA_character_
    )
    self$meta <- meta_table
  },
  add_tags = function(ids = NULL, tag_list){
    # TODO: append option
    if(is.null(ids)) return(self$meta[, tags := tag_list])
    self$meta[id %in% ids, tags := tag_list]

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
      dt[, .(N = .N), by = list(month = format(date, "%Y-%m"))]
    } else if(level == "week"){
      dt[, .(N = .N), by = list(week = format(date, "%Y-%V"))]
    } else{
      message("Only supports year, month and week ased aggregation.")
    }

  },
  search_tags = function(search, search_manual = FALSE){
    if(search_manual) return(self$meta[grepl(search, self$meta$tags_manual), id])
    self$meta[grepl(search, self$meta$tags), id]
    },
  subset_collect = function(ids){
    # this should be used with deep clone
    # otherwise the current collection istance is subsetted!
    self$corpus <- self$corpus[ids]
    self$meta <- self$meta[id %in% ids,]
  }
))


c2 <- collection$new("../avis-data/raw_data/orig_1834.csv",
                          docvars_to_meta = c("adcontent","adtype"),
                          transform_docvars = clean_manual_tags)

dd <- c2$search_tags("drucksachen",search_manual = T)

c2$count_records_by_date(dd,level = "month")

collect <- collection$new(corp)
tt <- list(c("this","and","that"),
           c("was","missing"))

ii <- names(corp)[1:2]

collect$add_tags(ii,tt)

collect$list_records()
collect$count_records_by_date(ids=ii,level = "week")
collect$add_language(lang = "de")

collect$meta




format(as.Date("2018-12-20"),"%V")


collect$search_tags("was")

xx <- collect$meta
xx[id %in% ii, .(N = .N), by = date]
