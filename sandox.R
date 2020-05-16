# MiRecord
#   - id
#   - date
#   - lang
#   - tag
#   - tags_manual
#
# AvisCollection
#   - id (vec)
#   - date (vec)
#   - lang (vec)
#   - tags (list of vectors)
#   - corpus
#   - list_record(id)
#   - subset_collect
#   - add_record
#   - remove_record
#   - initialize
#   - search_tags

corp <- ac$corpus
docvars(corp)$date

collection <- R6Class("collection", list(
  corpus = NULL,
  meta = NULL,
  initialize = function(crps){
    meta_table <- data.table(
      id = names(crps),
      date = as.Date(docvars(crps, "date")),
      tags = list()
    )
    self$corpus <- crps
    self$meta <- meta_table
  },
  add_tags = function(ids = NULL, tag_list){
    self$meta[id %in% ids, tags := tag_list]

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
  search_tags = function(search){
    self$meta[grepl(search, self$meta$tags), id]
    },
  subset_collect = function(ids){
    # this should be used with deep clone
    # otherwise the current collection istance is subsetted!
    self$corpus <- self$corpus[ids]
    self$meta <- self$meta[id %in% ids,]
  }
))

collect <- collection$new(corp)
tt <- list(c("this","and","that"),
           c("was","missing"))

collect$add_tags(ii,tt)
collect$list_records()

collect$count_records_by_date(level = "week")

format(as.Date("2018-12-20"),"%V")


collect$search_tags("was")

xx <- collect$meta
xx[id %in% ii, .(N = .N), by = date]
