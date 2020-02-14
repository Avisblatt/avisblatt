#' https://adv-r.hadley.nz/function-factories.html
#' Create a function that contains the filter and returns
#' a function that applies the filter on a corpus object passed
#' to the function.
create_filter <- function(dict){
  function(corp,
           identifier = "id",
           ignore.case = TRUE){
    stopifnot(inherits(corp,"corpus"))
    re_pos <- paste(unlist(dict$pos), collapse = "|")

    tf_pos <- grepl(re_pos, corp$documents$texts,
                    ignore.case = ignore.case)

    if(!is.null(dict$neg)){
      re_neg <- paste(unlist(dict$neg), collapse = "|")
      tf_neg <- !grepl(re_neg, corp$documents$texts,
                       ignore.case = ignore.case)

      sel <- tf_pos & tf_neg
    } else{
      sel <- tf_pos
    }

    ids <- docvars(corp, identifier)[sel]
    ids
  }
}


merge_filters <- function(...){
  l <- list(...)
  dict <- list()
  dict$pos <- unlist(lapply(l, function(x){
    x$tagfilters$pos
  }),
  recursive = F)
  dict$neg <- unlist(lapply(l, function(x){
    x$tagfilters$neg
  }),
  recursive = F)

  create_filter_output(dict)

}

create_filter_output <- function(dict){
  output <- list()
  output$filtrate <- create_filter(dict)
  output$tagfilters <- dict
  output
}


