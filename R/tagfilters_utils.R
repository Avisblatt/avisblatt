#' https://adv-r.hadley.nz/function-factories.html
#' Create a function that contains the filter and returns
#' a function that applies the filter on a corpus object passed
#' to the function.
create_filter <- function(dict){
  function(corp,
           return_corp = TRUE,
           ignore.case = TRUE){
    stopifnot(inherits(corp,"corpus"))
    re_pos <- paste(unlist(dict$pos), collapse = "|")
    tf_pos <- grepl(re_pos, corp, perl = TRUE,
                    ignore.case = ignore.case)

    if(!is.null(dict$neg)){
      re_neg <- paste(unlist(dict$neg), collapse = "|")
      tf_neg <- !grepl(re_neg, corp, perl = TRUE,
                       ignore.case = ignore.case,)

      sel <- tf_pos & tf_neg
    } else{
      sel <- tf_pos
    }

    # if there is provision in the tagfilter
    # to only apply it to the headers
    # listed under dict$applicable,
    # confine sel[elcted ids] to those
    # within said sections
    if(!is.null(dict$applicable)){
      re_applicable <- paste(unlist(dict$applicable), collapse = "|")
      tf_applicable <- grepl(re_applicable, corp$header_tag)
      sel <- sel & tf_applicable
    }

    # Likewise, if tagfilter is specified
    # to be not applied to sections
    # listed under dict$inapplicable,
    # confine sel[elcted ids] to those
    # outside said sections
    if(!is.null(dict$inapplicable)){
      re_inapplicable <- paste(unlist(dict$inapplicable), collapse = "|")
      tf_inapplicable <- !grepl(re_inapplicable, corp$header_tag)
      sel <- sel & tf_inapplicable
    }

    ids <- names(corp[sel])

    # If there is a list of IDs to include
    # under dict$include, add them IF they are in the corpus
    if(!is.null(dict$include)){
      include <- intersect(unlist(dict$include), names(corp))
      ids <- union(ids, include)
    }

    # If there is a list of IDs to exclude
    # under dict$exclude, remove them
    if(!is.null(dict$exclude)){
      exclude <- unlist(dict$exclude)
      ids <- setdiff(ids, exclude)
    }


    if(return_corp) return(corp[ids])
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


