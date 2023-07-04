#' Tagfilter Practices
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for practices related ads and finds ads that 
#' contain different practices from spinning to weaving or cooking.
#'
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
#' 
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_practices
#' @seealso tagfilters
NULL


#' Dictionary spinning
#' @rdname tagfilter_practices
#' @export
tagfilter_spinning <- function(){
  dict <- list()
  dict$pos <- list(
    spin = "spin"
  )

  create_filter_output(dict)
}

#' Dictionary weaving
#' @rdname tagfilter_practices
#' @export
tagfilter_weaving <- function(){
  dict <- list()
  dict$pos <- list(
    weave = "weben"
  )

  create_filter_output(dict)
}


#' Dictionary cook
#' @rdname tagfilter_practices
#' @export
tagfilter_cooking <- function(){
  dict <- list()
  dict$pos <- list(
    cook_1 = "kochen"
  )

  create_filter_output(dict)
}



#' Dictionary cook
#' @rdname tagfilter_practices
#' @export
tagfilter_knitting <- function(){
  dict <- list()
  dict$pos <- list(
    knit = "stricken"
  )

  create_filter_output(dict)
}

