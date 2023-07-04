#' Tagfilter opportunity
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads concerning specific opportunities, like 
#' joining in a stage ride to another town, or in charitable action.
#'
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
#' 
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_opportunity
#' @seealso tagfilters
NULL


#' Filter Quanteda Corpus: Travel opportunities in travelheader section
#' @name tagfilter_opportunity
#' @export
tagfilter_travel1 <- function(){
  dict <- list()
  dict$applicable <- list("travelheader")
  dict$pos <- list(
    all = "." #
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Travel opportunities outside travelheader section
#' @name tagfilter_opportunity
#' @export
tagfilter_travel2 <- function(){
  dict <- list()
  dict$pos <- list(
    
  )
  dict$neg <- list(
    
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Charity inside charityheader section
#' @name tagfilter_opportunity
#' @export
tagfilter_charity1 <- function(){
  dict <- list()
  dict$applicable <- list("charityheader")
  dict$pos <- list(
    all = "." #
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Charity outside charityheader section
#' @name tagfilter_opportunity
#' @export
tagfilter_charity2 <- function(){
  dict <- list()
  dict$pos <- list(
    test = "Barmherz" 
  )
  dict$neg <- list(
    
  )
  create_filter_output(dict)
}