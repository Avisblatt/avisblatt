#' Filter Quanteda Corpus: Charity inside charityheader section
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


#' Filter Quanteda Corpus: Travel opportunities in travelheader section
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
#' @export
tagfilter_travel2 <- function(){
  dict <- list()
  dict$pos <- list(
    
  )
  dict$neg <- list(
    
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Placement
#' @export
tagfilter_placement <- function(){
  dict <- list()
  dict$pos <- list(
    
  )
  dict$neg <- list(
    
  )
  create_filter_output(dict)
}