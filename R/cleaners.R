#' Return all Document IDs of a Particular Language
#'
#' Based in the textcat package, this function tries to detect a
#' given language in a document corpus and returns all document ids of
#' documents written in the desired language. While German is hard to
#' detect for in these historical texts, it does a very good job
#' seperating French and German when looking for French texts.
#'
#' @param corp quanteda corpus object
#' @param lang character language to detect, defaults to NULL, simply returning results for all elements / all detected languages.
#' @return document ids of language
#' @importFrom textcat textcat
#' @export
detect_lang <- function(corp, lang = NULL){
  if(is.null(lang)){
    textcat(corp)
  } else {
    pp <- which(textcat(corp) == lang)
    names(corp[pp])
  }

}




