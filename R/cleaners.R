
#' Return all Document IDs of a Particular Language
#'
#' Based in the textcat package, this function tries to detect a
#' given language in a document corpus and returns all document ids of
#' documents written in the desired language. While German is hard to the
#' detect for historical texts, it does a very good job seperating French
#' and German when looking for French texts.
#'
#' @param corp quanteda corpus object
#' @param lang character language to detect, defaults to french.
#' @return document ids of language
#' @importFrom textcat textcat
#' @export
detect_lang <- function(corp, lang = "french"){
  pos <- which(textcat(corp$documents[,"texts"]) == lang)
  corp$documents[pos,"id"]
}


