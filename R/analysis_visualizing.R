#' Convenience Function to Create Quanteda Wordclouds
#'
#' @description This function creates a wordcloud from a corpus of text.
#' @param coll A collection object containing the corpus.
#' @param ids A vector of document ids to include in the wordcloud.
#' @param remove A vector of words to remove from the corpus.
#' @param max_words The maximum number of words to include in the wordcloud.
#' @return A wordcloud plot.
#'
#' @import quanteda.textplots
#' @export
show_wordcloud <- function(coll,
                           ids = NULL,
                           remove = "",
                           max_words = 200){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = TRUE in read_collections to be able to search in texts")
  } else{
    corp <- corpus_subset(coll$corpus, names(coll$corpus) %in% ids)
    removal <- c(avis_stop(), remove)
    corp <- corp |>
      tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
      tokens_remove(removal, min_nchar = 3)
    textplot_wordcloud(dfm(corp), max_words = max_words)
  }
}
