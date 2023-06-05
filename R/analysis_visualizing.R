#' @import quanteda.textplots 
#' @export
show_wordcloud <- function(ids = NULL, coll = c_all, remove = "", max_words = 200){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    corp <- corpus_subset(c_all$corpus, names(c_all$corpus) %in% ids)
    removal <- c(avis_stop(), remove)
    corp <- corp |>
      tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
      tokens_remove(removal, min_nchar = 3)
    textplot_wordcloud(dfm(corp), max_words = max_words)
  }
}
