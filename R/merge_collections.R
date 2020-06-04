
#' Merge One or More Collections Into a One Large Collection
#'
#'
#'
#' @param c_list list of collections. Every element needs to be a 'collection'
#' object from the avisblatt R package.
#' @export
merge_collections <- function(c_list, merge_corpus = TRUE){
  # sanity checks
  if(length(c_list) < 2) stop("You need more than one collection to merge collectionS.")
  if(!any(sapply(c_list, inherits, what = "Collection"))) stop("All elements of the list have to be collections.")

  merged_meta <- rbindlist(lapply(c_list, function(x){
    get("meta", envir = x)
  }))

  if(merge_corpus){
    corpora <- lapply(c_list, function(x){
      get("corpus", envir = x)
    })
    merged_corpus <- do.call("c", corpora)
  } else{
    merged_corpus <- NULL
  }

  Collection$new(merged_corpus, merged_meta)
}


