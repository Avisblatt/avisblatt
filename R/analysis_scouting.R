#' Get Headers from Collection
#'
#' @param coll object of class Collection
#' @param text boolean. Defaults to FALSE.
#' @export
get_headers <- function(coll,
                        text = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(text){
    if(is.null(coll$corpus)){
      stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to get header texts")
    } else{
      as.character(corpus_subset(coll$corpus, isheader == TRUE))}
  } else{
    coll$meta[(isheader), id]
  }
}

#' Get noadvert records from a Collection object
#'
#' This function returns the noadvert records from a Collection object.
#'
#' @param coll A Collection object.
#' @param text A logical value indicating whether to return the text of the noadvert records or their names.
#' @return If text is TRUE, the text of the noadvert records; otherwise, their names.
#' @export
#' @importFrom R6 inherits stopifnot
#' @examples
#' coll <- read_collection("path/to/collection")
#' get_noadverts(coll)
#'
#' @seealso \code{\link{read_collection}}, \code{\link{gather_collections}}
#' @keywords collection
#' @export
get_noadverts <- function(coll,
                          text = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to get the noadvert records.")}

  if(text){
    as.character(corpus_subset(coll$corpus, noadvert == TRUE))
    } else {
      names(corpus_subset(coll$corpus, noadvert == TRUE))
    }
}



#' List similar ads
#'
#' This function calculates the distance between two collections of ad texts and returns the N most similar ads.
#'
#' @param coll A collection object.
#' @param ids1 A vector of ad IDs from the first collection.
#' @param ids2 A vector of ad IDs from the second collection.
#' @param N The number of most similar ads to return.
#' @param min_dist The minimum distance between ads to consider.
#' @param max_dist The maximum distance between ads to consider.
#' @param matrix_limit The maximum number of elements in the distance matrix.
#'
#' @return A matrix with the N most similar ads and their distances.
#'
#' @examples
#' \dontrun{
#' list_similar(coll = coll,
#'              ids1 = c("ad1", "ad2", "ad3"),
#'              ids2 = c("ad4", "ad5", "ad6"),
#'              N = 5,
#'              min_dist = 0,
#'              max_dist = 1000,
#'              matrix_limit = 10000000)
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom quanteda corpus_subset
#'
#' @export
list_similar = function(coll,
                        ids1 = NULL,
                        ids2 = NULL,
                        N = 5,
                        min_dist = 0,
                        max_dist = 1000,
                        matrix_limit = 10000000){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  stopifnot(is.numeric(N) & is.numeric(min_dist) & is.numeric(max_dist))
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to calculate distance between ad texts.")
  } else{
    if(length(ids1)==1){if(ids1=="all"){ids1 <- coll$meta$id}}
    if(length(ids2)==1){if(ids2=="all"){ids2 <- coll$meta$id}}
    corp <- corpus_subset(c_all$corpus,
                          !(c_all$corpus$isheader)
                          &!(c_all$corpus$noadvert))
    corp_a <- corpus_subset(corp, names(corp) %in% ids1)
    corp_b <- corpus_subset(corp, names(corp) %in% ids2)
    if (length(corp_a)*length(corp_b) == 0){
      NULL
    } else if (length(corp_a)*length(corp_b) > matrix_limit){
      message(sprintf("This requires calculating a matrix with %s elements, which can take a while. If you really want this, set matrix_limit to a higher value (default is 10,000,000).",
                      format(length(corp_a)*length(corp_b), big.mark=",")))
    } else {
      dist <- advert_distance(corp_a, corp_b)
      dist[dist > max_dist] <- NA
      dist[dist < min_dist] <- NA
      rownames(dist) <- names(corp_a)
      colnames(dist) <- names(corp_b)
      # if an ad is in both corpus, don't return it as one of the most similar ads to itself
      # set distance measure to NA for such self-comparisons
      # (like in distance tables, where the main diagonal is empty)
      dist[t(sapply(rownames(dist), function(x) grepl(x, colnames(dist))))] <- NA
      # remove rows that contain only NA, i.e. deliver no similar ad of desired properties
      na_rows <- rownames(dist)[apply(dist, 1, function(x) all(is.na(x)))]
      dist <- dist[!(row.names(dist) %in% na_rows),]
      if(nrow(dist>0)){
        sim <- as.data.frame(apply(dist, 1, function(x) {colnames(dist)[which(rank(x, na.last = NA, ties.method = "first")<N+1)]}))
        # Now transpose the matrix
        # if it has more than one column
        # (if it has one column by virtue of N = 1,
        # it seems to get transposed already in the previous line)
        if(N > 1){sim <- t(sim)}
        if(ncol(sim)>0){colnames(sim) <- c(1:ncol(sim))}
        rownames(sim) <- rownames(dist)
        sim
      } else {
        NULL
        }
    }
  }
}
