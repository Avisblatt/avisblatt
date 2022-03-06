#' @export
get_headers <- function(coll = c_all, text = FALSE){
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


#' @export
get_noadverts <- function(coll = c_all, text = FALSE){
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


#' @export
list_similar = function(ids1 = NULL, ids2 = NULL, coll = c_all, N = 5, min_dist = 0, max_dist = 1000, matrix_limit = 10000000){
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