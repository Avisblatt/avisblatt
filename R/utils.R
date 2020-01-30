#' @export
get_text_by_id <- function(corp, ids, n = NULL,
                           identifier = "id",
                           txt = "texts"){
  tf <- docvars(corp, identifier) %in%
    ids

  if(is.null(n)) {
    return(corp$documents[,txt][tf])
  }

  corp$documents[,txt][tf][1:n]


}

#' @export
get_subcorpus_by_id <- function(corp, ids, idvar = "id"){
  corpus_subset(corp,
                (docvars(corp, idvar) %in% ids)
  )
}


