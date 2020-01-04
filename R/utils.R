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



