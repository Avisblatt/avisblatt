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


#' Turn Interrupted Print into Regular Print
#'
#' Turn s p a c e into space.
#'
#' @param txt character string
#' @examples
#' s <- "c o o l e r than a polar b e a r's toe n a i l s."
#' purge_s_p_a_c_e(s)
#' @export
purge_s_p_a_c_e <- function(txt){
  splits <- strsplit(txt, "\\s")[[1]]
  more_than_2 <- grepl("\\w{2,}", splits)
  tf <- more_than_2 | c(FALSE, more_than_2[1:(length(more_than_2)-1)])
  # cumsum is an elegant way to keep track of T/F swaps
  out <- split(splits, cumsum(tf))
  # reconstruct
  paste(sapply(out, paste, collapse = ""), collapse = " ")
}

