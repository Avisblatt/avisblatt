validate_filter <- function(corp, filter_ids,
                            search_col, pattern,
                            docid = "id"){
  doc_ids <- corp$documents[,docid]
  human_class_ids <- doc_ids[grepl(pattern,
                                   corp$documents[,search_col])]
  filter_T_hc_T <- doc_ids[(doc_ids %in% filter_ids) &
                             (doc_ids %in% human_class_ids)]
  filter_T_hc_F <- doc_ids[(doc_ids %in% filter_ids) &
                             !(doc_ids %in% human_class_ids)]
  hc_T_filter_F <- doc_ids[!(doc_ids %in% filter_ids) &
                             (doc_ids %in% human_class_ids)]

  hc_F_filter_F <- doc_ids[!(doc_ids %in% filter_ids) &
                             !(doc_ids %in% human_class_ids)]

  overview <- tibble(
    filter_T = c(length(filter_T_hc_T),
                 length(filter_T_hc_F)),
    filter_F = c(length(hc_T_filter_F),
                 length(hc_F_filter_F))
  )

  output <- list()
  output$filter_T_hc_T <- filter_T_hc_T
  output$filter_T_hc_F <- filter_T_hc_F
  output$hc_T_filter_F <- hc_T_filter_F
  output$hc_F_filter_F <- hc_F_filter_F
  output$overview <- overview
  output

  class(output) <- append("avis_confusion_matrix",
                          class(output))

  output
}

print.avis_confusion_matrix <- function(x){
  message("Showing overview. Use $filter_T_hc_T etc. to display document id vectors.")
  print(x$overview)
}

#undebug(validate_filter)

