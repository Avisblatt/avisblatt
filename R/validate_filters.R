validate_filter <- function(corp, filteredcorp,
                                search_col, pattern){
  filter_ids <- names(filteredcorp)
  doc_ids <- names(corp)
  human_class_ids <- doc_ids[grepl(pattern, docvars(corp, field = search_col))]
  filter_T_hc_T <- doc_ids[(doc_ids %in% filter_ids) &
                             (doc_ids %in% human_class_ids)]
  filter_T_hc_F <- doc_ids[(doc_ids %in% filter_ids) &
                             !(doc_ids %in% human_class_ids)]
  hc_T_filter_F <- doc_ids[!(doc_ids %in% filter_ids) &
                             (doc_ids %in% human_class_ids)]
  hc_F_filter_F <- doc_ids[!(doc_ids %in% filter_ids) &
                             !(doc_ids %in% human_class_ids)]

  # Confusion Matrix ----
  overview <- tibble(
    filter_T = c(length(filter_T_hc_T),
                 length(filter_T_hc_F)),
    filter_F = c(length(hc_T_filter_F),
                 length(hc_F_filter_F))
  )

  # Relative Range and Precision
  filter_range <- round(100 / (1+ length(hc_T_filter_F)/length(filter_T_hc_T)), 1)

  filter_precision <- round(100 / (1 + length(filter_T_hc_F) /
                                     length(filter_T_hc_T)), 1)



  # Compose Output ----
  output <- list()
  output$filter_T_hc_T <- filter_T_hc_T
  output$filter_T_hc_F <- filter_T_hc_F
  output$hc_T_filter_F <- hc_T_filter_F
  output$hc_F_filter_F <- hc_F_filter_F
  output$overview <- overview
  output$range <- filter_range
  output$precision <- filter_precision
  output$filter_ids <- filter_ids
  output

  class(output) <- append("avis_confusion_matrix",
                          class(output))
  output
}

print.avis_confusion_matrix <- function(x){
  message(sprintf("Range: The filter recognized %.1f percent of the pertinent ads recognized by human classifcation.\n", x$range))
  message(
    sprintf("Precision: %.1f percent of the ads recognized by the filter were also recognized by human classifcation.\n",
            x$precision)
  )
  message("Showing overview. Use $filter_T_hc_T etc. to display document id vectors.\n")
  print(x$overview)
}

