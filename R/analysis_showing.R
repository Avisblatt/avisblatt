#' @export
show_records <- function(ids = NULL, coll = c_all, show_date = TRUE, show_text = TRUE, show_id = TRUE, show_tags = FALSE, show_header = FALSE, show_edit = FALSE, show_position = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")}
  invalid_ids <- setdiff(ids, coll$meta$id)
  ids <- setdiff(ids, invalid_ids)
  if (show_date){p_date <- paste0("[", coll$corpus[ids]$date, "] ")}
    else {p_date <-""}
  if (show_text){p_text <- as.vector(as.character(coll$corpus)[ids])}
    else {p_text <-""}
  if (show_id){p_id <- paste0(" (", names(coll$corpus[ids]), ")")}
    else {p_id <-""}
  if (show_tags){p_tags <- paste0("\n Tags: ", coll$meta$tags[coll$meta$id %in% ids])}
    else {p_tags <-""}
  if (show_header){p_header <- paste0("\n Header: ", coll$meta$tags_section[coll$meta$id %in% ids])}
    else {p_header <-""}
  if (show_edit){p_edit <- paste0("\nbrowseURL('https://avisblatt.freizo.org/iiif/anno/", names(coll$corpus[ids]), "/edit')")}
    else {p_edit <-""}
  if (show_position){p_pos <- paste0(" issue ", coll$corpus[ids]$issue, ", page ", coll$corpus[ids]$pageno, ", readingorder ", coll$corpus[ids]$readingorder)}
    else {p_pos <-""}
  output <- paste0(p_date, p_text, p_id, p_tags, p_header, p_edit, p_pos, "\n\n")
  cat(output)
  if (length(invalid_ids)>0){
    message("Records with the following IDs could not be shown, as they were not found in the collection: \n")
    cat(invalid_ids, sep = "\n")
  }
}


#' @export
write_records = function(ids = NULL, coll = c_all, show_date = TRUE, show_text = TRUE, show_id = TRUE, show_tags = TRUE, show_header = TRUE, show_edit = TRUE, fn = "../output.tsv"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    if (show_date){p_date <- paste0("[", coll$corpus[ids]$date, "] ")}
    else {p_date <-""}
    if (show_text){p_text <- as.vector(as.character(coll$corpus)[ids])}
    else {p_text <-""}
    if (show_id){p_id <- paste0(" (", names(coll$corpus[ids]), ")")}
    else {p_id <-""}
    if (show_tags){p_tags <- coll$meta$tags[coll$meta$id %in% ids]}
    else {p_tags <-""}
    if (show_header){p_header <- coll$meta$tags_section[coll$meta$id %in% ids]}
    else {p_header <-""}
    if (show_edit){p_edit <- paste0("https://avisblatt.freizo.org/iiif/anno/", names(coll$corpus[ids]), "/edit")}
    else {p_edit <-""}
    output <- cbind(p_date, p_text, p_id, p_tags, p_header, p_edit)
    if (show_date){output <- output[order(p_date),]}
    output <- as.data.table(output)
    names(output)[1] <- paste0('\xEF\xBB\xBF', names(output)[1]) #add BOM, then easier to open in Excel
    fwrite(output, fn, sep="\t")
  }
}


#' @import magick
#' @export
show_iiif <- function(ids = NULL, coll = c_all, show_record = TRUE, max.plot = 10){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to display iiifs.")
  }
  ids <- intersect(ids, names(coll$corpus))
  if(length(ids)>max.plot){
    stop(sprintf("More than %d IDs provided. This function produces a plot of iiif(s) for each record, per default it handles no more than ten. If you want to plot more, set max.plot to a higher value.", max.plot))
  } 
  if(length(ids)==0){
    stop("Could not find any record with the specified ID(s) in this collection.")
  }
  counter <- 0
  for(r in 1:length(ids)){
    id <- ids[r]
    if (show_record){
      output <- paste0("[", coll$corpus[id]$date, "] ",
                       as.character(coll$corpus)[id],
                       "\n Tags: ", coll$meta$tags[coll$meta$id == id],
                       "\n Header: ", coll$meta$tags_section[coll$meta$id == id],
                       "\n")
      cat(output)
    }
    frags_list <- list()
    frags_list[1]  <- coll$corpus[id]$fragment1
    frags_list[2]  <- coll$corpus[id]$fragment2
    frags_list[3]  <- coll$corpus[id]$fragment3
    frags_list[4]  <- coll$corpus[id]$fragment4
    frags_list[5]  <- coll$corpus[id]$fragment5
    frags_list[6]  <- coll$corpus[id]$fragment6
    frags_list[7]  <- coll$corpus[id]$fragment7
    frags_list[8]  <- coll$corpus[id]$fragment8
    frags_list[9]  <- coll$corpus[id]$fragment9
    frags_list[10] <- coll$corpus[id]$fragment10
    # if a fragment value does not contain iiif, it's not a iiif link, 
    # so set frag to NA for easier handling it below:
    frags_list[!grepl("iiif", frags_list, fixed = TRUE)] <- NA
    n <- 10-sum(is.na(frags_list))
    # If it's all NA, abort:
    if(n == 0){
      message(sprintf("For record %s, no iiif are specified.", id))
    } else {
      imgs <- magick::image_read(frags_list[[1]])
      for(i in 2:10){
        if(!is.na(frags_list[[i]])){
          imgs <- c(imgs, magick::image_read(frags_list[[i]]))
        }
      }
      if(n>2){
        combined <- magick::image_append(imgs, stack=F)
      } else {
        combined <- magick::image_append(imgs, stack=T)
        }
      plot(combined)
      counter <- counter+1
    }
  }
  if(counter == 1){
    message("\n1 plot created (might take a moment to load)")
  } else {
    message(sprintf("\n%d plot(s) created  (might take a moment to load)", counter))
    }
}


#' @export
show_tags <- function(ids, coll = c_all, manual = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1)
  {if(ids=="all"){ids <- coll$meta$id}}
  if(manual){
    coll$meta$tags_manual[coll$meta$id %in% ids] %>% unlist %>% unique %>% sort
  } else {
    coll$meta$tags[coll$meta$id %in% ids]  %>% unlist %>% unique %>% sort
  }
}


#' @export
show_headers <- function(coll = c_all){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  unique(unlist(coll$meta$tags_section))
}